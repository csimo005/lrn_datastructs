use std::cell::RefCell;
use std::collections::VecDeque;
use std::error::Error;
use std::fs::File;
use std::fmt;
use std::rc::{Rc, Weak};

use dot_writer::{Attributes, DotWriter, Rank, RankDirection, Style};

pub struct Node {
    key: i32,
    value: String,
    parent: Option<Weak<RefCell<Node>>>,
    lhs: Option<Rc<RefCell<Node>>>,
    rhs: Option<Rc<RefCell<Node>>>,
}

impl Node {
    fn new(key: i32, value: String) -> Self {
        Self {
            key,
            value,
            parent: None,
            lhs: None,
            rhs: None,
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Node({}: {})", self.key, self.value)
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lhs_str = match &self.lhs {
            Some(lhs) => lhs.borrow().key.to_string(),
            None => "None".to_string(),
        };
        let rhs_str = match &self.rhs {
            Some(rhs) => rhs.borrow().key.to_string(),
            None => "None".to_string(),
        };
        let parent_str = match &self.parent {
            Some(wp) => {
                if let Some(p) = wp.upgrade() {
                    p.borrow().key.to_string()
                } else {
                    "Broken".to_string()
                }
            },
            None => "None".to_string(),
        };
        write!(f, "Node({}: {}, lhs: {}, rhs: {}, parent: {})", self.key, self.value, lhs_str, rhs_str, parent_str)
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key && self.value == other.value
    }
}

impl Eq for Node {}

pub struct BinaryTree {
    root: Option<Rc<RefCell<Node>>>, // Box may have been sufficient, but this makes recurrent calls more ergonomic
    node_list: Vec<Rc<RefCell<Node>>>,
}

impl BinaryTree {
    pub fn new() -> Self {
        Self { root: None, node_list: Vec::<Rc<RefCell<Node>>>::new()}
    }

    pub fn insert(&mut self, key: i32, value: String) -> Result<Rc<RefCell<Node>>, &'static str> {
        // It's probably bad practice to directly expose data like this, I should probably refactor....
        match &self.root {
            Some(curr) => {
                let mut curr = Rc::clone(&curr);
                loop {
                    let next: Rc<RefCell<Node>>;
                    let mut cn = curr.borrow_mut();
                    if key < cn.key {
                        match &cn.lhs {
                            Some(node) => next = Rc::clone(&node),
                            None => {
                                let new_node = Rc::new(RefCell::new(Node::new(key, value)));
                                self.node_list.push(Rc::clone(&new_node));
                                new_node.borrow_mut().parent = Some(Rc::downgrade(&curr));
                                cn.lhs = Some(Rc::clone(&new_node));
                                return Ok(new_node);
                            }
                        };
                    } else if key > cn.key {
                        match &cn.rhs {
                            Some(node) => next = Rc::clone(&node),
                            None => {
                                let new_node = Rc::new(RefCell::new(Node::new(key, value)));
                                self.node_list.push(Rc::clone(&new_node));
                                new_node.borrow_mut().parent = Some(Rc::downgrade(&curr));
                                cn.rhs = Some(Rc::clone(&new_node));
                                return Ok(new_node);
                            }
                        };
                    } else {
                        //keys are equal, so key is already present
                        return Err("Key already present in tree");
                    }
                    drop(cn);
                    curr = next;
                }
            }
            None => {
                let new_node = Rc::new(RefCell::new(Node::new(key, value)));
                self.node_list.push(Rc::clone(&new_node));
                self.root = Some(Rc::clone(&new_node));
                return Ok(new_node);
            }
        };
    }

    pub fn remove(&mut self, node: &Rc<RefCell<Node>>) {
        let node_parent: Option<Weak<RefCell<Node>>>;
        let node_lhs: Option<Rc<RefCell<Node>>>;
        let node_rhs: Option<Rc<RefCell<Node>>>;
        { // We get lhs, rhs, and parent here to make sure this borrow goes out of scope before the next match
            let n = node.borrow();
            node_parent = n.parent.as_ref().map(|parent| Weak::clone(parent));
            node_lhs = n.lhs.as_ref().map(|child| Rc::clone(child));
            node_rhs = n.rhs.as_ref().map(|child| Rc::clone(child));
        }

        match (node_lhs, node_rhs) {
            (Some(_), Some(rhs)) => { 
                let mut successor: Rc<RefCell<Node>> = Rc::clone(&rhs);
                loop {
                    let next: Rc<RefCell<Node>>;
                    let cn = successor.borrow();
                    match &cn.lhs {
                        Some(node) => next = Rc::clone(&node),
                        None => break,
                    };

                    drop(cn);
                    successor = next;
                }

                let mut mut_successor = successor.borrow_mut();
                match &mut_successor.parent {
                    None => panic!("Successor will never be root, but it is missing its parent"),
                    Some(wp) => {
                        match wp.upgrade() {
                            None => panic!("Successor weak refference to parent is broken"),
                            Some(parent) => {
                                let mut parent = parent.borrow_mut();
                                match &parent.rhs {
                                    Some(rhs) => {
                                        if Rc::ptr_eq(rhs, &successor) {
                                            match &mut_successor.rhs {
                                                Some(rhs) => {
                                                    parent.rhs = Some(Rc::clone(&rhs));
                                                    rhs.borrow_mut().parent = Some(Weak::clone(&wp));
                                                },
                                                None => parent.lhs = None,
                                            }
                                        } else {
                                            match &mut_successor.rhs {
                                                Some(rhs) => {
                                                    parent.lhs = Some(Rc::clone(&rhs));
                                                    rhs.borrow_mut().parent = Some(Weak::clone(&wp));
                                                },
                                                None => parent.lhs = None,
                                            }
                                        }
                                    }
                                    None => {
                                        match &mut_successor.rhs {
                                            Some(rhs) => {
                                                parent.lhs = Some(Rc::clone(&rhs));
                                                rhs.borrow_mut().parent = Some(Weak::clone(&wp));
                                            },
                                            None => parent.lhs = None,
                                        }
                                    }
                                }
                            }
                        }
                    },
                };

                //Reborrow in case the previous block modified node
                //let node_parent: Option<Weak<RefCell<Node>>>;
                //let node_lhs: Option<Rc<RefCell<Node>>>;
                //let node_rhs: Option<Rc<RefCell<Node>>>;
                //{ // We get lhs, rhs, and parent here to make sure this borrow goes out of scope before the next match
                //    let n = node.borrow();
                //    node_parent = n.parent.as_ref().map(|parent| Weak::clone(parent));
                //    node_lhs = n.lhs.as_ref().map(|child| Rc::clone(child));
                //    node_rhs = n.rhs.as_ref().map(|child| Rc::clone(child));
                //}
                let n = node.borrow();
                match &n.lhs {
                    Some(lhs) => {
                        mut_successor.lhs = Some(Rc::clone(&lhs));
                        lhs.borrow_mut().parent = Some(Rc::downgrade(&successor));
                    },
                    None => panic!(),
                };
                match &n.rhs {
                    Some(rhs) => {
                        mut_successor.rhs = Some(Rc::clone(&rhs));
                        rhs.borrow_mut().parent = Some(Rc::downgrade(&successor));
                    },
                    None => panic!(),
                };
                match &n.parent {
                    Some(wp) => {
                        mut_successor.parent = Some(Weak::clone(&wp));
                        if let Some(parent) = wp.upgrade() {
                            let mut parent = parent.borrow_mut();
                            match &parent.rhs {
                                Some(rhs) => {
                                    if Rc::ptr_eq(rhs, node) {
                                        parent.rhs = Some(Rc::clone(&successor));
                                    } else {
                                        parent.lhs = Some(Rc::clone(&successor));
                                    }
                                }
                                None => {
                                    parent.lhs = Some(Rc::clone(&successor));
                                }
                            }
                        }
                        
                    },
                    None => {
                        mut_successor.parent = None;
                        self.root = Some(Rc::clone(&successor));
                    },
                };
            },
            (Some(lhs), None) => { 
                match node_parent {
                    Some(wp) => {
                        let mut mut_lhs = lhs.borrow_mut();
                        mut_lhs.parent = Some(Weak::clone(&wp));

                        if let Some(parent) = wp.upgrade() {
                            let mut parent = parent.borrow_mut();
                            match &parent.rhs {
                                Some(rhs) => {
                                    if Rc::ptr_eq(rhs, node) {
                                        let _ = parent.rhs.replace(Rc::clone(&lhs));
                                    } else {
                                        let _ = parent.lhs.replace(Rc::clone(&lhs));
                                    }
                                }
                                None => {
                                    let _ = parent.lhs.replace(Rc::clone(&lhs));
                                }
                            }
                        }
                    },
                    None => {
                        self.root.replace(Rc::clone(&lhs));
                        let mut mut_lhs = lhs.borrow_mut();
                        mut_lhs.parent = None;
                    }
                }
            },
            (None, Some(rhs)) => {
                match node_parent {
                    Some(wp) => {
                        let mut mut_rhs = rhs.borrow_mut();
                        mut_rhs.parent = Some(Weak::clone(&wp));

                        if let Some(parent) = wp.upgrade() {
                            let mut parent = parent.borrow_mut();
                            match &parent.lhs {
                                Some(lhs) => {
                                    if Rc::ptr_eq(lhs, node) {
                                        let _ = parent.lhs.replace(Rc::clone(&rhs));
                                    } else {
                                        let _ = parent.rhs.replace(Rc::clone(&rhs));
                                    }
                                }
                                None => {
                                    let _ = parent.rhs.replace(Rc::clone(&rhs));
                                }
                            }
                        }
                    },
                    None => { // If node doesn't have a parent, it must be root
                        self.root.replace(Rc::clone(&rhs));
                        let mut mut_rhs = rhs.borrow_mut();
                        mut_rhs.parent = None;
                    }
                }
            },
            (None, None) => {
                match node_parent {
                    Some(wp) => {
                        if let Some(parent) = wp.upgrade() {
                            let mut parent = parent.borrow_mut();
                            match &parent.lhs {
                                Some(lhs) => {
                                    if Rc::ptr_eq(lhs, node) {
                                        parent.lhs = None;
                                    } else {
                                        parent.rhs = None;
                                    }
                                }
                                None => {
                                    parent.rhs = None;
                                }
                            }
                        }
                    },
                    None => { // If node doesn't have a parent, it must be root
                        self.root = None;
                    }
                }
            },
        };
    }

    pub fn write_graphviz(&self, fname: &str) -> Result<(), Box<dyn Error>> {
        let mut f = File::create(fname)?;
        {
            let mut writer = DotWriter::from(&mut f);
            writer.set_pretty_print(true);

            let mut digraph = writer.digraph();

            if let Some(r) = &self.root {
                let mut q: VecDeque<_> = VecDeque::<Rc<RefCell<Node>>>::new();
                q.push_front(Rc::clone(&r));

                while let Some(curr) = q.pop_back() {
                    let cb = curr.borrow();
                    {
                        let _n = digraph.node_named(cb.value.clone());
                    }

                    if let (Some(lhs), Some(rhs)) = (&cb.lhs, &cb.rhs) {
                        let mut sg = digraph.subgraph();
                        sg.set_rank_direction(RankDirection::LeftRight);
                        sg.set_rank(Rank::Same);
                        sg.edge(lhs.borrow().value.clone(), rhs.borrow().value.clone())
                            .attributes()
                            .set_style(Style::Invisible);
                    }

                    if let Some(wp) = &cb.parent {
                        if let Some(sp) = wp.upgrade() {
                            let sp = sp.borrow();
                            digraph.edge(sp.value.clone(), cb.value.clone());
                        }
                    }

                    if let Some(lhs) = &cb.lhs {
                        q.push_back(Rc::clone(lhs));
                    }

                    if let Some(rhs) = &cb.rhs {
                        q.push_back(Rc::clone(rhs));
                    }
                }
            }
        }
        Ok(())
    }
}

impl fmt::Debug for BinaryTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Node List:\n")?;
        for n in &self.node_list {
            write!(f, "\t{:?}, Count: {}\n", n.borrow(), Rc::strong_count(n)-1)?;
        }

        Ok(())
    }

}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn node_initializer() {
        let _n = Node::new(0, "".to_string());
    }

    #[test]
    fn tree_initializer() {
        let _t = BinaryTree::new();
    }

    #[test]
    fn tree_repeat_insert() {
        let mut t = BinaryTree::new();
        match t.insert(3, "foo".to_string()) {
            Ok(_) => (),
            Err(_) => panic!("Failed to insert Node[3] = \"foo\""),
        }

        match t.insert(3, "foo".to_string()) {
            Ok(_) => panic!("Repeated insert of Node[3] = \"foo\" should Err"),
            Err(_) => (),
        }
        
        match t.insert(4, "bar".to_string()) {
            Ok(_) => (),
            Err(_) => panic!("Failed to insert Node[4] = \"bar\""),
        }

        match t.insert(4, "bar".to_string()) {
            Ok(_) => panic!("Repeated insert of Node[4] = \"bar\" should Err"),
            Err(_) => (),
        }
        
        match t.insert(2, "buz".to_string()) {
            Ok(_) => (),
            Err(_) => panic!("Failed to insert Node[2] = \"buz\""),
        }

        match t.insert(2, "buz".to_string()) {
            Ok(_) => panic!("Repeated insert of Node[2] = \"buz\" should Err"),
            Err(_) => (),
        }
    }

    #[test]
    fn tree_insert() -> Result<(), String> {
        let mut t = BinaryTree::new();
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];

        for i in 0..nodes.len() {
            if let Some(n) = &nodes[i] {
                let _ = t.insert(n.key, n.value.clone())?;
            }
            struct_compare(&t, &nodes[0..=i])?;
        }
        
        let mut t = BinaryTree::new();
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            None,
            None,
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];

        for i in 0..nodes.len() {
            if let Some(n) = &nodes[i] {
                let _ = t.insert(n.key, n.value.clone())?;
            }
            struct_compare(&t, &nodes[0..=i])?;
        }
        
        let mut t = BinaryTree::new();
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            None,
            None,
            Some(Node::new(3, "qux".to_string())),
            None,
            None,
        ];

        for i in 0..nodes.len() {
            if let Some(n) = &nodes[i] {
                let _ = t.insert(n.key, n.value.clone())?;
            }
            struct_compare(&t, &nodes[0..=i])?;
        }


        

        let _ = t.write_graphviz("out/insertion_test.dot");
        Ok(())
    }
    
    #[test]
    fn tree_remove_leaves() {
        let mut t = BinaryTree::new();

        if let Ok(n) = t.insert(3, "foo".to_string()) {
            t.remove(&n);
            println!("{t:?}");
        }

        let r = t.insert(3, "foo".to_string()).unwrap();
        let lhs = t.insert(2, "bar".to_string()).unwrap();
        let rhs = t.insert(4, "buz".to_string()).unwrap();
            
        println!("{t:?}");
        t.remove(&lhs);
        println!("{t:?}");
        t.remove(&rhs);
        println!("{t:?}");
        t.remove(&r);
        println!("{t:?}");
    }
    
    #[test]
    fn tree_remove_internal() -> Result<(), String> {
        let mut t = BinaryTree::new();
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];

        let mut node_refs = Vec::<Option<Rc<RefCell<Node>>>>::new();
        for i in 0..nodes.len() {
            if let Some(n) = &nodes[i] {
                match t.insert(n.key, n.value.clone()) {
                    Ok(ret) => node_refs.push(Some(ret)),
                    Err(e) => return Err(e.to_string()),
                };
            }
        }
        println!("Full Tree");
        println!("{t:?}\n");

        if let Some(n) = &node_refs[3] {
            t.remove(n);
        }
        node_refs[3] = None;
        if let Some(n) = &node_refs[1] {
            t.remove(n);
        }
        node_refs[1] = None;
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(6, "buz".to_string())),
            None,
            None,
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];
        println!("Remove 1, Remove 2");
        println!("{t:?}\n");
        struct_compare(&t, &nodes)?;
       
        if let Some(n) = &node_refs[6] {
            t.remove(n);
        }
        node_refs[6] = None;
        if let Some(n) = &node_refs[2] {
            t.remove(n);
        }
        node_refs[2] = None;
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            None,
            None,
            None,
            None,
        ];
        println!("Remove 7, Remove 6");
        println!("{t:?}\n");
        struct_compare(&t, &nodes)?;
        
        let mut t = BinaryTree::new();
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];

        let mut node_refs = Vec::<Option<Rc<RefCell<Node>>>>::new();
        for i in 0..nodes.len() {
            if let Some(n) = &nodes[i] {
                match t.insert(n.key, n.value.clone()) {
                    Ok(ret) => node_refs.push(Some(ret)),
                    Err(e) => return Err(e.to_string()),
                };
            }
        }
        println!("Full Tree");
        println!("{t:?}\n");

        if let Some(n) = &node_refs[0] {
            t.remove(n);
        }
        node_refs[0] = None;
        let nodes: Vec<_> = vec![
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            None,
            Some(Node::new(7, "qox".to_string())),
        ];
        println!("Remove 4");
        println!("{t:?}\n");
        struct_compare(&t, &nodes)?;
        
        let mut t = BinaryTree::new();
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];
        let mut node_refs = Vec::<Option<Rc<RefCell<Node>>>>::new();
        for i in 0..nodes.len() {
            if let Some(n) = &nodes[i] {
                match t.insert(n.key, n.value.clone()) {
                    Ok(ret) => node_refs.push(Some(ret)),
                    Err(e) => return Err(e.to_string()),
                };
            }
        }
        println!("Full Tree");
        println!("{t:?}\n");

        if let Some(n) = &node_refs[5] {
            t.remove(n);
        }
        node_refs[5] = None;
        if let Some(n) = &node_refs[0] {
            t.remove(n);
        }
        node_refs[0] = None;
        let nodes: Vec<_> = vec![
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(7, "qox".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            None,
            None,
        ];
        println!("Remove 5, Remove 4");
        println!("{t:?}\n");
        struct_compare(&t, &nodes)?;
        
        let mut t = BinaryTree::new();
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];
        let mut node_refs = Vec::<Option<Rc<RefCell<Node>>>>::new();
        for i in 0..nodes.len() {
            if let Some(n) = &nodes[i] {
                match t.insert(n.key, n.value.clone()) {
                    Ok(ret) => node_refs.push(Some(ret)),
                    Err(e) => return Err(e.to_string()),
                };
            }
        }
        println!("Full Tree");
        println!("{t:?}\n");

        if let Some(n) = &node_refs[6] {
            t.remove(n);
        }
        node_refs[6] = None;
        if let Some(n) = &node_refs[0] {
            t.remove(n);
        }
        node_refs[0] = None;
        let nodes: Vec<_> = vec![
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            None,
            None,
        ];
        println!("Remove 5, Remove 4");
        println!("{t:?}\n");
        struct_compare(&t, &nodes)?;


        Ok(())
    }

    fn struct_compare(b: &BinaryTree, v: &[Option<Node>]) -> Result<(), String> {
        if v.len() == 0 {
            match b.root {
                Some(_) => return Err("Binary Tree should be Empty".to_string()),
                None => return Ok(()),
            }
        } else {
            match &b.root {
                None => return Err("Binary Tree is Empty".to_string()),
                Some(r) => {
                    let mut visited = vec![false; v.len()];
                    let mut curr = Rc::clone(r);
                    let mut ind = 0;

                    while !visited[0] {
                        let next: Rc<RefCell<Node>>;
                        {
                            let cn = curr.borrow();
    
                            match &v[ind] {
                                Some(n) => {
                                    if *cn !=  *n {
                                        return Err(format!("Actual: {}, Expected: {}", *cn, *n));
                                    }

                                },
                                None => return Err("Node present in tree missing from Vec".to_string()),
                            }
                               
                            match (&cn.lhs, &cn.rhs) {
                                (Some(lhs), Some(rhs)) => {
                                    if !visited[2 * ind + 1] { // Visit left child first
                                        match v[2 * ind + 1] {
                                            Some(_) => {
                                                ind = 2 * ind + 1;
                                                next = Rc::clone(lhs);
                                            },
                                            None => return Err(format!("Unexpected node left of {}", *cn)),
                                        };
                                    } else if !visited[2 * ind + 2] { // Visit right child next
                                        match v[2 * ind + 2] {
                                            Some(_) => {
                                                ind = 2 * ind + 2;
                                                next = Rc::clone(rhs);
                                            },
                                            None => return Err(format!("Unexpected node right of {}", *cn)),
                                        };
                                    } else { // Move up
                                        visited[ind] = true;
                                        match move_up_ind(ind) {
                                            Ok(i) => {
                                                ind = i;
                                                next = move_up_node(Rc::clone(&curr))?;
                                            },
                                            Err(_) => next = Rc::clone(&curr),
                                        };
                                    }
                                },
                                (Some(lhs), None) => {
                                    if visited[2 * ind + 1] { // If child is visited, step up
                                        visited[ind] = true;
                                        match move_up_ind(ind) {
                                            Ok(i) => {
                                                ind = i;
                                                next = move_up_node(Rc::clone(&curr))?;
                                            },
                                            Err(_) => next = Rc::clone(&curr),
                                        };
                                    } else { // Other wise step into child
                                        match v[2 * ind + 1] {
                                            Some(_) => {
                                                ind = 2 * ind + 1;
                                                next = Rc::clone(lhs);
                                            },
                                            None => return Err(format!("Unexpected node left of {}", *cn)),
                                        };
                                    }
                                },
                                (None, Some(rhs)) => {
                                    println!("{:?}", *cn);
                                    if visited[2 * ind + 2] { // If child is visited, step up
                                        visited[ind] = true;
                                        match move_up_ind(ind) {
                                            Ok(i) => {
                                                ind = i;
                                                next = move_up_node(Rc::clone(&curr))?;
                                            },
                                            Err(_) => next = Rc::clone(&curr),
                                        };
                                    } else { // Other wise step into child
                                        match v[2 * ind + 2] {
                                            Some(_) => {
                                                ind = 2 * ind + 2;
                                                next = Rc::clone(rhs);
                                            },
                                            None => return Err(format!("Unexpected node right of {}", *cn)),
                                        };
                                    }
                                },
                                (None, None) => { // if leaf node mark as visited and move up
                                    visited[ind] = true;
                                    match move_up_ind(ind) {
                                        Ok(i) => {
                                            ind = i;
                                            next = move_up_node(Rc::clone(&curr))?;
                                        },
                                        Err(_) => next = Rc::clone(&curr),
                                    };
                                }
                            }
                        }
                        curr = next;
                    }
                }
            }
        }
        Ok(())
    }

    fn move_up_node(rn: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
        let n = rn.borrow();
        match &n.parent {
            Some(wp) => {
                match wp.upgrade() {
                    None => Err(format!("{} parent missing", n)),
                    Some(sp) => Ok(Rc::clone(&sp)),
                }
            },
            None => Err(format!("{} has no parent", n)),
        }
    }
    
    fn move_up_ind(ind: usize) -> Result<usize, String> {
        match ind {
            0 => Err("Root node cannot move up".to_string()),
            _ => Ok((ind - 1) / 2),
        }
    }
}
