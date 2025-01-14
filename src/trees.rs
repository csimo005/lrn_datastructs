use std::cell::RefCell;
use std::collections::VecDeque;
use std::error::Error;
use std::fs::File;
use std::fmt;
use std::rc::{Rc, Weak};

use dot_writer::{Attributes, DotWriter, Rank, RankDirection, Style};

#[derive(Debug)]
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

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key && self.value == other.value
    }
}

impl Eq for Node {}

#[derive(Debug)]
pub struct BinaryTree {
    root: Option<Rc<RefCell<Node>>>, // Box may have been sufficient, but this makes recurrent calls more ergonomic
}

impl BinaryTree {
    pub fn new() -> Self {
        Self { root: None }
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
                self.root = Some(Rc::clone(&new_node));
                return Ok(new_node);
            }
        };
    }

    pub fn remove(&mut self, node: &Rc<RefCell<Node>>) {
        let mut n = node.borrow_mut();
        match (&n.lhs, &n.rhs) {
            (Some(lhs), Some(rhs)) => todo!(),
            (Some(lhs), None) => todo!(),
            (None, Some(rhs)) => {
                match &n.parent {
                    Some(wp) => {
                        if let Some(parent) = wp.upgrade() {
                            let mut parent = parent.borrow_mut();
                            match &parent.lhs {
                                Some(lhs) => {
                                    if Rc::ptr_eq(lhs, node) {
                                        let _ = parent.lhs.replace(Rc::clone(rhs));
                                    } else {
                                        let _ = parent.rhs.replace(Rc::clone(rhs));
                                    }
                                }
                                None => {
                                    let _ = parent.rhs.replace(Rc::clone(rhs));
                                }
                            }
                        }
                    },
                    None => {
                        self.root.replace(Rc::clone(rhs));
                    }
                }
            },
            (None, None) => {
                match &n.parent {
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
                    None => {
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
                        let mut n = digraph.node_named(cb.value.clone());
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
            struct_compare(&t, &nodes[0..=i]);
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
            struct_compare(&t, &nodes[0..=i]);
        }
        
        let mut t = BinaryTree::new();
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            None,
            None,
            None,
            Some(Node::new(5, "qux".to_string())),
            None,
        ];

        for i in 0..nodes.len() {
            if let Some(n) = &nodes[i] {
                let _ = t.insert(n.key, n.value.clone())?;
            }
            struct_compare(&t, &nodes[0..=i]);
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
                                        next = move_up(Rc::clone(&curr))?;
                                        ind = (ind - 1) / 2;
                                    }
                                },
                                (Some(lhs), None) => {
                                    if visited[2 * ind + 1] { // If child is visited, step up
                                        visited[ind] = true;
                                        next = move_up(Rc::clone(&curr))?;
                                        ind = (ind - 1) / 2;
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
                                        next = move_up(Rc::clone(&curr))?;
                                        ind = (ind - 1) / 2;
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
                                    next = move_up(Rc::clone(&curr))?;
                                    ind = (ind - 1) / 2;
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

    fn move_up(rn: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
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
}
