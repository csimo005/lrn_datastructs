use std::cell::RefCell;
use std::collections::VecDeque;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::rc::{Rc, Weak};

use dot_writer::{Attributes, DotWriter, Rank, RankDirection, Style};

use crate::trees::{BSTError, BinarySearchTree};

struct Node {
    key: i32,
    value: String,
    height: usize,
    parent: Option<Weak<RefCell<Node>>>,
    lhs: Option<Rc<RefCell<Node>>>,
    rhs: Option<Rc<RefCell<Node>>>,
}

impl Node {
    fn new(key: i32, value: String) -> Self {
        Self {
            key,
            value,
            height: 0,
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
            }
            None => "None".to_string(),
        };
        write!(
            f,
            "Node({}: {}, height: {}, lhs: {}, rhs: {}, parent: {})",
            self.key, self.value, self.height, lhs_str, rhs_str, parent_str
        )
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key && self.value == other.value
    }
}

impl Eq for Node {}

pub struct AVLTree {
    root: Option<Rc<RefCell<Node>>>, // Box may have been sufficient, but this makes recurrent calls more ergonomic
}

impl AVLTree {
    pub fn new() -> Self {
        Self { root: None }
    }

    fn remove_node(&mut self, node: Rc<RefCell<Node>>) -> Result<(), BSTError> {
        let node_parent: Option<Weak<RefCell<Node>>>;
        let node_lhs: Option<Rc<RefCell<Node>>>;
        let node_rhs: Option<Rc<RefCell<Node>>>;
        {
            // We get lhs, rhs, and parent here to make sure this borrow goes out of scope before the next match
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
                    Some(wp) => match wp.upgrade() {
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
                                            }
                                            None => parent.lhs = None,
                                        }
                                    } else {
                                        match &mut_successor.rhs {
                                            Some(rhs) => {
                                                parent.lhs = Some(Rc::clone(&rhs));
                                                rhs.borrow_mut().parent = Some(Weak::clone(&wp));
                                            }
                                            None => parent.lhs = None,
                                        }
                                    }
                                }
                                None => match &mut_successor.rhs {
                                    Some(rhs) => {
                                        parent.lhs = Some(Rc::clone(&rhs));
                                        rhs.borrow_mut().parent = Some(Weak::clone(&wp));
                                    }
                                    None => parent.lhs = None,
                                },
                            }
                        }
                    },
                };

                //Reborrow in case the previous block modified node
                let n = node.borrow();
                match &n.lhs {
                    Some(lhs) => {
                        mut_successor.lhs = Some(Rc::clone(&lhs));
                        lhs.borrow_mut().parent = Some(Rc::downgrade(&successor));
                    }
                    None => panic!(),
                };
                match &n.rhs {
                    Some(rhs) => {
                        mut_successor.rhs = Some(Rc::clone(&rhs));
                        rhs.borrow_mut().parent = Some(Rc::downgrade(&successor));
                    }
                    None => panic!(),
                };
                match &n.parent {
                    Some(wp) => {
                        mut_successor.parent = Some(Weak::clone(&wp));
                        if let Some(parent) = wp.upgrade() {
                            let mut parent = parent.borrow_mut();
                            match &parent.rhs {
                                Some(rhs) => {
                                    if Rc::ptr_eq(&rhs, &node) {
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
                    }
                    None => {
                        mut_successor.parent = None;
                        self.root = Some(Rc::clone(&successor));
                    }
                };
            }
            (Some(lhs), None) => match node_parent {
                Some(wp) => {
                    let mut mut_lhs = lhs.borrow_mut();
                    mut_lhs.parent = Some(Weak::clone(&wp));

                    if let Some(parent) = wp.upgrade() {
                        let mut parent = parent.borrow_mut();
                        match &parent.rhs {
                            Some(rhs) => {
                                if Rc::ptr_eq(&rhs, &node) {
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
                }
                None => {
                    self.root.replace(Rc::clone(&lhs));
                    let mut mut_lhs = lhs.borrow_mut();
                    mut_lhs.parent = None;
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
                                    if Rc::ptr_eq(&lhs, &node) {
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
                    }
                    None => {
                        // If node doesn't have a parent, it must be root
                        self.root.replace(Rc::clone(&rhs));
                        let mut mut_rhs = rhs.borrow_mut();
                        mut_rhs.parent = None;
                    }
                }
            }
            (None, None) => {
                match node_parent {
                    Some(wp) => {
                        if let Some(parent) = wp.upgrade() {
                            let mut parent = parent.borrow_mut();
                            match &parent.lhs {
                                Some(lhs) => {
                                    if Rc::ptr_eq(&lhs, &node) {
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
                    }
                    None => {
                        // If node doesn't have a parent, it must be root
                        self.root = None;
                    }
                }
            }
        };

        Ok(())
    }

    fn get_node(&self, key: i32) -> Option<Rc<RefCell<Node>>> {
        match &self.root {
            Some(curr) => {
                let mut curr = Rc::clone(&curr);
                loop {
                    let next: Rc<RefCell<Node>>;
                    let cn = curr.borrow();
                    if key < cn.key {
                        match &cn.lhs {
                            Some(node) => next = Rc::clone(&node),
                            None => {
                                return None;
                            }
                        };
                    } else if key > cn.key {
                        match &cn.rhs {
                            Some(node) => next = Rc::clone(&node),
                            None => {
                                return None;
                            }
                        };
                    } else {
                        //keys are equal, so key is already present
                        return Some(Rc::clone(&curr));
                    }
                    drop(cn);
                    curr = next;
                }
            }
            None => None,
        }
    }

    fn rotate_left(&mut self, x: Rc<RefCell<Node>>) {
        let mut x_mut = x.borrow_mut();

        let z = match &x_mut.rhs {
            Some(rhs) => Rc::clone(&rhs),
            None => panic!("Tried to rotate ineliglibe subtree"),
        };
        let mut z_mut = z.borrow_mut();

        let t23 = z_mut.lhs.as_ref().map(|child| Rc::clone(&child));
        let x_parent = x_mut.parent.as_ref().map(|par| Weak::clone(&par)); 

        x_mut.rhs = t23;
        match &x_mut.rhs {
            Some(node) => {
                let mut node_mut = node.borrow_mut();
                node_mut.parent = Some(Rc::downgrade(&x));
            },
            None => (),
        }

        z_mut.lhs = Some(Rc::clone(&x));
        x_mut.parent = Some(Rc::downgrade(&z));

        match x_parent {
            Some(wp) => {
                if let Some(parent) = wp.upgrade() {
                    let mut parent = parent.borrow_mut();
                    match &parent.lhs {
                        Some(lhs) => {
                            if Rc::ptr_eq(&lhs, &x) {
                                parent.lhs = Some(Rc::clone(&z));
                            } else {
                                parent.rhs = Some(Rc::clone(&z));
                            }
                        }
                        None => {
                            parent.rhs = Some(Rc::clone(&z));
                        }
                    }
                }
                z_mut.parent = Some(Weak::clone(&wp));
            },
            None => {
                self.root = Some(Rc::clone(&z));
                z_mut.parent = None;
            }
        };
    }

    fn rotate_right(&mut self, x: Rc<RefCell<Node>>) {
        let mut x_mut = x.borrow_mut();

        let z = match &x_mut.lhs {
            Some(lhs) => Rc::clone(&lhs),
            None => panic!("Tried to rotate ineliglibe subtree"),
        };
        let mut z_mut = z.borrow_mut();

        let t23 = z_mut.rhs.as_ref().map(|child| Rc::clone(&child));
        let x_parent = x_mut.parent.as_ref().map(|par| Weak::clone(&par)); 

        x_mut.lhs = t23;
        match &x_mut.lhs {
            Some(node) => {
                let mut node_mut = node.borrow_mut();
                node_mut.parent = Some(Rc::downgrade(&x));
            },
            None => (),
        }

        z_mut.rhs = Some(Rc::clone(&x));
        x_mut.parent = Some(Rc::downgrade(&z));

        match x_parent {
            Some(wp) => {
                if let Some(parent) = wp.upgrade() {
                    let mut parent = parent.borrow_mut();
                    match &parent.lhs {
                        Some(lhs) => {
                            if Rc::ptr_eq(&lhs, &x) {
                                parent.lhs = Some(Rc::clone(&z));
                            } else {
                                parent.rhs = Some(Rc::clone(&z));
                            }
                        }
                        None => {
                            parent.rhs = Some(Rc::clone(&z));
                        }
                    }
                }
                z_mut.parent = Some(Weak::clone(&wp));
            },
            None => {
                self.root = Some(Rc::clone(&z));
                z_mut.parent = None;
            }
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

impl BinarySearchTree for AVLTree {
    fn insert(&mut self, key: i32, value: String) -> Result<(), BSTError> {
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
                                return Ok(());
                            }
                        };
                    } else if key > cn.key {
                        match &cn.rhs {
                            Some(node) => next = Rc::clone(&node),
                            None => {
                                let new_node = Rc::new(RefCell::new(Node::new(key, value)));
                                new_node.borrow_mut().parent = Some(Rc::downgrade(&curr));
                                cn.rhs = Some(Rc::clone(&new_node));
                                return Ok(());
                            }
                        };
                    } else {
                        //keys are equal, so key is already present
                        return Err(BSTError::DoubleInsert(key));
                    }
                    drop(cn);
                    curr = next;
                }
            }
            None => {
                let new_node = Rc::new(RefCell::new(Node::new(key, value)));
                self.root = Some(Rc::clone(&new_node));
                return Ok(());
            }
        };
    }

    fn remove(&mut self, key: i32) -> Result<(), BSTError> {
        match self.get_node(key) {
            Some(n) => self.remove_node(n)?,
            None => return Err(BSTError::KeyNotFound(key)),
        };
        Ok(())
    }

    fn get(&self, key: i32) -> Option<String> {
        match self.get_node(key) {
            Some(n) => {
                let cn = n.borrow();
                return Some(cn.value.clone());
            }
            None => return None,
        };
    }

    fn update(&mut self, key: i32, value: String) -> Result<(), BSTError> {
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
                                return Err(BSTError::KeyNotFound(key));
                            }
                        };
                    } else if key > cn.key {
                        match &cn.rhs {
                            Some(node) => next = Rc::clone(&node),
                            None => {
                                return Err(BSTError::KeyNotFound(key));
                            }
                        };
                    } else {
                        //keys are equal, so key is already present
                        cn.value = value;
                        return Ok(());
                    }
                    drop(cn);
                    curr = next;
                }
            }
            None => return Err(BSTError::KeyNotFound(key)),
        };
    }
}

impl fmt::Debug for AVLTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Node List:\n")?;

        let mut q = VecDeque::<Rc<RefCell<Node>>>::new();
        match &self.root {
            Some(root) => q.push_front(Rc::clone(&root)),
            None => (),
        };

        while let Some(node) = q.pop_back() {
            let n = node.borrow();
            write!(f, "\t{:?}, Count: {}\n", n, Rc::strong_count(&node) - 1)?;
            match &n.lhs {
                Some(lhs) => q.push_front(Rc::clone(&lhs)),
                None => (),
            };
            match &n.rhs {
                Some(rhs) => q.push_front(Rc::clone(&rhs)),
                None => (),
            };
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
        let _t = AVLTree::new();
    }

    #[test]
    fn tree_repeat_insert() -> Result<(), Box<dyn Error>> {
        let nodes: Vec<_> = vec![
            Some(Node::new(08, "GzIvDtMP2L".to_string())),
            Some(Node::new(04, "Kc1HqZlxzZ".to_string())),
            Some(Node::new(12, "Teac1ED0Ff".to_string())),
            Some(Node::new(02, "Z7nQiISAh5".to_string())),
            Some(Node::new(06, "SN14A6P3gI".to_string())),
            Some(Node::new(10, "n9H96yXDul".to_string())),
            Some(Node::new(14, "54iCivisgx".to_string())),
            Some(Node::new(01, "SDlHQ4iOp3".to_string())),
            Some(Node::new(03, "vdNlwloop6".to_string())),
            Some(Node::new(05, "dyoGZqWUtU".to_string())),
            Some(Node::new(07, "wAyWEwsjqT".to_string())),
            Some(Node::new(09, "7XhxmrbC8H".to_string())),
            Some(Node::new(11, "MD39zhwPQd".to_string())),
            Some(Node::new(13, "809g02hlqY".to_string())),
            Some(Node::new(15, "fkgzjTIAVG".to_string())),
        ];

        let mut t = AVLTree::new();
        for (i, node) in nodes.iter().enumerate() {
            match node {
                Some(n) => {
                    t.insert(n.key, n.value.clone())?;
                    match t.insert(n.key, n.value.clone()) {
                        Err(BSTError::DoubleInsert(_)) => (),
                        _ => return Err("Double insert should have returned an error".into()),
                    }
                    struct_compare(&t, &nodes[0..=i])?;
                }
                None => unreachable!(),
            };
        }

        Ok(())
    }

    #[test]
    fn tree_insert() -> Result<(), Box<dyn Error>> {
        let mut t = AVLTree::new();
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
                t.insert(n.key, n.value.clone())?;
            }
            struct_compare(&t, &nodes[0..=i])?;
        }

        let mut t = AVLTree::new();
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
                t.insert(n.key, n.value.clone())?;
            }
            struct_compare(&t, &nodes[0..=i])?;
        }

        let mut t = AVLTree::new();
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
                t.insert(n.key, n.value.clone())?;
            }
            struct_compare(&t, &nodes[0..=i])?;
        }

        let _ = t.write_graphviz("out/insertion_test.dot");
        Ok(())
    }

    #[test]
    fn tree_remove_leaves() -> Result<(), Box<dyn Error>> {
        let mut t = AVLTree::new();

        t.insert(3, "foo".to_string())?;
        t.remove(3)?;

        t.insert(3, "foo".to_string())?;
        t.insert(2, "bar".to_string())?;
        t.insert(4, "buz".to_string())?;

        t.remove(4)?;
        t.remove(2)?;
        t.remove(3)?;

        Ok(())
    }

    #[test]
    fn tree_remove_internal() -> Result<(), Box<dyn Error>> {
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];

        let mut t = AVLTree::new();
        for node in nodes.iter() {
            if let Some(n) = node {
                t.insert(n.key, n.value.clone())?;
            }
        }

        t.remove(1)?;
        t.remove(2)?;
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(6, "buz".to_string())),
            None,
            None,
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];
        struct_compare(&t, &nodes)?;

        t.remove(7)?;
        t.remove(6)?;
        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            None,
            None,
            None,
            None,
        ];
        struct_compare(&t, &nodes)?;

        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];

        let mut t = AVLTree::new();
        for node in nodes.iter() {
            if let Some(n) = node {
                t.insert(n.key, n.value.clone())?;
            }
        }

        t.remove(4)?;
        let nodes: Vec<_> = vec![
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            None,
            Some(Node::new(7, "qox".to_string())),
        ];
        struct_compare(&t, &nodes)?;

        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];

        let mut t = AVLTree::new();
        for node in nodes.iter() {
            if let Some(n) = node {
                t.insert(n.key, n.value.clone())?;
            }
        }

        t.remove(5)?;
        t.remove(4)?;
        let nodes: Vec<_> = vec![
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(7, "qox".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            None,
            None,
        ];
        struct_compare(&t, &nodes)?;

        let nodes: Vec<_> = vec![
            Some(Node::new(4, "foo".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(7, "qox".to_string())),
        ];

        let mut t = AVLTree::new();
        for node in nodes.iter() {
            if let Some(n) = node {
                t.insert(n.key, n.value.clone())?;
            }
        }

        t.remove(7)?;
        t.remove(4)?;

        let nodes: Vec<_> = vec![
            Some(Node::new(5, "qux".to_string())),
            Some(Node::new(2, "bar".to_string())),
            Some(Node::new(6, "buz".to_string())),
            Some(Node::new(1, "qix".to_string())),
            Some(Node::new(3, "qax".to_string())),
            None,
            None,
        ];
        struct_compare(&t, &nodes)?;

        Ok(())
    }

    #[test]
    fn tree_get() -> Result<(), Box<dyn Error>> {
        let mut t = AVLTree::new();
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
                t.insert(n.key, n.value.clone())?;
            }
        }

        for i in 0..nodes.len() {
            if let Some(n) = &nodes[i] {
                let val = t.get(n.key);
                assert_eq!(n.value, val.unwrap());
            }
        }

        assert!(t.get(10).is_none());
        assert!(t.get(15).is_none());
        assert!(t.get(-1).is_none());

        Ok(())
    }

    fn struct_compare(b: &AVLTree, v: &[Option<Node>]) -> Result<(), Box<dyn Error>> {
        println!("{:?}", b);
        if v.len() == 0 {
            match b.root {
                Some(_) => return Err("Binary Tree should be Empty".into()),
                None => return Ok(()),
            }
        } else {
            match &b.root {
                None => return Err("Binary Tree is Empty".into()),
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
                                    if *cn != *n {
                                        return Err(
                                            format!("Actual: {}, Expected: {}", *cn, *n).into()
                                        );
                                    }
                                }
                                None => return Err("Node present in tree missing from Vec".into()),
                            }

                            match (&cn.lhs, &cn.rhs) {
                                (Some(lhs), Some(rhs)) => {
                                    if !visited[2 * ind + 1] {
                                        // Visit left child first
                                        match v[2 * ind + 1] {
                                            Some(_) => {
                                                ind = 2 * ind + 1;
                                                next = Rc::clone(lhs);
                                            }
                                            None => {
                                                return Err(format!(
                                                    "Unexpected node left of {}",
                                                    *cn
                                                )
                                                .into())
                                            }
                                        };
                                    } else if !visited[2 * ind + 2] {
                                        // Visit right child next
                                        match v[2 * ind + 2] {
                                            Some(_) => {
                                                ind = 2 * ind + 2;
                                                next = Rc::clone(rhs);
                                            }
                                            None => {
                                                return Err(format!(
                                                    "Unexpected node right of {}",
                                                    *cn
                                                )
                                                .into())
                                            }
                                        };
                                    } else {
                                        // Move up
                                        visited[ind] = true;
                                        match move_up_ind(ind) {
                                            Ok(i) => {
                                                ind = i;
                                                next = move_up_node(Rc::clone(&curr))?;
                                            }
                                            Err(_) => next = Rc::clone(&curr),
                                        };
                                    }
                                }
                                (Some(lhs), None) => {
                                    if visited[2 * ind + 1] {
                                        // If child is visited, step up
                                        visited[ind] = true;
                                        match move_up_ind(ind) {
                                            Ok(i) => {
                                                ind = i;
                                                next = move_up_node(Rc::clone(&curr))?;
                                            }
                                            Err(_) => next = Rc::clone(&curr),
                                        };
                                    } else {
                                        // Other wise step into child
                                        match v[2 * ind + 1] {
                                            Some(_) => {
                                                ind = 2 * ind + 1;
                                                next = Rc::clone(lhs);
                                            }
                                            None => {
                                                return Err(format!(
                                                    "Unexpected node left of {}",
                                                    *cn
                                                )
                                                .into())
                                            }
                                        };
                                    }
                                }
                                (None, Some(rhs)) => {
                                    if visited[2 * ind + 2] {
                                        // If child is visited, step up
                                        visited[ind] = true;
                                        match move_up_ind(ind) {
                                            Ok(i) => {
                                                ind = i;
                                                next = move_up_node(Rc::clone(&curr))?;
                                            }
                                            Err(_) => next = Rc::clone(&curr),
                                        };
                                    } else {
                                        // Other wise step into child
                                        match v[2 * ind + 2] {
                                            Some(_) => {
                                                ind = 2 * ind + 2;
                                                next = Rc::clone(rhs);
                                            }
                                            None => {
                                                return Err(format!(
                                                    "Unexpected node right of {}",
                                                    *cn
                                                )
                                                .into())
                                            }
                                        };
                                    }
                                }
                                (None, None) => {
                                    // if leaf node mark as visited and move up
                                    visited[ind] = true;
                                    match move_up_ind(ind) {
                                        Ok(i) => {
                                            ind = i;
                                            next = move_up_node(Rc::clone(&curr))?;
                                        }
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

    fn move_up_node(rn: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, Box<dyn Error>> {
        let n = rn.borrow();
        match &n.parent {
            Some(wp) => match wp.upgrade() {
                None => Err(format!("{} parent missing", n).into()),
                Some(sp) => Ok(Rc::clone(&sp)),
            },
            None => Err(format!("{} has no parent", n).into()),
        }
    }

    fn move_up_ind(ind: usize) -> Result<usize, Box<dyn Error>> {
        match ind {
            0 => Err("Root node cannot move up".to_string().into()),
            _ => Ok((ind - 1) / 2),
        }
    }
}
