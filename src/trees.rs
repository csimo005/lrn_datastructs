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
    fn tree_insert() {
        let mut t = BinaryTree::new();

        let _ = t.insert(3, "foo".to_string());
        let _ = t.insert(1, "bar".to_string());
        let _ = t.insert(2, "biz".to_string());
        let _ = t.insert(-1, "baz".to_string());
        let _ = t.insert(9, "buz".to_string());
        let _ = t.insert(4, "qux".to_string());
        let _ = t.insert(13, "qox".to_string());

        match &t.root {
            None => panic!("Newly inserted key does not exist!"),
            Some(r) => {
                assert_eq!(r.borrow().key, 3);
                assert_eq!(r.borrow().value, "foo");
                assert!(r.borrow().parent.is_none());

                match &r.borrow().lhs {
                    None => panic!("Root lhs node does not exist!"),
                    Some(lhs) => {
                        assert_eq!(lhs.borrow().key, 1);
                        assert_eq!(lhs.borrow().value, "bar");
                        match &lhs.borrow().parent {
                            None => panic!("Root lhs parent not set"),
                            Some(wp) => {
                                match wp.upgrade() {
                                    None => panic!("Root lhs parent no longer exists"),
                                    Some(sp) => assert!(Rc::ptr_eq(&sp, &r)),
                                };
                            }
                        };

                        match &lhs.borrow().lhs {
                            None => panic!("Root llhs node does not exist!"),
                            Some(llhs) => {
                                assert_eq!(llhs.borrow().key, -1);
                                assert_eq!(llhs.borrow().value, "baz");
                                match &llhs.borrow().parent {
                                    None => panic!("Root llhs parent not set"),
                                    Some(wp) => {
                                        match wp.upgrade() {
                                            None => panic!("Root llhs parent no longer exists"),
                                            Some(sp) => assert!(Rc::ptr_eq(&sp, &lhs)),
                                        };
                                    }
                                };
                            }
                        };

                        match &lhs.borrow().rhs {
                            None => panic!("Root lrhs node does not exist!"),
                            Some(lrhs) => {
                                assert_eq!(lrhs.borrow().key, 2);
                                assert_eq!(lrhs.borrow().value, "biz");
                                match &lrhs.borrow().parent {
                                    None => panic!("Root lrhs parent not set"),
                                    Some(wp) => {
                                        match wp.upgrade() {
                                            None => panic!("Root lrhs parent no longer exists"),
                                            Some(sp) => assert!(Rc::ptr_eq(&sp, &lhs)),
                                        };
                                    }
                                };
                            }
                        };
                    }
                };

                match &r.borrow().rhs {
                    None => panic!("Root rhs node does not exist!"),
                    Some(rhs) => {
                        assert_eq!(rhs.borrow().key, 9);
                        assert_eq!(rhs.borrow().value, "buz");
                        match &rhs.borrow().parent {
                            None => panic!("Root lhs parent not set"),
                            Some(wp) => {
                                match wp.upgrade() {
                                    None => panic!("Root lhs parent no longer exists"),
                                    Some(sp) => assert!(Rc::ptr_eq(&sp, &r)),
                                };
                            }
                        };

                        match &rhs.borrow().lhs {
                            None => panic!("Root llhs node does not exist!"),
                            Some(rlhs) => {
                                assert_eq!(rlhs.borrow().key, 4);
                                assert_eq!(rlhs.borrow().value, "qux");
                                match &rlhs.borrow().parent {
                                    None => panic!("Root rlhs parent not set"),
                                    Some(wp) => {
                                        match wp.upgrade() {
                                            None => panic!("Root rlhs parent no longer exists"),
                                            Some(sp) => assert!(Rc::ptr_eq(&sp, &rhs)),
                                        };
                                    }
                                };
                            }
                        };

                        match &rhs.borrow().rhs {
                            None => panic!("Root lrhs node does not exist!"),
                            Some(rrhs) => {
                                assert_eq!(rrhs.borrow().key, 13);
                                assert_eq!(rrhs.borrow().value, "qox");
                                match &rrhs.borrow().parent {
                                    None => panic!("Root rrhs parent not set"),
                                    Some(wp) => {
                                        match wp.upgrade() {
                                            None => panic!("Root rrhs parent no longer exists"),
                                            Some(sp) => assert!(Rc::ptr_eq(&sp, &rhs)),
                                        };
                                    }
                                };
                            }
                        };
                    }
                };
            }
        };

        let _ = t.write_graphviz("out/insertion_test.dot");
    }
    
    #[test]
    fn tree_remove_leaves() {
        let mut t = BinaryTree::new();

        if let Ok(n) = t.insert(3, "foo".to_string()) {
            println!("{t:?}");
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

    fn struct_compare(b: &BinaryTree, v: &Vec<Option<Node>>) -> Result<(), String> {
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
                                None => return Err("".to_string()),
                            }
                                
                            match (&cn.lhs, &cn.rhs) {
                                (Some(lhs), Some(rhs)) => {
                                    if !visited[2 * ind] { // Visit left child first
                                        match v[2 * ind] {
                                            Some(_) => {
                                                ind = 2 * ind;
                                                next = Rc::clone(lhs);
                                            },
                                            None => return Err(format!("Unexpected node left of {}", *cn)),
                                        };
                                    } else if !visited[2 * ind + 1] { // Visit right child next
                                        match v[2 * ind] {
                                            Some(_) => {
                                                ind = 2 * ind + 1;
                                                next = Rc::clone(rhs);
                                            },
                                            None => return Err(format!("Unexpected node right of {}", *cn)),
                                        };
                                    } else { // Move up
                                        visited[ind] = true;
                                        next = move_up(Rc::clone(&curr))?;
                                        ind = ind / 2;
                                    }
                                },
                                (Some(lhs), None) => {
                                    if visited[2 * ind] { // If child is visited, step up
                                        visited[ind] = true;
                                        next = move_up(Rc::clone(&curr))?;
                                        ind = ind / 2;
                                    } else { // Other wise step into child
                                        match v[2 * ind] {
                                            Some(_) => {
                                                ind = 2 * ind;
                                                next = Rc::clone(lhs);
                                            },
                                            None => return Err(format!("Unexpected node left of {}", *cn)),
                                        };
                                    }
                                },
                                (None, Some(rhs)) => {
                                    if visited[2 * ind + 1] { // If child is visited, step up
                                        visited[ind] = true;
                                        next = move_up(Rc::clone(&curr))?;
                                        ind = ind / 2;
                                    } else { // Other wise step into child
                                        match v[2 * ind + 1] {
                                            Some(_) => {
                                                ind = 2 * ind + 1;
                                                next = Rc::clone(rhs);
                                            },
                                            None => return Err(format!("Unexpected node right of {}", *cn)),
                                        };
                                    }
                                },
                                (None, None) => { // if leaf node mark as visited and move up
                                    visited[ind] = true;
                                    next = move_up(Rc::clone(&curr))?;
                                    ind = ind / 2;
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
