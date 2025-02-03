use std::error::Error;

pub mod binary_tree;

pub trait BinarySearchTree {
    fn insert(&mut self, key: i32, value: String) -> Result<(), BSTError>;
    fn remove(&mut self, key: i32) -> Result<(), BSTError>;
    fn get(&self, key: i32) -> Option<String>;
    fn update(&mut self, key: i32, value: String) -> Result<(), BSTError>;
}

#[derive(Debug)]
pub enum BSTError {
    KeyNotFound(i32),
    DoubleInsert(i32),
}

impl std::fmt::Display for BSTError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BSTError::KeyNotFound(k) => write!(f, "BSTError: Key \"{}\" not found in tree", k),
            BSTError::DoubleInsert(k) => write!(f, "BSTError: Key \"{}\" already in tree", k),
        }
    }
}

impl Error for BSTError {}
