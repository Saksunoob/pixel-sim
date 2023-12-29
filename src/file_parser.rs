use std::{path::Path, fs::{DirEntry, self}};
use crate::world::Element;
use serde_json::Result;

pub fn load_elements(path: &Path) -> Option<Vec<Element>> {
    if path.is_file() {
        let file_content = match fs::read_to_string(path) {
            Ok(content) => Some(content),
            Err(_) => None,
        }?;
        match serde_json::from_str(&file_content) {
            Ok(element) => Some(element),
            Err(_) => None
        }
    } else {
        None
    }
}

pub fn load_element(path: &Path) -> Option<Element> {
    if path.is_file() {
        let file_content = match fs::read_to_string(path) {
            Ok(content) => Some(content),
            Err(_) => None,
        }?;
        match serde_json::from_str(&file_content) {
            Ok(element) => Some(element),
            Err(_) => None
        }
    } else {
        None
    }
}