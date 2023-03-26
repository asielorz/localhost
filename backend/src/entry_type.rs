use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum EntryType {
    Article { pages: i32 },
    Paper { pages: i32 },
    Book { pages: i32 },
    Video { length_in_seconds: i32 },
    Audio { length_in_seconds: i32 },
}

impl Default for EntryType {
    fn default() -> Self {
        EntryType::Article { pages: 0 }
    }
}

pub fn index(t: EntryType) -> i32 {
    match t {
        EntryType::Article { pages: _ } => 0,
        EntryType::Paper { pages: _ } => 1,
        EntryType::Book { pages: _ } => 2,
        EntryType::Video {
            length_in_seconds: _,
        } => 3,
        EntryType::Audio {
            length_in_seconds: _,
        } => 4,
    }
}

pub fn metadata(t: EntryType) -> i32 {
    match t {
        EntryType::Article { pages } => pages,
        EntryType::Paper { pages } => pages,
        EntryType::Book { pages } => pages,
        EntryType::Video { length_in_seconds } => length_in_seconds,
        EntryType::Audio { length_in_seconds } => length_in_seconds,
    }
}

pub fn from_index_and_metadata(index: i32, metadata: i32) -> EntryType {
    match index {
        0 => EntryType::Article { pages: metadata },
        1 => EntryType::Paper { pages: metadata },
        2 => EntryType::Book { pages: metadata },
        3 => EntryType::Video {
            length_in_seconds: metadata,
        },
        4 => EntryType::Audio {
            length_in_seconds: metadata,
        },
        _ => unreachable!(),
    }
}
