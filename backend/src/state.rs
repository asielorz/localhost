use serde::Serialize;
use std::sync::Mutex;

pub struct State {
    pub database: Option<rusqlite::Connection>,
}

impl State {
    const fn new() -> State {
        State { database: None }
    }
}

static STATE: Mutex<State> = Mutex::new(State::new());

pub fn global_state() -> &'static Mutex<State> {
    &STATE
}

pub fn get_all_strings_of(table: &str) -> rusqlite::Result<Vec<String>> {
    let mut entries: Vec<String> = Vec::new();

    {
        let state = STATE.lock().unwrap();
        let mut statement = state
            .database
            .as_ref()
            .unwrap()
            .prepare(&format!("SELECT * from {}", table))?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            entries.push(row.get(1)?)
        }
    }

    Ok(entries)
}

#[derive(Serialize, Debug, Clone)]
pub struct StringWithCategory {
    value: String,
    category: String,
}

pub fn get_all_strings_by_category_of(table: &str) -> rusqlite::Result<Vec<StringWithCategory>> {
    let mut entries: Vec<StringWithCategory> = Vec::new();

    {
        let state = STATE.lock().unwrap();
        let mut statement = state
            .database
            .as_ref()
            .unwrap()
            .prepare(&format!("SELECT * from {}", table))?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            entries.push(StringWithCategory {
                value: row.get(1)?,
                category: row.get(2)?,
            })
        }
    }

    Ok(entries)
}
