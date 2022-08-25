use std::sync::Mutex;

pub struct State {
    pub database: Option<rusqlite::Connection>,
}

impl State {
    const fn new() -> State {
        return State { database: None };
    }
}

static STATE: Mutex<State> = Mutex::new(State::new());

pub fn global_state() -> &'static Mutex<State> {
    return &STATE;
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

    return Ok(entries);
}
