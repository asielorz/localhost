use chrono::Datelike;
use serde::{Deserialize, Serialize};
use std::cmp::{Ord, Ordering};

#[derive(Serialize, Deserialize, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Month {
    January,
    February,
    March,
    April,
    May,
    June,
    July,
    August,
    September,
    October,
    November,
    December,
}

impl Default for Month {
    fn default() -> Self {
        Month::January
    }
}

pub fn month_from_index(index: u32) -> Month {
    match index {
        1 => Month::January,
        2 => Month::February,
        3 => Month::March,
        4 => Month::April,
        5 => Month::May,
        6 => Month::June,
        7 => Month::July,
        8 => Month::August,
        9 => Month::September,
        10 => Month::October,
        11 => Month::November,
        _ => Month::December,
    }
}

pub fn month_to_index(month: Month) -> i32 {
    match month {
        Month::January => 1,
        Month::February => 2,
        Month::March => 3,
        Month::April => 4,
        Month::May => 5,
        Month::June => 6,
        Month::July => 7,
        Month::August => 8,
        Month::September => 9,
        Month::October => 10,
        Month::November => 11,
        Month::December => 12,
    }
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct Date {
    pub day: i32,
    pub month: Month,
    pub year: i32,
}

impl Ord for Date {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.year, self.month, self.day).cmp(&(other.year, other.month, other.day))
    }
}

impl PartialOrd for Date {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn today() -> Date {
    let date = chrono::Local::today().naive_local();

    Date {
        day: date.day() as i32,
        month: month_from_index(date.month()),
        year: date.year(),
    }
}

pub fn format_as_sql_date(date: Date) -> String {
    format!(
        "{:04}-{:02}-{:02}",
        date.year,
        month_to_index(date.month),
        date.day
    )
}

pub fn read_sql_date(text: &str) -> Option<Date> {
    let elements: Vec<_> = text.split('-').collect();
    if elements.len() != 3 {
        return None;
    }

    let year = match elements[0].parse::<i32>() {
        Ok(year) => year,
        Err(_) => return None,
    };

    let month = match elements[1].parse::<u32>() {
        Ok(month) => {
            if (1..=12).contains(&month) {
                month_from_index(month)
            } else {
                return None;
            }
        }
        Err(_) => return None,
    };

    let day = match elements[2].parse::<i32>() {
        Ok(day) => {
            if (1..=31).contains(&day) {
                day
            } else {
                return None;
            }
        }
        Err(_) => return None,
    };

    Some(Date { day, month, year })
}
