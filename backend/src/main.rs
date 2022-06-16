use std::convert::Infallible;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Request, Response, Server, Method, StatusCode};
use serde_json;
use serde::{Serialize, Deserialize};
use std::fs;
use std::sync::Mutex;
use chrono;
use chrono::Datelike;
use percent_encoding::percent_decode_str;
use std::cmp::{Ord, Ordering};
use std::env;
use sqlite;

#[macro_use]
extern crate lazy_static;

#[derive(Serialize, Deserialize, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum Month {
    January, February, March, April, May, June, July, August, September, October, November, December
}

impl Default for Month
{
    fn default() -> Self { Month::January }
}

fn month_from_index(index : u32) -> Month
{
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

fn month_to_index(month : Month) -> i32
{
    match month {
        Month::January   =>  1,
        Month::February  =>  2,
        Month::March     =>  3,
        Month::April     =>  4,
        Month::May       =>  5,
        Month::June      =>  6,
        Month::July      =>  7,
        Month::August    =>  8,
        Month::September =>  9,
        Month::October   => 10,
        Month::November  => 11,
        Month::December  => 12,
    }
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone, Eq, PartialEq, Default)]
struct Date {
    day : i32,
    month : Month,
    year : i32
}

impl Ord for Date {
    fn cmp(&self, other: &Self) -> Ordering {
        return (self.year, self.month, self.day).cmp(&(other.year, other.month, other.day));
    }
}

impl PartialOrd for Date {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        return Some(self.cmp(other));
    }
}

#[derive(Deserialize, Debug)]
struct NewEntryForm {
    link : String,
    title : String,
    description : String,
    author : String,
    category : String,
    themes : Vec<String>,
    works_mentioned : Vec<String>,
    tags : Vec<String>,
    date_published : Date,
    exceptional : bool,
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
struct Entry {
    id : i32,
    link : String,
    title : String,
    description : String,
    author : String,
    category : String,
    themes : Vec<String>,
    works_mentioned : Vec<String>,
    tags : Vec<String>,
    date_published : Date,
    date_saved : Date,
    exceptional : bool,
}

fn parse_date_query_argument(query_argument : &str) -> Option<Date>
{
    let elements : Vec<_> = query_argument.split("-").collect();
    if elements.len() != 3 {
        return None;
    }

    let year = match elements[0].parse::<i32>() {
        Ok(year) => year,
        Err(_) => return None
    };

    let month = match elements[1].parse::<u32>() {
        Ok(month) => {
            if month >= 1 && month <= 12 {
                month_from_index(month)
            } else {
                return None
            }
        },
        Err(_) => return None
    };

    let day = match elements[2].parse::<i32>() {
        Ok(day) => {
            if day >= 1 && day <= 31 {
                day
            } else {
                return None
            }
        },
        Err(_) => return None
    };

    return Some(Date {
        day : day,
        month : month,
        year : year
    });
}

fn url_to_sql_query(query_text : &str) -> Option<String>
{
    let mut result = String::from("SELECT * FROM entries WHERE ");

    if let Ok(decoded_query_text) = percent_decode_str(query_text).decode_utf8() {
        for query_argument in decoded_query_text.split("&")
        {
            let key_value : Vec<_> = query_argument.split("=").collect();
            if key_value.len() == 2 {
                match key_value[0] {
                    "link" => {
                        result += "link LIKE "; 
                        result += &escape_quotes_for_contains(key_value[1]);
                    }
                    "title" => {
                        result += "title LIKE "; 
                        result += &escape_quotes_for_contains(key_value[1]);
                    }
                    "author" => {
                        result += "author = "; 
                        result += &escape_quotes(key_value[1]);
                    }
                    "description" => {
                        result += "description LIKE "; 
                        result += &escape_quotes_for_contains(key_value[1]);
                    }
                    "category" => {
                        result += "category = "; 
                        result += &escape_quotes(key_value[1]);
                    }
                    "works_mentioned" => {
                        for s in key_value[1].split("|") {
                            result += "works_mentioned LIKE "; 
                            result += &escape_quotes_for_contains_in_list(s);
                            result += " AND ";
                        }
                        // Remove last " AND "
                        for _ in 0..5 {
                            result.pop();
                        }
                    }
                    "themes" => {
                        for s in key_value[1].split("|") {
                            result += "themes LIKE "; 
                            result += &escape_quotes_for_contains_in_list(s);
                            result += " AND ";
                        }
                        // Remove last " AND "
                        for _ in 0..5 {
                            result.pop();
                        }
                    }
                    "tags" => {
                        for s in key_value[1].split("|") {
                            result += "tags LIKE "; 
                            result += &escape_quotes_for_contains_in_list(s);
                            result += " AND ";
                        }
                        // Remove last " AND "
                        for _ in 0..5 {
                            result.pop();
                        }
                    }
                    "published_between_from" => {
                        result += "date_published >= DATE(";
                        result += &format_as_sql_date(parse_date_query_argument(key_value[1])?);
                        result += ")";
                    }
                    "published_between_until" => {
                        result += "date_published <= DATE(";
                        result += &format_as_sql_date(parse_date_query_argument(key_value[1])?);
                        result += ")";
                    }
                    "saved_between_from" => {
                        result += "date_saved >= DATE(";
                        result += &format_as_sql_date(parse_date_query_argument(key_value[1])?);
                        result += ")";
                    }
                    "saved_between_until" => {
                        result += "date_saved <= DATE(";
                        result += &format_as_sql_date(parse_date_query_argument(key_value[1])?);
                        result += ")";
                    }
                    "exceptional" => {
                        result += "exceptional = "; 
                        result += &format_as_sql_bool(key_value[1] == "true");
                    }
                    _ => { return None; }
                }
            }

            result += " AND ";
        }
    } else { 
        return None;
    }

    // Remove last " AND "
    for _ in 0..5 {
        result.pop();
    }

    result += " LIMIT 10";
    return Some(result);
}

fn today() -> Date
{
    let date = chrono::Local::today().naive_local();

    return Date{
        day : date.day() as i32,
        month : month_from_index(date.month()),
        year : date.year()
    };
}

struct State
{
    database : Option<sqlite::Connection>
}

impl State
{
    fn new() -> State
    {
        return State{ database : None };
    }
}

fn escape_quotes(string : &str) -> String
{
    return String::from("\"") + &string.replace("\"", "\\\"") + "\"";
}

fn escape_quotes_for_contains(string : &str) -> String
{
    return String::from("\"%") + &string.replace("\"", "\\\"") + "%\"";
}

fn escape_quotes_for_contains_in_list(string : &str) -> String
{
    return String::from("\"%|") + &string.replace("\"", "\\\"") + "|%\"";
}

fn format_as_sql_array(strings : &[String]) -> String
{
    if strings.is_empty() {
        return String::new();
    } else {
        return escape_quotes(&(String::from("|") + &strings.join("|") + "|"));
    }
}

fn read_from_sql_array(string : &str) -> Vec<String>
{
    if string.is_empty() {
        return Vec::new();
    } else {
        return string[1..string.len() - 1].split("|").map(String::from).collect();
    }
}

fn format_as_sql_date(date : Date) -> String
{
    return format!("'{:04}-{:02}-{:02}'", date.year, month_to_index(date.month), date.day);
}

fn read_sql_date(text : &str) -> Option<Date>
{
    // It is the same format.
    return parse_date_query_argument(text);
}

fn format_as_sql_bool(b : bool) -> &'static str
{
    if b {
        return "TRUE";
    } else {
        return "FALSE";
    }
}

fn run_sql(database : &sqlite::Connection, command : &str) -> sqlite::Result<()>
{
    let result = database.execute(command);
    if let Err(err) = &result {
        println!("SQL error: {}", err);
    }
    return result;
}

fn get_all_strings_of(table : &str) -> Vec<String>
{
    let mut entries : Vec<String> = Vec::new();

    {
        let state = STATE.lock().unwrap();
        state.database.as_ref().unwrap().iterate(format!("SELECT * from {}", table), |pairs| {
            for &(column, value) in pairs.iter() {
                if column == "value" {
                    if let Some(x) = value {
                        entries.push(String::from(x));
                    }
                }
            }
            true
        }).unwrap();
    }

    return entries;
}

lazy_static! 
{
    static ref STATE: Mutex<State> = Mutex::new(State::new());
}

// TO DO: Parameterize
static DATABASE_FILENAME : &str = "./target/entries.db";

struct Requests{}
impl Requests
{
    fn options() -> Result<Response<Body>, hyper::Error>
    {
        println!("Options!");
        return Response::builder()
            .status(StatusCode::NO_CONTENT)
            .header("Allow", "OPTIONS, GET, POST")
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .body(Body::from(""))
            .or_else(|_| Requests::internal_server_error_response());
    }

    fn internal_server_error_response() -> Result<Response<Body>, hyper::Error>
    {
        println!("500 Internal server error");
        return Ok(Response::builder()
            .status(StatusCode::INTERNAL_SERVER_ERROR)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .body(Body::from(""))
            .unwrap()
        );
    }

    fn entries_as_http_response(entries : &[Entry]) -> Result<Response<Body>, hyper::Error>
    {
        if let Ok(json) = serde_json::to_string(entries) {
            return Response::builder()
                .status(StatusCode::OK)
                .header("Access-Control-Allow-Origin", "*")
                .header("Access-Control-Allow-Headers", "*")
                .header("Content-Type", "application/json")
                .body(Body::from(json))
                .or_else(|_| Requests::internal_server_error_response());
        } else {
            return Requests::internal_server_error_response();
        }
    }

    fn get_texts(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let mut served_entries : Vec<Entry> = Vec::new();

        let sql_query = match req.uri().query() {
            Some(query_text) => match url_to_sql_query(query_text) {
                Some(result) => result,
                None => { return Requests::internal_server_error_response(); }
            },
            None => String::from("SELECT * FROM entries LIMIT 10")
        };

        {
            let state = STATE.lock().unwrap();
            state.database.as_ref().unwrap().iterate(sql_query, |pairs| {
                let mut entry : Entry = Default::default();
                for &(column, value) in pairs.iter() {
                    if let Some(x) = value {
                        if column == "link" {
                            entry.link = String::from(x);
                        } else if column == "title" {
                            entry.title = String::from(x);
                        } else if column == "description" {
                            entry.description = String::from(x);
                        } else if column == "author" {
                            entry.author = String::from(x);
                        } else if column == "category" {
                            entry.category = String::from(x);
                        } else if column == "themes" {
                            entry.themes = read_from_sql_array(x);
                        } else if column == "works_mentioned" {
                            entry.works_mentioned = read_from_sql_array(x);
                        } else if column == "tags" {
                            entry.tags = read_from_sql_array(x);
                        } else if column == "date_published" {
                            if let Some(date) = read_sql_date(x) {
                                entry.date_published = date;
                            }
                        } else if column == "date_saved" {
                            if let Some(date) = read_sql_date(x) {
                                entry.date_saved = date;
                            }
                        } else if column == "exceptional" {
                            entry.exceptional = x == "TRUE";
                        }
                    }
                }
                served_entries.push(entry);
                true
            }).unwrap();
        }

        return Requests::entries_as_http_response(&served_entries);
    }

    fn strings_as_http_response(strings : &Vec<String>) -> Result<Response<Body>, hyper::Error>
    {
        if let Ok(json) = serde_json::to_string(strings) {
            return Response::builder()
                .status(StatusCode::OK)
                .header("Access-Control-Allow-Origin", "*")
                .header("Access-Control-Allow-Headers", "*")
                .header("Content-Type", "application/json")
                .body(Body::from(json))
                .or_else(|_| Requests::internal_server_error_response());
        } else {
            return Requests::internal_server_error_response();
        }
    }

    fn get_categories() -> Result<Response<Body>, hyper::Error>
    {
        return Requests::strings_as_http_response(&get_all_strings_of("categories"));
    }

    fn get_authors() -> Result<Response<Body>, hyper::Error>
    {
        return Requests::strings_as_http_response(&get_all_strings_of("authors"));
    }

    fn get_themes() -> Result<Response<Body>, hyper::Error>
    {
        return Requests::strings_as_http_response(&get_all_strings_of("themes"));
    }

    fn get_works() -> Result<Response<Body>, hyper::Error>
    {
        return Requests::strings_as_http_response(&get_all_strings_of("works"));
    }

    fn get_tags() -> Result<Response<Body>, hyper::Error>
    {
        return Requests::strings_as_http_response(&get_all_strings_of("tags"));
    }

    async fn post_texts(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let whole_body = hyper::body::to_bytes(req.into_body()).await?;
        match serde_json::from_slice(&whole_body) as Result<NewEntryForm, serde_json::Error> {
            Ok(form) => {
                let state = STATE.lock().unwrap();

                if let Some(database) = &state.database {
                    let result = database.execute(format!(
                        "
                        INSERT INTO entries (link, title, description, author, category, themes, works_mentioned, tags, date_published, date_saved, exceptional)
                        VALUES ({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {})
                        ",
                        escape_quotes(&form.link), escape_quotes(&form.title), escape_quotes(&form.description), escape_quotes(&form.author), escape_quotes(&form.category), 
                        format_as_sql_array(&form.themes), format_as_sql_array(&form.works_mentioned), format_as_sql_array(&form.tags),
                        format_as_sql_date(form.date_published), format_as_sql_date(today()), format_as_sql_bool(form.exceptional)
                    ));

                    if let Err(err) = result {
                        println!("Insert to database failed: {}", err);
                        return Requests::internal_server_error_response();
                    }

                    // Ignore errors when inserting an element that is already present in tables of unique values.
                    _ = run_sql(&database, &format!("INSERT INTO authors (value) VALUES ({})", escape_quotes(&form.author)));
                    _ = run_sql(&database, &format!("INSERT INTO categories (value) VALUES ({})", escape_quotes(&form.category)));
                    for theme in &form.themes {
                        _ = run_sql(&database, &format!("INSERT INTO themes (value) VALUES ({})", escape_quotes(theme)));
                    }
                    for work in &form.works_mentioned {
                        _ = run_sql(&database, &format!("INSERT INTO works (value) VALUES ({})", escape_quotes(work)));
                    }
                    for tag in &form.tags {
                        _ = run_sql(&database, &format!("INSERT INTO tags (value) VALUES ({})", escape_quotes(tag)));
                    }
                } else {
                    return Requests::internal_server_error_response();
                }

                Response::builder()
                    .status(StatusCode::OK)
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "*")
                    .header("Content-Type", "application/json")
                    .body(Body::from(r#"{"success":"true"}"#))
                    .or_else(|_| Requests::internal_server_error_response())
            }
            Err(err) => {
                println!("Rejecting bad request: {}", err);
                
                Response::builder()
                    .status(StatusCode::BAD_REQUEST)
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "*")
                    .header("Content-Type", "application/json")
                    .body(Body::from(format!("Invalid entry: {}", err)))
                    .or_else(|_| Requests::internal_server_error_response())
            }
        }
    }

    fn serve_page(path : &str) -> Result<Response<Body>, hyper::Error>
    {
        return Requests::serve_file(path, "text/html");
    }

    fn serve_file(path : &str, content_type : &str) -> Result<Response<Body>, hyper::Error>
    {
        match fs::read(path) {
            Ok(content) =>{
                Response::builder()
                    .status(StatusCode::OK)
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "*")
                    .header("Content-Type", content_type)
                    .body(Body::from(content))
                    .or_else(|_| Requests::internal_server_error_response())
            }
            Err(_) => Requests::internal_server_error_response()
        }
    }
}

async fn process_request(req : Request<Body>) -> Result<Response<Body>, hyper::Error> 
{
    match req.uri().query() {
        Some(query_text) => { println!("{} request at path {} with query {}", req.method(), req.uri().path(), query_text); }
        None             => { println!("{} request at path {}", req.method(), req.uri().path()); }
    };

    match (req.method(), req.uri().path()) {
        (&Method::OPTIONS, _) => Requests::options(),

        (&Method::GET, "/new_entry") => Requests::serve_page("pages/new_entry.html"),
        (&Method::GET, "/search") => Requests::serve_page("pages/search.html"),
        (&Method::GET, "/favicon.ico") => Requests::serve_file("pages/favicon.ico", "image/vnd.microsoft.icon"),

        (&Method::GET, "/api/texts") => Requests::get_texts(req),
        (&Method::POST, "/api/texts") => Requests::post_texts(req).await,

        (&Method::GET, "/api/categories") => Requests::get_categories(),
        (&Method::GET, "/api/authors") => Requests::get_authors(),
        (&Method::GET, "/api/themes") => Requests::get_themes(),
        (&Method::GET, "/api/works") => Requests::get_works(),
        (&Method::GET, "/api/tags") => Requests::get_tags(),

        // Return the 404 Not Found for other routes.
        _ => {
            println!("404 Not found");
            Ok(Response::builder()
                .status(StatusCode::NOT_FOUND)
                .body(Body::from(""))
                .unwrap()
            )
        }
    }
}

#[tokio::main]
pub async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    
    let connection = sqlite::open(DATABASE_FILENAME)?;

    connection.execute(
        "
        CREATE TABLE IF NOT EXISTS entries (
            entry_id INT PRIMARY KEY, 
            link TEXT NOT NULL COLLATE NOCASE, 
            title TEXT NOT NULL COLLATE NOCASE,
            description TEXT NOT NULL COLLATE NOCASE,
            author TEXT NOT NULL,
            category TEXT NOT NULL,
            themes TEXT NUT NULL,
            works_mentioned TEXT NOT NULL,
            tags TEXT NOT NULL,
            date_published DATE NOT NULL,
            date_saved DATE NOT NULL,
            exceptional BOOL NOT NULL,
            image BLOB,
            backup BLOB
        );
        "
    )?;

    connection.execute("CREATE TABLE IF NOT EXISTS authors (entry_id INT PRIMARY KEY, value TEXT UNIQUE NOT NULL);")?;
    connection.execute("CREATE TABLE IF NOT EXISTS categories (entry_id INT PRIMARY KEY, value TEXT UNIQUE NOT NULL);")?;
    connection.execute("CREATE TABLE IF NOT EXISTS themes (entry_id INT PRIMARY KEY, value TEXT UNIQUE NOT NULL);")?;
    connection.execute("CREATE TABLE IF NOT EXISTS works (entry_id INT PRIMARY KEY, value TEXT UNIQUE NOT NULL);")?;
    connection.execute("CREATE TABLE IF NOT EXISTS tags (entry_id INT PRIMARY KEY, value TEXT UNIQUE NOT NULL);")?;

    STATE.lock().unwrap().database = Some(connection);

    // For every connection, we must make a `Service` to handle all
    // incoming HTTP requests on said connection.
    let make_svc = make_service_fn(|_conn| {
        // This is the `Service` that will handle the connection.
        // `service_fn` is a helper to convert a function that
        // returns a Response into a `Service`.
        async { Ok::<_, Infallible>(service_fn(process_request)) }
    });

    let addr = ([127, 0, 0, 1], 8080).into();

    let server = Server::bind(&addr).serve(make_svc);

    println!("Current dir: {}", env::current_dir().unwrap().to_str().unwrap());
    println!("Listening on http://{}", addr);

    server.await?;

    Ok(())
}

// ********************************************************************************************************************

#[cfg(test)]
mod tests 
{
    use super::*;

    // url_to_sql_query

    #[test]
    fn test_url_to_sql_query_link_checks_for_containment() {
        let url_params = "link=wikipedia";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE link LIKE "%wikipedia%" LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_title_checks_for_containment() {
        let url_params = "title=Hello";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE title LIKE "%Hello%" LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_author_checks_for_equality() {
        let url_params = "author=Pauline%20Kael";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE author = "Pauline Kael" LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_description_checks_for_containment() {
        let url_params = "description=compiler";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE description LIKE "%compiler%" LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_category_checks_for_equality() {
        let url_params = "category=Programming";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE category = "Programming" LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_themes_checks_for_containment_of_each() {
        let url_params = "themes=Rust%7CTesting";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE themes LIKE "%|Rust|%" AND themes LIKE "%|Testing|%" LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_works_checks_for_containment_of_each() {
        let url_params = "works_mentioned=Hamlet%7CMacBeth%7CKing%20Lear";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE works_mentioned LIKE "%|Hamlet|%" AND works_mentioned LIKE "%|MacBeth|%" AND works_mentioned LIKE "%|King Lear|%" LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_tags_checks_for_containment_of_each() {
        let url_params = "tags=Soulslike%7CGreat%20soundtrack%7CFemale%20protagonist";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE tags LIKE "%|Soulslike|%" AND tags LIKE "%|Great soundtrack|%" AND tags LIKE "%|Female protagonist|%" LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_date_is_formated_as_yyyy_mm_dd() {
        let url_params = "published_between_from=1967-5-3";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE date_published >= DATE('1967-05-03') LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_published_between_until_checks_for_less_equal() {
        let url_params = "published_between_until=1967-11-24";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE date_published <= DATE('1967-11-24') LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_bool_true_is_formated_as_uppercase_true() {
        let url_params = "exceptional=true";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE exceptional = TRUE LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_bool_false_is_formated_as_uppercase_false() {
        let url_params = "exceptional=false";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE exceptional = FALSE LIMIT 10"#),
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_two_or_more_predicates_are_anded() {
        let url_params = "link=wikipedia&author=Pauline%20Kael&tags=Soulslike%7CGreat%20soundtrack%7CFemale%20protagonist";
        match url_to_sql_query(url_params) {
            Some(query) => assert_eq!(query, r#"SELECT * FROM entries WHERE link LIKE "%wikipedia%" AND author = "Pauline Kael" AND tags LIKE "%|Soulslike|%" AND tags LIKE "%|Great soundtrack|%" AND tags LIKE "%|Female protagonist|%" LIMIT 10"#),
            None => unreachable!()
        }
    }

    // format_as_sql_array

    #[test]
    fn test_format_as_sql_array_empty_array_is_formatted_as_empty_string() {
        let strings = [];
        let as_sql_array = format_as_sql_array(&strings);
        assert_eq!(as_sql_array, "");
    }

    #[test]
    fn test_format_as_sql_array_a_single_element_is_surrounded_by_or_bars_and_quotes() {
        let strings = [ String::from("Iruña") ];
        let as_sql_array = format_as_sql_array(&strings);
        assert_eq!(as_sql_array, r#""|Iruña|""#);
    }

    #[test]
    fn test_format_as_sql_array_several_elements_are_separated_by_or_bars() {
        let strings = [ String::from("Iruña"), String::from("Bilbo"), String::from("Gasteiz"), String::from("Donostia") ];
        let as_sql_array = format_as_sql_array(&strings);
        assert_eq!(as_sql_array, r#""|Iruña|Bilbo|Gasteiz|Donostia|""#);
    }

    // read_from_sql_array

    #[test]
    fn test_read_from_sql_array_empty_string_is_read_as_empty_array() {
        let strings = read_from_sql_array("");
        let expected_output : Vec<String> = Vec::new();
        assert_eq!(strings, expected_output);
    }

    #[test]
    fn test_read_from_sql_array_single_element_must_be_surrounded_by_or_bars() {
        let strings = read_from_sql_array("|Iruña|");
        let expected_output = [ String::from("Iruña") ];
        assert_eq!(strings, expected_output);
    }

    #[test]
    fn test_read_from_sql_array_several_elements_must_be_separated_by_or_bars() {
        let strings = read_from_sql_array("|Iruña|Bilbo|Gasteiz|Donostia|");
        let expected_output = [ String::from("Iruña"), String::from("Bilbo"), String::from("Gasteiz"), String::from("Donostia") ];
        assert_eq!(strings, expected_output);
    }
}
