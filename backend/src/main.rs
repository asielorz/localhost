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
use rusqlite;

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
    id : i64,
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

fn url_to_sql_query(query_text : &str) -> Option<(String, Vec<String>)>
{
    let mut result = String::from("SELECT * FROM entries WHERE ");
    let mut params : Vec<String> = Vec::new();

    if let Ok(decoded_query_text) = percent_decode_str(query_text).decode_utf8() {
        for query_argument in decoded_query_text.split("&")
        {
            let key_value : Vec<_> = query_argument.split("=").collect();
            if key_value.len() == 2 {
                match key_value[0] {
                    "link" => {
                        result += "link LIKE ?";
                        params.push(sql_arg_string_contains(key_value[1]));
                    }
                    "title" => {
                        result += "title LIKE ?";
                        params.push(sql_arg_string_contains(key_value[1]));
                    }
                    "author" => {
                        result += "author = ?";
                        params.push(String::from(key_value[1]));
                    }
                    "description" => {
                        result += "description LIKE ?";
                        params.push(sql_arg_string_contains(key_value[1]));
                    }
                    "category" => {
                        result += "category = ?";
                        params.push(String::from(key_value[1]));
                    }
                    "works_mentioned" => {
                        for s in key_value[1].split("|") {
                            result += "works_mentioned LIKE ? AND "; 
                            params.push(sql_arg_list_contains(s));
                        }
                        // Remove last " AND "
                        for _ in 0..5 {
                            result.pop();
                        }
                    }
                    "themes" => {
                        for s in key_value[1].split("|") {
                            result += "themes LIKE ? AND ";
                            params.push(sql_arg_list_contains(s));
                        }
                        // Remove last " AND "
                        for _ in 0..5 {
                            result.pop();
                        }
                    }
                    "tags" => {
                        for s in key_value[1].split("|") {
                            result += "tags LIKE ? AND ";
                            params.push(sql_arg_list_contains(s));
                        }
                        // Remove last " AND "
                        for _ in 0..5 {
                            result.pop();
                        }
                    }
                    "published_between_from" => {
                        result += "date_published >= DATE(?)";
                        params.push(format_as_sql_date(parse_date_query_argument(key_value[1])?));
                    }
                    "published_between_until" => {
                        result += "date_published <= DATE(?)";
                        params.push(format_as_sql_date(parse_date_query_argument(key_value[1])?));
                    }
                    "saved_between_from" => {
                        result += "date_saved >= DATE(?)";
                        params.push(format_as_sql_date(parse_date_query_argument(key_value[1])?));
                    }
                    "saved_between_until" => {
                        result += "date_saved <= DATE(?)";
                        params.push(format_as_sql_date(parse_date_query_argument(key_value[1])?));
                    }
                    "exceptional" => {
                        result += "exceptional = ";
                        result += if key_value[1] == "true" { "TRUE" } else { "FALSE" }; 
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
    return Some((result, params));
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
    database : Option<rusqlite::Connection>
}

impl State
{
    fn new() -> State
    {
        return State{ database : None };
    }
}

fn sql_arg_string_contains(string : &str) -> String
{
    return String::from("%") + string + "%";
}

fn sql_arg_list_contains(string : &str) -> String
{
    return String::from("%|") + string + "|%";
}

fn format_as_sql_array(strings : &[String]) -> String
{
    if strings.is_empty() {
        return String::from("");
    } else {
        return String::from("|") + &strings.join("|") + "|";
    }
}

fn read_from_sql_array(string : &str) -> Vec<String>
{
    if string.is_empty() || string == "||" {
        return Vec::new();
    } else {
        return string[1..string.len() - 1].split("|").map(String::from).collect();
    }
}

fn format_as_sql_date(date : Date) -> String
{
    return format!("{:04}-{:02}-{:02}", date.year, month_to_index(date.month), date.day);
}

fn read_sql_date(text : &str) -> Option<Date>
{
    // It is the same format.
    return parse_date_query_argument(text);
}

fn run_sql<Params : rusqlite::Params>(database : &rusqlite::Connection, command : &str, params : Params) -> rusqlite::Result<usize>
{
    let result = database.execute(command, params);
    if let Err(err) = &result {
        println!("SQL error: {}", err);
    }
    return result;
}

fn get_all_strings_of(table : &str) -> rusqlite::Result<Vec<String>>
{
    let mut entries : Vec<String> = Vec::new();

    {
        let state = STATE.lock().unwrap();
        let mut statement = state.database.as_ref().unwrap().prepare(&format!("SELECT * from {}", table))?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            entries.push(row.get(1)?)
        }
    }

    return Ok(entries);
}

fn select_texts<Params : rusqlite::Params>(database : &rusqlite::Connection, sql_query : &str, sql_params : Params) -> rusqlite::Result<Vec<Entry>> {
    let mut found_entries : Vec<Entry> = Vec::new();

    let mut statement = database.prepare(sql_query)?;
    let mut rows = statement.query(sql_params)?;
    while let Some(row) = rows.next()? {
        found_entries.push(Entry{
            id : row.get(0)?,
            link : row.get(1)?,
            title : row.get(2)?,
            description : row.get(3)?,
            author : row.get(4)?,
            category : row.get(5)?,
            themes : read_from_sql_array(&row.get::<_, String>(6)?),
            works_mentioned : read_from_sql_array(&row.get::<_, String>(7)?),
            tags : read_from_sql_array(&row.get::<_, String>(8)?),
            date_published : read_sql_date(&row.get::<_, String>(9)?).unwrap(),
            date_saved : read_sql_date(&row.get::<_, String>(10)?).unwrap(),
            exceptional : row.get(11)?,
        });
    }

    return Ok(found_entries);
}

fn is_single_entry_path(path : &str) -> bool {
    if let Some(p) = path.strip_prefix("/api/texts/") {
        return p.parse::<i64>().is_ok();
    }
    return false;
}

fn is_entry_image_path(path : &str) -> bool {
    if let Some(p) = path.strip_prefix("/api/texts/") {
        let parts = p.split("/").collect::<Vec<_>>();
        if parts.len() != 2 {
            return false;
        }
        
        return parts[0].parse::<i64>().is_ok() && parts[1] == "image";
    }

    return false;
}

fn get_entry_id_from_path(path : &str) -> i64 {
    if let Some(p) = path.strip_prefix("/api/texts/") {
        let parts = p.split("/").collect::<Vec<_>>();
        return parts[0].parse::<i64>().unwrap();
    }

    unreachable!();
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
            .header("Allow", "OPTIONS, GET, PUT, POST")
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

    fn not_found_404_response() -> Result<Response<Body>, hyper::Error>
    {
        println!("404 Not found");
        Ok(Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(Body::from(""))
            .unwrap()
        )
    }

    fn bad_request_response(error_message : &str) -> Result<Response<Body>, hyper::Error>
    {
        println!("Rejecting bad request: {}", error_message);

        Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .header("Content-Type", "application/json")
            .body(Body::from(format!(r#"{{"error_message":"{}""#, error_message)))
            .or_else(|_| Requests::internal_server_error_response())
    }

    fn to_json_http_response<T : Serialize>(entries : &T) -> Result<Response<Body>, hyper::Error>
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
        let (sql_query, sql_params) = match req.uri().query() {
            Some(query_text) => match url_to_sql_query(query_text) {
                Some(result) => result,
                None => { return Requests::internal_server_error_response(); }
            },
            None => (String::from("SELECT * FROM entries LIMIT 10"), Vec::new())
        };

        let served_entries = {
            let state = STATE.lock().unwrap();
            match select_texts(state.database.as_ref().unwrap(), &sql_query, sql_params.iter().map(|x| x as &dyn rusqlite::ToSql).collect::<Vec<&dyn rusqlite::ToSql>>().as_slice()) {
                Ok(entries) => entries,
                Err(err) => {
                    println!("SQL query error: {}", err);
                    return Requests::internal_server_error_response();
                }
            }
        };

        return Requests::to_json_http_response(&served_entries);
    }

    fn get_single_text(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let path = req.uri().path().strip_prefix("/api/texts/").unwrap();
        if let Ok(entry_id) = path.parse::<i64>() {
            let served_entries = {
                let state = STATE.lock().unwrap();
                match select_texts(state.database.as_ref().unwrap(), "SELECT * FROM entries WHERE entry_id = ?1", [ entry_id ]) {
                    Ok(entries) => entries,
                    Err(err) => {
                        println!("{}", err);
                        return Requests::internal_server_error_response();
                    }
                }
            };
            if served_entries.is_empty() {
                return Requests::not_found_404_response();
            } else {
                return Requests::to_json_http_response(&served_entries[0]);
            }
        } else {
            return Requests::bad_request_response(&format!("Could not convert '{}' to an entry id", path));
        }
    }

    fn get_entry_image(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let entry_id = get_entry_id_from_path(req.uri().path());

        let state = STATE.lock().unwrap();
        let database = state.database.as_ref().unwrap();

        let mut statement = match database.prepare("SELECT image FROM entries WHERE entry_id = ?") {
            Ok(s) => s,
            Err(_) => { return Requests::internal_server_error_response(); }
        };
        let mut rows = match statement.query([entry_id]) {
            Ok(r) => r,
            Err(_) => { return Requests::internal_server_error_response(); }
        };
        
        let first = match rows.next() {
            Ok(r) => r,
            Err(_) => { return Requests::internal_server_error_response(); }
        };

        if let Some(found_row) = first {
            let blob_ref = match found_row.get_ref(0) {
                Ok(r) => r,
                Err(_) => { return Requests::internal_server_error_response(); }
            };

            let maybe_blob = match blob_ref.as_blob_or_null() {
                Ok(r) => r,
                Err(_) => { return Requests::internal_server_error_response(); }
            };

            if let Some(blob) = maybe_blob {
                let copy = Vec::from(blob);

                Response::builder()
                    .status(StatusCode::OK)
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "*")
                    .header("Content-Type", "image/png")
                    .body(Body::from(copy))
                    .or_else(|_| Requests::internal_server_error_response())
            } else {
                return Requests::not_found_404_response();
            }
        } else {
            return Requests::not_found_404_response();
        }
    }

    async fn put_entry_image(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let entry_id = get_entry_id_from_path(req.uri().path());

        match req.headers().get("Content-Type") {
            None => { return Requests::bad_request_response("Missing content type hader"); }
            Some(value) => match value.to_str().unwrap() {
                // TO DO: Save image type.
                "image/png" => (),
                "image/jpeg" => (),
                "image/gif" => (),
                "image/bmp" => (),
                other => { return Requests::bad_request_response(&format!("Unsuported content type: {}", other)); }
            }
        };

        let whole_body = hyper::body::to_bytes(req.into_body()).await?;

        let state = STATE.lock().unwrap();
        let database = state.database.as_ref().unwrap();
        match database.execute(
            "
            UPDATE entries
            SET image = ?
            WHERE employee_id = ?
            ",
            rusqlite::params![rusqlite::blob::ZeroBlob(whole_body.len() as i32), entry_id]
        ) {
            // No values where modified.
            Ok(0) => { return Requests::not_found_404_response(); }
            // Something went wrong within the database.
            Err(_) => { return Requests::internal_server_error_response(); }
            // Everything went fine.
            Ok(_) => ()
        };

        let mut blob = match database.blob_open(rusqlite::MAIN_DB, "entries", "image", entry_id, false) {
            Ok(b) => b,
            Err(_) => { return Requests::internal_server_error_response(); }
        };

        if let Err(_) = blob.write_at(&whole_body, 0) {
            return Requests::internal_server_error_response();
        };

        Response::builder()
            .status(StatusCode::OK)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .header("Content-Type", "application/json")
            .body(Body::from(format!(r#"{{"success":true,"link":"http://localhost:8080/api/texts/{}/image"}}"#, entry_id)))
            .or_else(|_| Requests::internal_server_error_response())
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
        match &get_all_strings_of("categories") {
            Ok(strings) => Requests::strings_as_http_response(strings),
            Err(_) => Requests::internal_server_error_response()
        }
    }

    fn get_authors() -> Result<Response<Body>, hyper::Error>
    {
        match &get_all_strings_of("authors") {
            Ok(strings) => Requests::strings_as_http_response(strings),
            Err(_) => Requests::internal_server_error_response()
        }
    }

    fn get_themes() -> Result<Response<Body>, hyper::Error>
    {
        match &get_all_strings_of("themes") {
            Ok(strings) => Requests::strings_as_http_response(strings),
            Err(_) => Requests::internal_server_error_response()
        }
    }

    fn get_works() -> Result<Response<Body>, hyper::Error>
    {
        match &get_all_strings_of("works") {
            Ok(strings) => Requests::strings_as_http_response(strings),
            Err(_) => Requests::internal_server_error_response()
        }
    }

    fn get_tags() -> Result<Response<Body>, hyper::Error>
    {
        match &get_all_strings_of("tags") {
            Ok(strings) => Requests::strings_as_http_response(strings),
            Err(_) => Requests::internal_server_error_response()
        }
    }

    async fn post_texts(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let whole_body = hyper::body::to_bytes(req.into_body()).await?;
        match serde_json::from_slice(&whole_body) as Result<NewEntryForm, serde_json::Error> {
            Ok(form) => {
                let state = STATE.lock().unwrap();

                if let Some(database) = &state.database {
                    let result = database.execute(
                        "
                        INSERT INTO entries (link, title, description, author, category, themes, works_mentioned, tags, date_published, date_saved, exceptional)
                        VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11);
                        ",
                        rusqlite::params![ &form.link
                        , &form.title
                        , &form.description
                        , &form.author
                        , &form.category 
                        , &format_as_sql_array(&form.themes) 
                        , &format_as_sql_array(&form.works_mentioned) 
                        , &format_as_sql_array(&form.tags)
                        , &format_as_sql_date(form.date_published)
                        , &format_as_sql_date(today())
                        , form.exceptional
                        ]
                    );

                    if let Err(err) = result {
                        println!("Insert to database failed: {}", err);
                        return Requests::internal_server_error_response();
                    }

                    let last_insert_row_id = database.last_insert_rowid();

                    // Ignore errors when inserting an element that is already present in tables of unique values.
                    _ = run_sql(&database, "INSERT INTO authors (value) VALUES (?)", [&form.author]);
                    _ = run_sql(&database, "INSERT INTO categories (value) VALUES (?)", [&form.category]);
                    for theme in &form.themes {
                        _ = run_sql(&database, "INSERT INTO themes (value) VALUES (?)", [theme]);
                    }
                    for work in &form.works_mentioned {
                        _ = run_sql(&database, "INSERT INTO works (value) VALUES (?)", [work]);
                    }
                    for tag in &form.tags {
                        _ = run_sql(&database, "INSERT INTO tags (value) VALUES (?)", [tag]);
                    }

                    Response::builder()
                        .status(StatusCode::OK)
                        .header("Access-Control-Allow-Origin", "*")
                        .header("Access-Control-Allow-Headers", "*")
                        .header("Content-Type", "application/json")
                        .body(Body::from(format!(r#"{{"success":true,"id":{},"link":"http://localhost:8080/api/texts/{}"}}"#, last_insert_row_id, last_insert_row_id)))
                        .or_else(|_| Requests::internal_server_error_response())
                } else {
                    return Requests::internal_server_error_response();
                }
            }
            Err(err) => {
                Requests::bad_request_response(&format!("{}", err))
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

        (&Method::GET, "/favicon.ico") => Requests::serve_file("pages/favicon.ico", "image/vnd.microsoft.icon"),
        (&Method::GET, path) if !path.starts_with("/api/")  => Requests::serve_page("pages/index.html"),

        (&Method::GET, "/api/texts") => Requests::get_texts(req),
        (&Method::GET, path) if is_single_entry_path(path) => Requests::get_single_text(req),
        (&Method::GET, path) if is_entry_image_path(path) => Requests::get_entry_image(req),
        (&Method::PUT, path) if is_entry_image_path(path) => Requests::put_entry_image(req).await,
        (&Method::POST, "/api/texts") => Requests::post_texts(req).await,

        (&Method::GET, "/api/categories") => Requests::get_categories(),
        (&Method::GET, "/api/authors") => Requests::get_authors(),
        (&Method::GET, "/api/themes") => Requests::get_themes(),
        (&Method::GET, "/api/works") => Requests::get_works(),
        (&Method::GET, "/api/tags") => Requests::get_tags(),

        // Return the 404 Not Found for other routes.
        _ => Requests::not_found_404_response()
    }
}

#[tokio::main]
pub async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    
    let connection = rusqlite::Connection::open(DATABASE_FILENAME)?;

    connection.execute(
        "
        CREATE TABLE IF NOT EXISTS entries (
            entry_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, 
            link TEXT NOT NULL COLLATE NOCASE, 
            title TEXT NOT NULL COLLATE NOCASE,
            description TEXT NOT NULL COLLATE NOCASE,
            author TEXT NOT NULL,
            category TEXT NOT NULL,
            themes TEXT NOT NULL,
            works_mentioned TEXT NOT NULL,
            tags TEXT NOT NULL,
            date_published DATE NOT NULL,
            date_saved DATE NOT NULL,
            exceptional BOOL NOT NULL,
            image BLOB,
            backup BLOB
        );
        ", []
    )?;

    connection.execute("CREATE TABLE IF NOT EXISTS authors (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, value TEXT UNIQUE NOT NULL);", [])?;
    connection.execute("CREATE TABLE IF NOT EXISTS categories (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, value TEXT UNIQUE NOT NULL);", [])?;
    connection.execute("CREATE TABLE IF NOT EXISTS themes (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, value TEXT UNIQUE NOT NULL);", [])?;
    connection.execute("CREATE TABLE IF NOT EXISTS works (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, value TEXT UNIQUE NOT NULL);", [])?;
    connection.execute("CREATE TABLE IF NOT EXISTS tags (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, value TEXT UNIQUE NOT NULL);", [])?;

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
            Some((query, params)) => { 
                assert_eq!(query, r#"SELECT * FROM entries WHERE link LIKE ? LIMIT 10"#);
                assert_eq!(params, ["%wikipedia%"]);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_title_checks_for_containment() {
        let url_params = "title=Hello";
        match url_to_sql_query(url_params) {
            Some((query, params)) => {
                assert_eq!(query, r#"SELECT * FROM entries WHERE title LIKE ? LIMIT 10"#);
                assert_eq!(params, ["%Hello%"]);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_author_checks_for_equality() {
        let url_params = "author=Pauline%20Kael";
        match url_to_sql_query(url_params) {
            Some((query, params)) => {
                assert_eq!(query, r#"SELECT * FROM entries WHERE author = ? LIMIT 10"#);
                assert_eq!(params, ["Pauline Kael"]);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_description_checks_for_containment() {
        let url_params = "description=compiler";
        match url_to_sql_query(url_params) {
            Some((query, params)) => {
                assert_eq!(query, r#"SELECT * FROM entries WHERE description LIKE ? LIMIT 10"#);
                assert_eq!(params, ["%compiler%"]);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_category_checks_for_equality() {
        let url_params = "category=Programming";
        match url_to_sql_query(url_params) {
            Some((query, params)) => {
                assert_eq!(query, r#"SELECT * FROM entries WHERE category = ? LIMIT 10"#);
                assert_eq!(params, ["Programming"]);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_themes_checks_for_containment_of_each() {
        let url_params = "themes=Rust%7CTesting";
        match url_to_sql_query(url_params) {
            Some((query, params)) => {
                assert_eq!(query, r#"SELECT * FROM entries WHERE themes LIKE ? AND themes LIKE ? LIMIT 10"#);
                assert_eq!(params, ["%|Rust|%", "%|Testing|%"]);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_works_checks_for_containment_of_each() {
        let url_params = "works_mentioned=Hamlet%7CMacBeth%7CKing%20Lear";
        match url_to_sql_query(url_params) {
            Some((query, params)) => {
                assert_eq!(query, r#"SELECT * FROM entries WHERE works_mentioned LIKE ? AND works_mentioned LIKE ? AND works_mentioned LIKE ? LIMIT 10"#);
                assert_eq!(params, ["%|Hamlet|%", "%|MacBeth|%", "%|King Lear|%"]);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_tags_checks_for_containment_of_each() {
        let url_params = "tags=Soulslike%7CGreat%20soundtrack%7CFemale%20protagonist";
        match url_to_sql_query(url_params) {
            Some((query, params)) => { 
                assert_eq!(query, r#"SELECT * FROM entries WHERE tags LIKE ? AND tags LIKE ? AND tags LIKE ? LIMIT 10"#);
                assert_eq!(params, ["%|Soulslike|%", "%|Great soundtrack|%", "%|Female protagonist|%"]);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_date_is_formated_as_yyyy_mm_dd() {
        let url_params = "published_between_from=1967-5-3";
        match url_to_sql_query(url_params) {
            Some((query, params)) => { 
                assert_eq!(query, r#"SELECT * FROM entries WHERE date_published >= DATE(?) LIMIT 10"#);
                assert_eq!(params, ["1967-05-03"]);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_published_between_until_checks_for_less_equal() {
        let url_params = "published_between_until=1967-11-24";
        match url_to_sql_query(url_params) {
            Some((query, params)) => { 
                assert_eq!(query, r#"SELECT * FROM entries WHERE date_published <= DATE(?) LIMIT 10"#);
                assert_eq!(params, ["1967-11-24"]);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_bool_true_is_formated_as_uppercase_true() {
        let url_params = "exceptional=true";
        match url_to_sql_query(url_params) {
            Some((query, params)) => { 
                assert_eq!(query, r#"SELECT * FROM entries WHERE exceptional = TRUE LIMIT 10"#);
                assert_eq!(params.is_empty(), true);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_bool_false_is_formated_as_uppercase_false() {
        let url_params = "exceptional=false";
        match url_to_sql_query(url_params) {
            Some((query, params)) => { 
                assert_eq!(query, r#"SELECT * FROM entries WHERE exceptional = FALSE LIMIT 10"#);
                assert_eq!(params.is_empty(), true);
            }
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_two_or_more_predicates_are_anded() {
        let url_params = "link=wikipedia&author=Pauline%20Kael&tags=Soulslike%7CGreat%20soundtrack%7CFemale%20protagonist";
        match url_to_sql_query(url_params) {
            Some((query, params)) => {
                assert_eq!(query, r#"SELECT * FROM entries WHERE link LIKE ? AND author = ? AND tags LIKE ? AND tags LIKE ? AND tags LIKE ? LIMIT 10"#);
                assert_eq!(params, ["%wikipedia%", "Pauline Kael", "%|Soulslike|%", "%|Great soundtrack|%", "%|Female protagonist|%"]);
            } 
            None => unreachable!()
        }
    }

    #[test]
    fn test_url_to_sql_query_quotes_are_escaped() {
        let url_params = "link=%22wikipedia%22";
        match url_to_sql_query(url_params) {
            Some((query, params)) => { 
                assert_eq!(query, r#"SELECT * FROM entries WHERE link LIKE ? LIMIT 10"#);
                assert_eq!(params, [r#"%"wikipedia"%"#]);
            }
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
        assert_eq!(as_sql_array, "|Iruña|");
    }

    #[test]
    fn test_format_as_sql_array_several_elements_are_separated_by_or_bars() {
        let strings = [ String::from("Iruña"), String::from("Bilbo"), String::from("Gasteiz"), String::from("Donostia") ];
        let as_sql_array = format_as_sql_array(&strings);
        assert_eq!(as_sql_array, "|Iruña|Bilbo|Gasteiz|Donostia|");
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

    // is_single_entry_path

    #[test]
    fn is_single_entry_path_correct_paths() {
        assert_eq!(is_single_entry_path("/api/texts/1"), true);
        assert_eq!(is_single_entry_path("/api/texts/215"), true);
        assert_eq!(is_single_entry_path("/api/texts/1845348"), true);
    }

    #[test]
    fn is_single_entry_path_not_starting_with_correct_prefix() {
        assert_eq!(is_single_entry_path("/texts/1"), false);
        assert_eq!(is_single_entry_path("/api/text/215"), false);
        assert_eq!(is_single_entry_path("/foo/bar/baz/1845348"), false);
    }

    #[test]
    fn is_single_entry_path_not_a_number() {
        assert_eq!(is_single_entry_path("/api/texts/hello"), false);
        assert_eq!(is_single_entry_path("/api/texts/five"), false);
        assert_eq!(is_single_entry_path("/api/texts/2.25"), false);
    }

    #[test]
    fn is_single_entry_path_more_subpaths() {
        assert_eq!(is_single_entry_path("/api/texts/1/more_stuff"), false);
        assert_eq!(is_single_entry_path("/api/texts/215/image"), false);
        assert_eq!(is_single_entry_path("/api/texts/1845348/backup"), false);
    }
}
