use std::convert::Infallible;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Request, Response, Server, Method, StatusCode};
use hyper_tls::HttpsConnector;
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
use dirs;
use image;
use image::imageops;

#[macro_use]
extern crate lazy_static;

#[derive(Deserialize)]
struct ConfigFile
{
    database_path : String
}

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
struct Date
{
    day : i32,
    month : Month,
    year : i32
}

impl Ord for Date 
{
    fn cmp(&self, other: &Self) -> Ordering {
        return (self.year, self.month, self.day).cmp(&(other.year, other.month, other.day));
    }
}

impl PartialOrd for Date 
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        return Some(self.cmp(other));
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
enum EntryType
{
    Article { pages : i32 },
    Paper { pages : i32 },
    Book { pages : i32 },
    Video { length_in_seconds : i32 },
    Audio { length_in_seconds : i32 },
}

impl Default for EntryType
{
    fn default() -> Self { EntryType::Article{pages : 0} }
}

fn entry_type_index(t : EntryType) -> i32
{
    match t {
        EntryType::Article{pages : _}           => 0,
        EntryType::Paper{pages : _}             => 1,
        EntryType::Book{pages : _}              => 2,
        EntryType::Video{length_in_seconds : _} => 3,
        EntryType::Audio{length_in_seconds : _} => 4,
    }
}

fn entry_type_metadata(t : EntryType) -> i32
{
    match t {
        EntryType::Article{pages}           => pages,
        EntryType::Paper{pages}             => pages,
        EntryType::Book{pages}              => pages,
        EntryType::Video{length_in_seconds} => length_in_seconds,
        EntryType::Audio{length_in_seconds} => length_in_seconds,
    }
}

fn make_entry_type_from_index_and_metadata(index : i32, metadata : i32) -> EntryType
{
    match index {
        0 => EntryType::Article{pages : metadata},
        1 => EntryType::Paper{pages : metadata},
        2 => EntryType::Book{pages : metadata},
        3 => EntryType::Video{length_in_seconds : metadata},
        4 => EntryType::Audio{length_in_seconds : metadata},
        _ => unreachable!(),
    }
}

#[derive(Deserialize, Debug)]
struct NewEntryForm 
{
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
    entry_type : EntryType
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
struct Entry 
{
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
    entry_type : EntryType,
    image : Option<String>,
    backup : Option<String>
}

#[derive(Deserialize)]
struct ImageLinkForm 
{
    image_url : String
}

#[derive(Deserialize)]
struct BackupLinkForm 
{
    backup_url : String
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

// Returns type index
fn parse_type_query_argument(argument : &str) -> Option<i32>
{
    match argument {
        "article" => Some(0),  
        "paper"   => Some(1),    
        "book"    => Some(2),     
        "video"   => Some(3),    
        "audio"   => Some(4),
        _         => None
    }
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
                    "type" => {
                        result += &format!("entry_type = {}", parse_type_query_argument(key_value[1])?);
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
        let id : i64 = row.get(0)?;
        let entry_type_index : i32 = row.get(12)?;
        let entry_type_metadata : i32 = row.get(13)?;
        
        found_entries.push(Entry{
            id : id,
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
            entry_type : make_entry_type_from_index_and_metadata(entry_type_index, entry_type_metadata),
            image : if row.get_ref(14)?.as_blob_or_null()?.is_some() { Some(format!("http://localhost:8080/api/texts/{}/image", id)) } else { None },
            backup : if row.get_ref(15)?.as_blob_or_null()?.is_some() { Some(format!("http://localhost:8080/api/texts/{}/backup", id)) } else { None },
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

fn is_entry_subpath(path : &str, subpath : &str) -> bool 
{
    if let Some(p) = path.strip_prefix("/api/texts/") {
        let parts = p.split("/").collect::<Vec<_>>();
        if parts.len() != 2 {
            return false;
        }
        
        return parts[0].parse::<i64>().is_ok() && parts[1] == subpath;
    }

    return false;
}

fn is_entry_image_path(path : &str) -> bool 
{
    return is_entry_subpath(path, "image");
}

fn is_entry_backup_path(path : &str) -> bool 
{
    return is_entry_subpath(path, "backup");
}

fn get_entry_id_from_path(path : &str) -> i64 {
    if let Some(p) = path.strip_prefix("/api/texts/") {
        let parts = p.split("/").collect::<Vec<_>>();
        return parts[0].parse::<i64>().unwrap();
    }

    unreachable!();
}

fn get_header_case_insensitive<'a>(headers: &'a hyper::HeaderMap, target_header: &str) -> Option<&'a hyper::header::HeaderValue> 
{
    let target_lowercase = target_header.to_lowercase();
    return headers.iter()
        .find(|(name, _)| name.as_str().to_lowercase() == target_lowercase)
        .map(|(_, value)| value);
}

async fn make_http_request(request : hyper::Request<hyper::Body>) -> hyper::Result<hyper::Response<hyper::Body>>
{
    let https = HttpsConnector::new();
    let client = hyper::Client::builder().build::<_, hyper::Body>(https);

    return client.request(request).await;
}

async fn make_get_http_request(url : &str) -> hyper::Result<hyper::Response<hyper::Body>>
{
    let request = hyper::Request::builder()
        .method(hyper::Method::GET)
        .uri(url)
        .header("user-agent", "localhost/0.1.0")
        .body(hyper::Body::from(""))
        .unwrap();

    return make_http_request(request).await;
}

fn get_value_between_quotes(source : &str) -> Option<&str>
{
    source
        .split_once("=\"")
        .map(|(_, after_first_quote)| after_first_quote)
        .and_then(|s| s.split_once('"'))
        .map(|(before_second_quote, _)| before_second_quote)
}

fn get_name_and_value_of_meta_string(source : &str) -> Option<(&str, &str)>
{
    let name_index = source.find("name=\"").or_else(|| source.find("property=\""))?;
    let content_index = source.find("content=\"")?;
    let name = get_value_between_quotes(source.split_at(name_index).1)?;
    let content = get_value_between_quotes(source.split_at(content_index).1)?;
    return Some((name, content));
}

fn html_meta_headers(html_source : &str) -> Vec<(&str, &str)>
{
    let mut headers = Vec::new();

    // Skip the first because that is the part before the first meta
    for part in html_source.split("<meta ").skip(1) {
        if let Some(meta) = get_name_and_value_of_meta_string(part) {
            headers.push(meta);
        }
    }  

    return headers;
}

fn scaled_width_and_height(width : u32, height : u32, target_width : u32, target_height : u32) -> (u32, u32, u32, u32)
{
    let width_left_full : (u32, u32) = (target_width, (target_width * height) / width);
    let height_left_full : (u32, u32) = ((target_height * width) / height, target_height);

    if width_left_full.1 >= target_height {
        return (width_left_full.0, width_left_full.1, 0, (width_left_full.1 - target_height) / 2);
    } else {
        return (height_left_full.0, height_left_full.1, (width_left_full.0 - target_width) / 2, 0);
    }
}

// Return the image in png format, with 8 bit rgba component pixels (32 bits per pixel), and size 300x169
fn normalize_image(image_bytes : hyper::body::Bytes) -> image::ImageResult<Vec<u8>>
{
    let image_reader = image::io::Reader::new(std::io::Cursor::new(image_bytes))
        .with_guessed_format()
        .expect("Cursor IO never fails.");

    let image = image_reader.decode()?;

    let rgba8_image = image.to_rgba8();

    let (width, height) = rgba8_image.dimensions();
    let (scaled_width, scaled_height, crop_offset_x, crop_offset_y) = scaled_width_and_height(width, height, 300, 169);

    let resized_image = imageops::resize(&rgba8_image, scaled_width, scaled_height, imageops::FilterType::CatmullRom);

    let cropped_image = imageops::crop_imm(&resized_image, crop_offset_x, crop_offset_y, 300, 169).to_image();

    let mut png_encoded_image = Vec::new();

    cropped_image.write_to(&mut std::io::Cursor::new(&mut png_encoded_image), image::ImageOutputFormat::Png)?;

    return Ok(png_encoded_image);
}

lazy_static! 
{
    static ref STATE: Mutex<State> = Mutex::new(State::new());
}

struct Requests{}
impl Requests
{
    fn options() -> Result<Response<Body>, hyper::Error>
    {
        println!("Options!");
        return Response::builder()
            .status(StatusCode::NO_CONTENT)
            .header("Allow", "OPTIONS, GET, PUT, POST, DELETE")
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .header("Access-Control-Allow-Methods", "OPTIONS, GET, PUT, POST, DELETE")
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

    async fn put_single_text(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let entry_id = get_entry_id_from_path(req.uri().path());

        let whole_body = hyper::body::to_bytes(req.into_body()).await?;
        match serde_json::from_slice(&whole_body) as Result<NewEntryForm, serde_json::Error> {
            Ok(form) => {
                let state = STATE.lock().unwrap();

                if let Some(database) = &state.database {
                    let result = database.execute(
                        "
                        UPDATE entries
                        SET link = ?1,
                            title = ?2,
                            description = ?3,
                            author = ?4,
                            category = ?5,
                            themes = ?6,
                            works_mentioned = ?7,
                            tags = ?8,
                            date_published = ?9,
                            date_saved = ?10,
                            exceptional = ?11,
                            entry_type = ?12,
                            entry_type_metadata = ?13
                        WHERE entry_id = ?14;
                        ",
                        rusqlite::params!
                            [ &form.link
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
                            , entry_type_index(form.entry_type)
                            , entry_type_metadata(form.entry_type)
                            , entry_id
                            ]
                    );

                    if let Err(err) = result {
                        println!("Entry update failed: {}", err);
                        return Requests::internal_server_error_response();
                    }

                    Response::builder()
                        .status(StatusCode::NO_CONTENT)
                        .header("Access-Control-Allow-Origin", "*")
                        .header("Access-Control-Allow-Headers", "*")
                        .body(Body::from(""))
                        .or_else(|_| Requests::internal_server_error_response())
                } else {
                    Requests::internal_server_error_response()
                }
            },
            Err(err) => Requests::bad_request_response(&format!("{}", err))
        }
    }

    fn delete_single_text(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let entry_id = get_entry_id_from_path(req.uri().path());

        let state = STATE.lock().unwrap();
        let database = state.database.as_ref().unwrap();

        let result = database.execute(
            "
            DELETE FROM entries
            WHERE entry_id = ?
            ",
            rusqlite::params![entry_id]
        );

        if let Err(err) = result {
            println!("Entry delete failed: {}", err);
            return Requests::not_found_404_response();
        }

        Response::builder()
            .status(StatusCode::NO_CONTENT)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .body(Body::from(""))
            .or_else(|_| Requests::internal_server_error_response())
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

        let is_content_in_body : bool = match get_header_case_insensitive(req.headers(), "Content-Type") {
            None => { return Requests::bad_request_response("Missing content type hader"); }
            Some(value) => match value.to_str().unwrap() {
                // TO DO: Save image type.
                "image/png" => true,
                "image/jpeg" => true,
                "image/gif" => true,
                "image/bmp" => true,
                "application/json" => false,
                other => { return Requests::bad_request_response(&format!("Unsuported content type: {}", other)); }
            }
        };

        let whole_body = hyper::body::to_bytes(req.into_body()).await?;

        let image_bytes = if is_content_in_body {
            whole_body
        } else {
            match serde_json::from_slice(&whole_body) as Result<ImageLinkForm, serde_json::Error> {
                Ok(form) => { 
                    let response = match make_get_http_request(&form.image_url).await {
                        Ok(r) => r,
                        Err(err) => {
                            println!("Request failed: {}", err);
                            Err(err)?
                        }
                    };

                    match get_header_case_insensitive(response.headers(), "Content-Type") {
                        None => { return Requests::bad_request_response(&format!("Missing content type header at link {}.", &form.image_url)); }
                        Some(value) => match value.to_str().unwrap() {
                            // TO DO: Save image type.
                            "image/png" => (),
                            "image/jpeg" => (),
                            "image/gif" => (),
                            "image/bmp" => (),
                            other => { return Requests::bad_request_response(&format!("Unsuported content type {} at link {}", other, &form.image_url)); }
                        }
                    }

                    hyper::body::to_bytes(response.into_body()).await?
                }
                Err(err) => { return Requests::bad_request_response(&format!("{}", err));  }
            }
        };

        let normalized_image_bytes = match normalize_image(image_bytes) {
            Ok(x) => x,
            Err(err) => return Requests::bad_request_response(&format!("Body is not a valid image: {}", err))
        };

        let state = STATE.lock().unwrap();
        let database = state.database.as_ref().unwrap();
        match database.execute(
            "
            UPDATE entries
            SET image = ?
            WHERE entry_id = ?
            ",
            rusqlite::params![rusqlite::blob::ZeroBlob(normalized_image_bytes.len() as i32), entry_id]
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

        if let Err(_) = blob.write_at(&normalized_image_bytes, 0) {
            return Requests::internal_server_error_response();
        };

        Response::builder()
            .status(StatusCode::OK)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .header("Content-Type", "application/json")
            .body(Body::from(format!(r#"{{"link":"http://localhost:8080/api/texts/{}/image"}}"#, entry_id)))
            .or_else(|_| Requests::internal_server_error_response())
    }

    fn delete_entry_image(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let entry_id = get_entry_id_from_path(req.uri().path());

        let state = STATE.lock().unwrap();
        let database = state.database.as_ref().unwrap();

        let result = database.execute(
            "
            UPDATE entries
            SET image = NULL
            WHERE entry_id = ?
            ",
            rusqlite::params![entry_id]
        );

        if let Err(err) = result {
            println!("Image delete failed: {}", err);
            return Requests::not_found_404_response();
        }

        Response::builder()
            .status(StatusCode::NO_CONTENT)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .body(Body::from(""))
            .or_else(|_| Requests::internal_server_error_response())
    }

    fn get_entry_backup(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let entry_id = get_entry_id_from_path(req.uri().path());

        let state = STATE.lock().unwrap();
        let database = state.database.as_ref().unwrap();

        let mut statement = match database.prepare("SELECT backup FROM entries WHERE entry_id = ?") {
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
                let content_type_length = blob[0] as usize;
                let content_type = &blob[1..content_type_length + 1];
                let content_data = Vec::from(&blob[content_type_length + 1..]);

                Response::builder()
                    .status(StatusCode::OK)
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "*")
                    .header("Content-Type", content_type)
                    .body(Body::from(content_data))
                    .or_else(|_| Requests::internal_server_error_response())
            } else {
                return Requests::not_found_404_response();
            }
        } else {
            return Requests::not_found_404_response();
        }
    }

    fn write_entry_backup_to_database(entry_id : i64, body : hyper::body::Bytes, content_type : &str) -> Result<Response<Body>, hyper::Error>
    {
        let blob_header_length = content_type.len() + 1; // + 1 for storing the size.
        let blob_length = blob_header_length + body.len();

        let state = STATE.lock().unwrap();
        let database = state.database.as_ref().unwrap();
        match database.execute(
            "
            UPDATE entries
            SET backup = ?
            WHERE entry_id = ?
            ",
            rusqlite::params![rusqlite::blob::ZeroBlob(blob_length as i32), entry_id]
        ) {
            // No values where modified.
            Ok(0) => { return Requests::not_found_404_response(); }
            // Something went wrong within the database.
            Err(_) => { return Requests::internal_server_error_response(); }
            // Everything went fine.
            Ok(_) => ()
        };

        let mut blob = match database.blob_open(rusqlite::MAIN_DB, "entries", "backup", entry_id, false) {
            Ok(b) => b,
            Err(_) => { return Requests::internal_server_error_response(); }
        };

        // Write the length of the content type string in 1 byte. Content type strings are very short
        // so 1 byte should always be enough.
        if let Err(_) = blob.write_at(&[content_type.len() as u8], 0) {
            return Requests::internal_server_error_response();
        };

        // Write the content type string. This way, when a client requests the backup, we can return it with
        // the correct content type.
        if let Err(_) = blob.write_at(content_type.as_bytes(), 1) {
            return Requests::internal_server_error_response();
        };

        // Write the actual backup data.
        if let Err(_) = blob.write_at(&body, blob_header_length) {
            return Requests::internal_server_error_response();
        };

        Response::builder()
            .status(StatusCode::OK)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .header("Content-Type", "application/json")
            .body(Body::from(format!(r#"{{"link":"http://localhost:8080/api/texts/{}/backup"}}"#, entry_id)))
            .or_else(|_| Requests::internal_server_error_response())
    }

    async fn put_entry_backup(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let entry_id = get_entry_id_from_path(req.uri().path());

        let content_type = match get_header_case_insensitive(req.headers(), "Content-Type") {
            None => { return Requests::bad_request_response("Missing content type header"); }
            Some(value) => String::from(value.to_str().unwrap())
        };

        let whole_body = hyper::body::to_bytes(req.into_body()).await?;

        if content_type == "application/json" {
            if let Ok(form) = serde_json::from_slice(&whole_body) as Result<BackupLinkForm, serde_json::Error> {
                let response = match make_get_http_request(&form.backup_url).await {
                    Ok(x) => x,
                    Err(err) => return Requests::bad_request_response(&format!("Request to url {} failed: {}", form.backup_url, err))
                };
                
                if response.status() != StatusCode::OK {
                    return Requests::bad_request_response(&format!("Could not get resource at url {}", form.backup_url));
                }
                
                let response_content_type = match get_header_case_insensitive(response.headers(), "Content-Type") {
                    None => { return Requests::bad_request_response(&format!("Missing content type header in resource at url {}", form.backup_url)); }
                    Some(value) => String::from(value.to_str().unwrap())
                };

                let response_body = hyper::body::to_bytes(response.into_body()).await?;

                return Requests::write_entry_backup_to_database(entry_id, response_body, &response_content_type)
            }
        }

        return Requests::write_entry_backup_to_database(entry_id, whole_body, &content_type)
    }

    fn delete_entry_backup(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let entry_id = get_entry_id_from_path(req.uri().path());

        let state = STATE.lock().unwrap();
        let database = state.database.as_ref().unwrap();

        let result = database.execute(
            "
            UPDATE entries
            SET backup = NULL
            WHERE entry_id = ?
            ",
            rusqlite::params![entry_id]
        );

        if let Err(err) = result {
            println!("Backup delete failed: {}", err);
            return Requests::not_found_404_response();
        }

        Response::builder()
            .status(StatusCode::NO_CONTENT)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .body(Body::from(""))
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
                        INSERT INTO entries (link, title, description, author, category, themes, works_mentioned, tags, date_published, date_saved, exceptional, entry_type, entry_type_metadata)
                        VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13);
                        ",
                        rusqlite::params!
                            [ &form.link
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
                            , entry_type_index(form.entry_type)
                            , entry_type_metadata(form.entry_type)
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
                        .body(Body::from(format!(r#"{{"id":{},"link":"http://localhost:8080/api/texts/{}"}}"#, last_insert_row_id, last_insert_row_id)))
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
        return Requests::serve_file(path, "text/html; charset=utf-8", false);
    }

    fn serve_file(path : &str, content_type : &str, cache : bool) -> Result<Response<Body>, hyper::Error>
    {
        match fs::read(path) {
            Ok(content) =>{
                Response::builder()
                    .status(StatusCode::OK)
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "*")
                    .header("Cache-Control", if cache { "public, max-age=31919000, immutable" } else { "max-age=0" })
                    .header("Content-Type", content_type)
                    .body(Body::from(content))
                    .or_else(|_| Requests::internal_server_error_response())
            }
            Err(_) => Requests::internal_server_error_response()
        }
    }

    async fn forward_get_request(url : &str) -> Result<Response<Body>, hyper::Error>
    {
        let mut response = make_get_http_request(url).await?;

        response.headers_mut().insert("Access-Control-Allow-Origin", hyper::header::HeaderValue::from_static("*"));
        response.headers_mut().insert("Access-Control-Allow-Headers", hyper::header::HeaderValue::from_static("*"));

        return Ok(response);
    }

    async fn get_meta_headers_at_url(request_uri : &hyper::Uri) -> Result<Response<Body>, hyper::Error>
    {
        let url = {
            let mut url = String::from(request_uri.path().strip_prefix("/api/meta_headers/").unwrap());
            if let Some(query) = request_uri.query() {
                url += "?";
                url += query;
            }
            url
        };

        println!("Url: {}", url);

        let response = match make_get_http_request(&url).await {
            Ok(r) => r,
            Err(_) => return Requests::not_found_404_response()
        };

        println!("Request succesful");

        // If the requested resource is not able to return a succesful response, return a 404.
        if response.status() != StatusCode::OK {
            println!("Response status: {}", response.status().as_u16());
            return Requests::not_found_404_response();
        }

        println!("Response is OK");

        // If the requested resource is not html, return a 404.
        let content_type_header = get_header_case_insensitive(response.headers(), "Content-Type");

        println!("{}", content_type_header.map(|x| x.to_str().unwrap()).unwrap_or("No content type"));

        if !content_type_header.map_or(false, |h| h.to_str().unwrap().starts_with("text/html")) {
            return Requests::not_found_404_response();
        }

        println!("Content type is text/html");

        let whole_body = hyper::body::to_bytes(response.into_body()).await?;
        let whole_text = match String::from_utf8(whole_body.to_vec()) {
            Ok(x) => x,
            Err(_) => return Requests::internal_server_error_response()
        };

        println!("Got body");

        let headers = html_meta_headers(&whole_text);

        for (name, value) in &headers {
            println!("Header: {} {}", name, value);
        }

        if let Ok(json) = serde_json::to_string(&headers) {
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
}

async fn process_request(req : Request<Body>) -> Result<Response<Body>, hyper::Error> 
{
    match req.uri().query() {
        Some(query_text) => { println!("{} request at path {} with query {}", req.method(), req.uri().path(), query_text); }
        None             => { println!("{} request at path {}", req.method(), req.uri().path()); }
    };

    match (req.method(), req.uri().path()) {
        (&Method::OPTIONS, _) => Requests::options(),

        (&Method::GET, "/favicon.ico") => Requests::serve_file("pages/favicon.ico", "image/vnd.microsoft.icon", true),
        (&Method::GET, path) if path.starts_with("/fontawesome/") => Requests::serve_file(
            &format!("pages/{}", path), 
            if path.ends_with(".css") { "text/css" } else { "font/ttf" },
            true),
        (&Method::GET, path) if !path.starts_with("/api/")  => Requests::serve_page("pages/index.html"),

        (&Method::GET, "/api/texts") => Requests::get_texts(req),
        (&Method::GET, path) if is_single_entry_path(path) => Requests::get_single_text(req),
        (&Method::PUT, path) if is_single_entry_path(path) => Requests::put_single_text(req).await,
        (&Method::DELETE, path) if is_single_entry_path(path) => Requests::delete_single_text(req),
        (&Method::GET, path) if is_entry_image_path(path) => Requests::get_entry_image(req),
        (&Method::PUT, path) if is_entry_image_path(path) => Requests::put_entry_image(req).await,
        (&Method::DELETE, path) if is_entry_image_path(path) => Requests::delete_entry_image(req),
        (&Method::GET, path) if is_entry_backup_path(path) => Requests::get_entry_backup(req),
        (&Method::PUT, path) if is_entry_backup_path(path) => Requests::put_entry_backup(req).await,
        (&Method::DELETE, path) if is_entry_backup_path(path) => Requests::delete_entry_backup(req),
        (&Method::POST, "/api/texts") => Requests::post_texts(req).await,

        (&Method::GET, "/api/categories") => Requests::get_categories(),
        (&Method::GET, "/api/authors") => Requests::get_authors(),
        (&Method::GET, "/api/themes") => Requests::get_themes(),
        (&Method::GET, "/api/works") => Requests::get_works(),
        (&Method::GET, "/api/tags") => Requests::get_tags(),

        (&Method::GET, path) if path.starts_with("/api/forward/") => Requests::forward_get_request(path.strip_prefix("/api/forward/").unwrap()).await,
        (&Method::GET, path) if path.starts_with("/api/meta_headers/") => Requests::get_meta_headers_at_url(req.uri()).await,

        // Return the 404 Not Found for other routes.
        _ => Requests::not_found_404_response()
    }
}

fn load_config_file<P : AsRef<std::path::Path>>(path : P) -> Result<ConfigFile, Box<dyn std::error::Error + Send + Sync>>
{
    Ok(serde_json::from_str(&fs::read_to_string(path)?)?)
}

#[tokio::main]
pub async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> 
{
    let config_path = match dirs::home_dir() {
        Some(dir) => { let mut osstr = dir.into_os_string(); osstr.push("/.localhost.json"); osstr },
        None => {
            println!("Could not find user directory in the system. The program will now close.");
            return Ok(());
        }
    };
    
    println!("Config path: {}", config_path.to_str().unwrap());
    
    let config = load_config_file(config_path)?;

    println!("Loading database at: {}", config.database_path);

    let connection = rusqlite::Connection::open(config.database_path)?;

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
            entry_type INT NOT NULL,
            entry_type_metadata INT NOT NULL,
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

    #[test]
    fn test_url_to_sql_query_type_is_converted_to_an_index() {
        {
            let url_params = "type=article";
            match url_to_sql_query(url_params) {
                Some((query, params)) => { 
                    assert_eq!(query, r#"SELECT * FROM entries WHERE entry_type = 0 LIMIT 10"#);
                    assert_eq!(params.is_empty(), true);
                }
                None => unreachable!()
            }
        }
        {
            let url_params = "type=paper";
            match url_to_sql_query(url_params) {
                Some((query, params)) => { 
                    assert_eq!(query, r#"SELECT * FROM entries WHERE entry_type = 1 LIMIT 10"#);
                    assert_eq!(params.is_empty(), true);
                }
                None => unreachable!()
            }
        }
        {
            let url_params = "type=book";
            match url_to_sql_query(url_params) {
                Some((query, params)) => { 
                    assert_eq!(query, r#"SELECT * FROM entries WHERE entry_type = 2 LIMIT 10"#);
                    assert_eq!(params.is_empty(), true);
                }
                None => unreachable!()
            }
        }
        {
            let url_params = "type=video";
            match url_to_sql_query(url_params) {
                Some((query, params)) => { 
                    assert_eq!(query, r#"SELECT * FROM entries WHERE entry_type = 3 LIMIT 10"#);
                    assert_eq!(params.is_empty(), true);
                }
                None => unreachable!()
            }
        }
        {
            let url_params = "type=audio";
            match url_to_sql_query(url_params) {
                Some((query, params)) => { 
                    assert_eq!(query, r#"SELECT * FROM entries WHERE entry_type = 4 LIMIT 10"#);
                    assert_eq!(params.is_empty(), true);
                }
                None => unreachable!()
            }
        }
    }

    #[test]
    fn test_url_to_sql_query_bad_type() {
        let url_params = "type=snafucated";
        match url_to_sql_query(url_params) {
            Some(_) => unreachable!(),
            None => () 
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
