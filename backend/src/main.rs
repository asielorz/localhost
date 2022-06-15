use std::convert::Infallible;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Request, Response, Server, Method, StatusCode};
use serde_json;
use serde::{Serialize, Deserialize};
use std::fs;
use std::fs::File;
use std::io::Write;
use std::sync::Mutex;
use std::collections::HashSet;
use chrono;
use chrono::Datelike;
use percent_encoding::percent_decode_str;
use std::cmp::{Ord, Ordering};
use std::env;

#[macro_use]
extern crate lazy_static;

#[derive(Serialize, Deserialize, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum Month {
    January, February, March, April, May, June, July, August, September, October, November, December
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

#[derive(Serialize, Deserialize, Debug, Copy, Clone, Eq, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, Clone)]
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

struct Query {
    link : String,
    title : String,
    description : String,
    author : String,
    category : String,
    themes : Vec<String>,
    works_mentioned : Vec<String>,
    tags : Vec<String>,
    published_between_from : Option<Date>,
    published_between_until : Option<Date>,
    saved_between_from : Option<Date>,
    saved_between_until : Option<Date>,
    exceptional : bool,
}

fn parse_string_query_argument(query_argument : &str) -> Option<String>
{
    match percent_decode_str(query_argument).decode_utf8() {
        Ok(result) => Some(String::from(result.as_ref())),
        Err(_) => None
    }
}

fn parse_string_list_query_argument(query_argument : &str) -> Option<Vec<String>>
{
    let mut strings : Vec<String> = Vec::new();

    for s in query_argument.split("%7C") {
        strings.push(parse_string_query_argument(s)?);
    }

    return Some(strings);
}

fn parse_date_query_argument(query_argument : &str) -> Option<Date>
{
    let elements : Vec<_> = query_argument.split("-").collect();
    if elements.len() != 3 {
        return None;
    }

    let day = match elements[0].parse::<i32>() {
        Ok(day) => {
            if day >= 1 && day <= 31 {
                day
            } else {
                return None
            }
        },
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

    let year = match elements[2].parse::<i32>() {
        Ok(year) => year,
        Err(_) => return None
    };

    return Some(Date {
        day : day,
        month : month,
        year : year
    });
}

fn parse_query(query_text : &str) -> Option<Query>
{
    let mut query = Query{
        link : String::new(),
        title : String::new(),
        description : String::new(),
        author : String::new(),
        category : String::new(),
        themes : Vec::new(),
        works_mentioned : Vec::new(),
        tags : Vec::new(),
        published_between_from : None,
        published_between_until : None,
        saved_between_from : None,
        saved_between_until : None,
        exceptional : false,
    };

    for query_argument in query_text.split("&")
    {
        let key_value : Vec<_> = query_argument.split("=").collect();
        if key_value.len() == 2 {
            match key_value[0] {
                "link" => query.link = parse_string_query_argument(key_value[1])?,
                "title" => query.title = parse_string_query_argument(key_value[1])?,
                "author" => query.author = parse_string_query_argument(key_value[1])?,
                "description" => query.description = parse_string_query_argument(key_value[1])?,
                "category" => query.category = parse_string_query_argument(key_value[1])?,
                "works_mentioned" => query.works_mentioned = parse_string_list_query_argument(key_value[1])?,
                "themes" => query.themes = parse_string_list_query_argument(key_value[1])?,
                "tags" => query.tags = parse_string_list_query_argument(key_value[1])?,
                "published_between_from" => query.published_between_from = Some(parse_date_query_argument(key_value[1])?),
                "published_between_until" => query.published_between_until = Some(parse_date_query_argument(key_value[1])?),
                "saved_between_from" => query.saved_between_from = Some(parse_date_query_argument(key_value[1])?),
                "saved_between_until" => query.saved_between_until = Some(parse_date_query_argument(key_value[1])?),
                "exceptional" => query.exceptional = true,
                _ => ()
            }
        }
    }

    return Some(query);
}

fn search_words_query_filter(query_string : &str, text : &str) -> bool
{
    let query_string_lowercase = query_string.to_lowercase();
    let text_lowercase = text.to_lowercase();

    for word in query_string_lowercase.split(" ") {
        if text_lowercase.contains(word) {
            return true;
        }
    }

    return false;
}

fn passes_query_filter(query : &Query, entry : &Entry) -> bool
{
    if !query.link.is_empty() && !entry.link.to_lowercase().contains(&query.link.to_lowercase()) {
        return false;
    }
    
    if !query.title.is_empty() && !search_words_query_filter(&query.title, &entry.title) {
        return false;
    }

    if !query.author.is_empty() && query.author != entry.author {
        return false;
    }

    if !query.category.is_empty() && query.category != entry.category {
        return false;
    }

    if !query.description.is_empty() && !search_words_query_filter(&query.description, &entry.description) {
        return false;
    }

    if !query.works_mentioned.is_empty() {
        for work in &query.works_mentioned {
            if !entry.works_mentioned.contains(&work) {
                return false;
            }
        }
    }

    if !query.themes.is_empty() {
        for work in &query.themes {
            if !entry.themes.contains(&work) {
                return false;
            }
        }
    }

    if !query.tags.is_empty() {
        for work in &query.tags {
            if !entry.tags.contains(&work) {
                return false;
            }
        }
    }

    if let Some(date) = query.published_between_from {
        if entry.date_published < date {
            return false;
        }
    }

    if let Some(date) = query.published_between_until {
        if entry.date_published > date {
            return false;
        }
    }

    if let Some(date) = query.saved_between_from {
        if entry.date_saved < date {
            return false;
        }
    }

    if let Some(date) = query.saved_between_until {
        if entry.date_saved > date {
            return false;
        }
    }

    if query.exceptional && !entry.exceptional {
        return false;
    }

    return true;
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

fn make_entry(form : &NewEntryForm) -> Entry
{
    return Entry{
        id : 0,
        link : form.link.clone(),
        title : form.title.clone(),
        description : form.description.clone(),
        author : form.author.clone(),
        category : form.category.clone(),
        themes : form.themes.clone(),
        works_mentioned : form.works_mentioned.clone(),
        tags : form.tags.clone(),
        date_published : form.date_published,
        date_saved : today(),
        exceptional : form.exceptional,
    };
}

struct Cache
{
    categories : HashSet<String>,
    authors : HashSet<String>,
    themes : HashSet<String>,
    works : HashSet<String>,
    tags : HashSet<String>,
}

impl Cache
{
    fn new() -> Cache
    {
        return Cache{ categories : HashSet::new(), authors : HashSet::new(), themes : HashSet::new(), works : HashSet::new(), tags : HashSet::new() }; 
    }
}

struct State
{
    entries: Vec<Entry>,
    cache : Cache
}

impl State
{
    fn new() -> State
    {
        return State{ entries : Vec::new(), cache : Cache::new() };
    }

    fn initialize(&mut self, entries : Vec<Entry>)
    {
        self.cache = cache_all_search_values(&entries);
        self.entries = entries;
    }

    fn add_entry(&mut self, entry : Entry) -> ()
    {
        cache_search_values_for(&mut self.cache, &entry);
        self.entries.push(entry);
    }
}

fn cache_search_values_for(cache : &mut Cache, entry : &Entry) -> ()
{
    cache.categories.insert(entry.category.clone());
    cache.authors.insert(entry.author.clone());

    for theme in &entry.themes {
        cache.themes.insert(theme.clone());
    }

    for work in &entry.works_mentioned {
        cache.works.insert(work.clone());
    }

    for tag in &entry.tags {
        cache.tags.insert(tag.clone());
    }
}

fn cache_all_search_values(entries : &[Entry]) -> Cache
{
    let mut cache = Cache::new();

    for entry in entries {
        cache_search_values_for(&mut cache, &entry);
    }

    return cache;
}

lazy_static! 
{
    static ref STATE: Mutex<State> = Mutex::new(State::new());
}

// TO DO: Parameterize
static FILENAME : &str = "./target/entries.json";

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
        match req.uri().query() {
            Some(query_text) => {
                if let Some(query) = parse_query(query_text) {
                    let state = STATE.lock().unwrap();
                    let mut filtered : Vec<Entry> = Vec::new();
                    for entry in state.entries.iter().filter(|entry| passes_query_filter(&query, entry)) {
                        filtered.push(entry.clone());
                    }
                    Requests::entries_as_http_response(&filtered)
                }
                else {
                    Requests::internal_server_error_response()
                }
            },
            None => Requests::entries_as_http_response(&STATE.lock().unwrap().entries)
        }
    }

    fn strings_as_http_response(strings : &HashSet<String>) -> Result<Response<Body>, hyper::Error>
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
        return Requests::strings_as_http_response(&STATE.lock().unwrap().cache.categories);
    }

    fn get_authors() -> Result<Response<Body>, hyper::Error>
    {
        return Requests::strings_as_http_response(&STATE.lock().unwrap().cache.authors);
    }

    fn get_themes() -> Result<Response<Body>, hyper::Error>
    {
        return Requests::strings_as_http_response(&STATE.lock().unwrap().cache.themes);
    }

    fn get_works() -> Result<Response<Body>, hyper::Error>
    {
        return Requests::strings_as_http_response(&STATE.lock().unwrap().cache.works);
    }

    fn get_tags() -> Result<Response<Body>, hyper::Error>
    {
        return Requests::strings_as_http_response(&STATE.lock().unwrap().cache.tags);
    }

    async fn post_texts(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
    {
        let whole_body = hyper::body::to_bytes(req.into_body()).await?;
        match serde_json::from_slice(&whole_body) as Result<NewEntryForm, serde_json::Error> {
            Ok(form) => {
                let new_entry = make_entry(&form);
                let mut state = STATE.lock().unwrap();
                state.add_entry(new_entry);

                if let Ok(mut file) = File::create(FILENAME) {
                    if let Ok(json) = serde_json::to_vec_pretty(&*state.entries) {
                        _ = file.write_all(&json);
                        println!("Request succesful!");
                    }
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

        (&Method::GET, "/texts") => Requests::get_texts(req),
        
        (&Method::POST, "/texts") => Requests::post_texts(req).await,

        (&Method::GET, "/categories") => Requests::get_categories(),
        (&Method::GET, "/authors") => Requests::get_authors(),
        (&Method::GET, "/themes") => Requests::get_themes(),
        (&Method::GET, "/works") => Requests::get_works(),
        (&Method::GET, "/tags") => Requests::get_tags(),

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
    
    match fs::read_to_string(FILENAME) {
        Ok(content) =>{
            match serde_json::from_slice(content.as_bytes()) as Result<Vec<Entry>, _> {
                Ok(read_entries) => STATE.lock().unwrap().initialize(read_entries),
                Err(err) => println!("Error when parsing entries from file {}: {}", FILENAME, err)
            }
        }
        Err(err) => println!("Could not open file {}: {}", FILENAME, err)
    };

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
