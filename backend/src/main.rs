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

#[macro_use]
extern crate lazy_static;

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
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

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
struct Date {
    day : i32,
    month : Month,
    year : i32
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

#[derive(Serialize, Deserialize, Debug)]
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
        return Ok(Response::builder()
            .status(StatusCode::NO_CONTENT)
            .header("Allow", "OPTIONS, POST")
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .body(Body::from(""))
            .unwrap()
        );
    }

    fn get_texts() -> Result<Response<Body>, hyper::Error>
    {
        let state = STATE.lock().unwrap();

        if let Ok(json) = serde_json::to_string(&*state.entries)
        {
            return Ok(Response::builder()
                .status(StatusCode::OK)
                .header("Allow", "OPTIONS, POST")
                .header("Access-Control-Allow-Origin", "*")
                .header("Access-Control-Allow-Headers", "*")
                .body(Body::from(json))
                .unwrap()
            );
        }
        else
        {
            return Ok(Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .header("Allow", "OPTIONS, POST")
                .header("Access-Control-Allow-Origin", "*")
                .header("Access-Control-Allow-Headers", "*")
                .body(Body::from(""))
                .unwrap()
            );
        }
    }

    fn strings_as_http_response(strings : &HashSet<String>) -> Result<Response<Body>, hyper::Error>
    {
        if let Ok(json) = serde_json::to_string(strings)
        {
            return Ok(Response::builder()
                .status(StatusCode::OK)
                .header("Allow", "OPTIONS, POST")
                .header("Access-Control-Allow-Origin", "*")
                .header("Access-Control-Allow-Headers", "*")
                .body(Body::from(json))
                .unwrap()
            );
        }
        else
        {
            return Ok(Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .header("Allow", "OPTIONS, POST")
                .header("Access-Control-Allow-Origin", "*")
                .header("Access-Control-Allow-Headers", "*")
                .body(Body::from(""))
                .unwrap()
            );
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

                Ok(
                    Response::builder()
                        .status(StatusCode::OK)
                        .header("Allow", "OPTIONS, POST")
                        .header("Access-Control-Allow-Origin", "*")
                        .header("Access-Control-Allow-Headers", "*")
                        .body(Body::from(r#"{"success":"true"}"#))
                        .unwrap()
                )
            }
            Err(err) => {
                println!("Rejecting bad request: {}", err);
                
                Ok(Response::builder()
                    .status(StatusCode::BAD_REQUEST)
                    .header("Allow", "OPTIONS, POST")
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "*")
                    .body(Body::from(format!("Invalid entry: {}", err)))
                    .unwrap()
                )
            }
        }
    }
}

async fn process_request(req : Request<Body>) -> Result<Response<Body>, hyper::Error> 
{
    println!("Processing {} request at path {}", req.method(), req.uri().path());

    match (req.method(), req.uri().path()) {
        (&Method::OPTIONS, _) => Requests::options(),

        (&Method::GET, "/texts") => Requests::get_texts(),
        
        (&Method::POST, "/texts") => Requests::post_texts(req).await,

        (&Method::GET, "/categories") => Requests::get_categories(),
        (&Method::GET, "/authors") => Requests::get_authors(),
        (&Method::GET, "/themes") => Requests::get_themes(),
        (&Method::GET, "/works") => Requests::get_works(),
        (&Method::GET, "/tags") => Requests::get_tags(),

        // Return the 404 Not Found for other routes.
        _ => {
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

    println!("Listening on http://{}", addr);

    server.await?;

    Ok(())
}
