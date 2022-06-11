use std::convert::Infallible;

use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Request, Response, Server, Method, StatusCode};
use serde_json;
use serde::{Serialize, Deserialize};
use std::fs;
use std::fs::File;
use std::io::Write;
use std::sync::Mutex;

#[macro_use]
extern crate lazy_static;

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
enum Month {
    January, February, March, April, May, June, July, August, September, October, November, December
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
    return Date{
        day : 0,
        month : Month::April,
        year : 2000
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

lazy_static! {
    static ref ENTRIES: Mutex<Vec<Entry>> = Mutex::new(Vec::new());
}

// TO DO: Parameterize
static FILENAME : &str = "./target/entries.json";

async fn process_request(req : Request<Body>) -> Result<Response<Body>, hyper::Error> {
    match (req.method(), req.uri().path()) {
        (&Method::POST, "/texts") => {
            let whole_body = hyper::body::to_bytes(req.into_body()).await?;
            match serde_json::from_slice(&whole_body) as Result<NewEntryForm, serde_json::Error> {
                Ok(form) => {
                    let mut entries = ENTRIES.lock().unwrap();
                    let new_entry = make_entry(&form);
                    entries.push(new_entry);

                    if let Ok(mut file) = File::create(FILENAME) {
                        if let Ok(json) = serde_json::to_vec_pretty(&*entries) {
                            _ = file.write_all(&json);
                        }
                    }

                    Ok(Response::new(Body::from(format!("{:?}", form))))
                }
                Err(err) => Ok(Response::builder()
                    .status(StatusCode::BAD_REQUEST)
                    .body(Body::from(format!("Invalid entry: {}", err)))
                    .unwrap()
                )
            }
        }

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
                Ok(read_entries) => *ENTRIES.lock().unwrap() = read_entries,
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
