mod date;
mod entry_type;
mod forms;
mod html_meta;
mod http;
mod images;
mod paths;
mod requests;
mod sql_array;
mod state;
mod url_to_sql_query;

use state::global_state;

use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, Server};
use serde::Deserialize;
use std::convert::Infallible;
use std::env;
use std::fs;

#[derive(Deserialize)]
struct ConfigFile {
    database_path: String,
}

async fn process_request(req: Request<Body>) -> Result<Response<Body>, hyper::Error> {
    match req.uri().query() {
        Some(query_text) => {
            println!(
                "{} request at path {} with query {}",
                req.method(),
                req.uri().path(),
                query_text
            );
        }
        None => {
            println!("{} request at path {}", req.method(), req.uri().path());
        }
    };

    match (req.method(), req.uri().path()) {
        (&Method::OPTIONS, _) => requests::options(),

        (&Method::GET, "/favicon.ico") => {
            requests::serve_file("pages/favicon.ico", "image/vnd.microsoft.icon", true)
        }
        (&Method::GET, path) if path.starts_with("/fontawesome/") => requests::serve_file(
            &format!("pages/{}", path),
            if path.ends_with(".css") {
                "text/css"
            } else {
                "font/ttf"
            },
            true,
        ),
        (&Method::GET, path) if !path.starts_with("/api/") => {
            requests::serve_page("pages/index.html")
        }

        (&Method::GET, "/api/texts") => requests::get_texts(req),
        (&Method::GET, path) if paths::is_single_entry_path(path) => requests::get_single_text(req),
        (&Method::PUT, path) if paths::is_single_entry_path(path) => {
            requests::put_single_text(req).await
        }
        (&Method::DELETE, path) if paths::is_single_entry_path(path) => {
            requests::delete_single_text(req)
        }
        (&Method::GET, path) if paths::is_entry_image_path(path) => requests::get_entry_image(req),
        (&Method::PUT, path) if paths::is_entry_image_path(path) => {
            requests::put_entry_image(req).await
        }
        (&Method::DELETE, path) if paths::is_entry_image_path(path) => {
            requests::delete_entry_image(req)
        }
        (&Method::GET, path) if paths::is_entry_backup_path(path) => {
            requests::get_entry_backup(req)
        }
        (&Method::PUT, path) if paths::is_entry_backup_path(path) => {
            requests::put_entry_backup(req).await
        }
        (&Method::DELETE, path) if paths::is_entry_backup_path(path) => {
            requests::delete_entry_backup(req)
        }
        (&Method::POST, "/api/texts") => requests::post_texts(req).await,

        (&Method::GET, "/api/categories") => requests::get_categories(),
        (&Method::GET, "/api/authors") => requests::get_authors(),
        (&Method::GET, "/api/themes") => requests::get_themes(),
        (&Method::GET, "/api/works") => requests::get_works(),
        (&Method::GET, "/api/tags") => requests::get_tags(),

        (&Method::GET, path) if path.starts_with("/api/forward/") => {
            requests::forward_get_request(path.strip_prefix("/api/forward/").unwrap()).await
        }
        (&Method::GET, path) if path.starts_with("/api/meta_headers/") => {
            requests::get_meta_headers_at_url(req.uri()).await
        }

        // Return the 404 Not Found for other routes.
        _ => requests::not_found_404_response(),
    }
}

fn load_config_file<P: AsRef<std::path::Path>>(
    path: P,
) -> Result<ConfigFile, Box<dyn std::error::Error + Send + Sync>> {
    Ok(serde_json::from_str(&fs::read_to_string(path)?)?)
}

#[tokio::main]
pub async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let config_path = match dirs::home_dir() {
        Some(dir) => {
            let mut osstr = dir.into_os_string();
            osstr.push("/.localhost.json");
            osstr
        }
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
        ",
        [],
    )?;

    connection.execute("CREATE TABLE IF NOT EXISTS authors (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, value TEXT UNIQUE NOT NULL);", [])?;
    connection.execute("CREATE TABLE IF NOT EXISTS categories (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, value TEXT UNIQUE NOT NULL);", [])?;
    connection.execute("CREATE TABLE IF NOT EXISTS themes (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, value TEXT UNIQUE NOT NULL);", [])?;
    connection.execute("CREATE TABLE IF NOT EXISTS works (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, value TEXT UNIQUE NOT NULL);", [])?;
    connection.execute("CREATE TABLE IF NOT EXISTS tags (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, value TEXT UNIQUE NOT NULL);", [])?;

    global_state().lock().unwrap().database = Some(connection);

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

    println!(
        "Current dir: {}",
        env::current_dir().unwrap().to_str().unwrap()
    );
    println!("Listening on http://{}", addr);

    server.await?;

    Ok(())
}
