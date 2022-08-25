use crate::http;
use crate::paths;
use crate::date;
use crate::state::*;
use crate::url_to_sql_query::{url_to_sql_query};
use crate::sql_array::*;
use crate::entry_type;
use crate::forms::*;
use crate::html_meta::html_meta_headers;
use crate::images::normalize_image;

use hyper::{Body, Request, Response, StatusCode};
use serde::{Serialize};
use std::fs;

pub fn options() -> Result<Response<Body>, hyper::Error>
{
    println!("Options!");
    Response::builder()
        .status(StatusCode::NO_CONTENT)
        .header("Allow", "OPTIONS, GET, PUT, POST, DELETE")
        .header("Access-Control-Allow-Origin", "*")
        .header("Access-Control-Allow-Headers", "*")
        .header("Access-Control-Allow-Methods", "OPTIONS, GET, PUT, POST, DELETE")
        .body(Body::from(""))
        .or_else(|_| internal_server_error_response())
}

fn internal_server_error_response() -> Result<Response<Body>, hyper::Error>
{
    println!("500 Internal server error");
    Ok(Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .header("Access-Control-Allow-Origin", "*")
        .header("Access-Control-Allow-Headers", "*")
        .body(Body::from(""))
        .unwrap()
    )
}

pub fn not_found_404_response() -> Result<Response<Body>, hyper::Error>
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
        .or_else(|_| internal_server_error_response())
}

fn to_json_http_response<T : Serialize>(entries : &T) -> Result<Response<Body>, hyper::Error>
{
    if let Ok(json) = serde_json::to_string(entries) {
        Response::builder()
            .status(StatusCode::OK)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .header("Content-Type", "application/json")
            .body(Body::from(json))
            .or_else(|_| internal_server_error_response())
    } else {
        internal_server_error_response()
    }
}

pub fn get_texts(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let (sql_query, sql_params, offset) = match req.uri().query() {
        Some(query_text) => match url_to_sql_query(query_text) {
            Some(result) => result,
            None => { return internal_server_error_response(); }
        },
        None => (String::from(""), Vec::new(), 0)
    };

    let served_entries = {
        let state = global_state().lock().unwrap();
        match select_texts(state.database.as_ref().unwrap(), &sql_query, sql_params.iter().map(|x| x as &dyn rusqlite::ToSql).collect::<Vec<&dyn rusqlite::ToSql>>().as_slice(), offset) {
            Ok(entries) => entries,
            Err(err) => {
                println!("SQL query error: {}", err);
                return internal_server_error_response();
            }
        }
    };

    to_json_http_response(&served_entries)
}

pub fn get_single_text(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let path = req.uri().path().strip_prefix("/api/texts/").unwrap();
    if let Ok(entry_id) = path.parse::<i64>() {
        let served_entries = {
            let state = global_state().lock().unwrap();
            match select_texts(state.database.as_ref().unwrap(), "entry_id = ?", [ entry_id ], 0) {
                Ok(entries) => entries,
                Err(err) => {
                    println!("{}", err);
                    return internal_server_error_response();
                }
            }
        };
        if served_entries.entries.is_empty() {
            not_found_404_response()
        } else {
            to_json_http_response(&served_entries.entries[0])
        }
    } else {
        bad_request_response(&format!("Could not convert '{}' to an entry id", path))
    }
}

pub async fn put_single_text(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let entry_id = paths::get_entry_id_from_path(req.uri().path());

    let whole_body = hyper::body::to_bytes(req.into_body()).await?;
    match serde_json::from_slice(&whole_body) as Result<NewEntryForm, serde_json::Error> {
        Ok(form) => {
            let state = global_state().lock().unwrap();

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
                        , &date::format_as_sql_date(form.date_published)
                        , &date::format_as_sql_date(date::today())
                        , form.exceptional
                        , entry_type::index(form.entry_type)
                        , entry_type::metadata(form.entry_type)
                        , entry_id
                        ]
                );

                if let Err(err) = result {
                    println!("Entry update failed: {}", err);
                    return internal_server_error_response();
                }

                Response::builder()
                    .status(StatusCode::NO_CONTENT)
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "*")
                    .body(Body::from(""))
                    .or_else(|_| internal_server_error_response())
            } else {
                internal_server_error_response()
            }
        },
        Err(err) => bad_request_response(&format!("{}", err))
    }
}

pub fn delete_single_text(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let entry_id = paths::get_entry_id_from_path(req.uri().path());

    let state = global_state().lock().unwrap();
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
        return not_found_404_response();
    }

    Response::builder()
        .status(StatusCode::NO_CONTENT)
        .header("Access-Control-Allow-Origin", "*")
        .header("Access-Control-Allow-Headers", "*")
        .body(Body::from(""))
        .or_else(|_| internal_server_error_response())
}

pub fn get_entry_image(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let entry_id = paths::get_entry_id_from_path(req.uri().path());

    let state = global_state().lock().unwrap();
    let database = state.database.as_ref().unwrap();
 
    let mut statement = match database.prepare("SELECT image FROM entries WHERE entry_id = ?") {
        Ok(s) => s,
        Err(_) => { return internal_server_error_response(); }
    };
    let mut rows = match statement.query([entry_id]) {
        Ok(r) => r,
        Err(_) => { return internal_server_error_response(); }
    };
        
    let first = match rows.next() {
        Ok(r) => r,
        Err(_) => { return internal_server_error_response(); }
    };

    if let Some(found_row) = first {
        let blob_ref = match found_row.get_ref(0) {
            Ok(r) => r,
            Err(_) => { return internal_server_error_response(); }
        };

        let maybe_blob = match blob_ref.as_blob_or_null() {
            Ok(r) => r,
            Err(_) => { return internal_server_error_response(); }
        };

        if let Some(blob) = maybe_blob {
            let copy = Vec::from(blob);

            Response::builder()
                .status(StatusCode::OK)
                .header("Access-Control-Allow-Origin", "*")
                .header("Access-Control-Allow-Headers", "*")
                .header("Content-Type", "image/png")
                .header("Cache-Control", "public, max-age=31919000, immutable")
                .body(Body::from(copy))
                .or_else(|_| internal_server_error_response())
        } else {
            not_found_404_response()
        }
    } else {
        not_found_404_response()
    }
}

pub async fn put_entry_image(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let entry_id = paths::get_entry_id_from_path(req.uri().path());

    let is_content_in_body : bool = match http::get_header_case_insensitive(req.headers(), "Content-Type") {
        None => { return bad_request_response("Missing content type hader"); }
        Some(value) => match value.to_str().unwrap() {
            // TO DO: Save image type.
            "image/png" => true,
            "image/jpeg" => true,
            "image/gif" => true,
            "image/bmp" => true,
            "application/json" => false,
            other => { return bad_request_response(&format!("Unsuported content type: {}", other)); }
        }
    };

    let whole_body = hyper::body::to_bytes(req.into_body()).await?;

    let image_bytes = if is_content_in_body {
        whole_body
    } else {
        match serde_json::from_slice(&whole_body) as Result<ImageLinkForm, serde_json::Error> {
            Ok(form) => { 
                let response = match http::get(&form.image_url).await {
                    Ok(r) => r,
                    Err(err) => {
                        println!("Request failed: {}", err);
                        Err(err)?
                    }
                };

                match http::get_header_case_insensitive(response.headers(), "Content-Type") {
                    None => { return bad_request_response(&format!("Missing content type header at link {}.", &form.image_url)); }
                    Some(value) => match value.to_str().unwrap() {
                        // TO DO: Save image type.
                        "image/png" => (),
                        "image/jpeg" => (),
                        "image/gif" => (),
                        "image/bmp" => (),
                        other => { return bad_request_response(&format!("Unsuported content type {} at link {}", other, &form.image_url)); }
                    }
                }

                hyper::body::to_bytes(response.into_body()).await?
            }
            Err(err) => { return bad_request_response(&format!("{}", err));  }
        }
    };

    let normalized_image_bytes = match normalize_image(image_bytes) {
        Ok(x) => x,
        Err(err) => return bad_request_response(&format!("Body is not a valid image: {}", err))
    };

    let state = global_state().lock().unwrap();
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
        Ok(0) => { return not_found_404_response(); }
        // Something went wrong within the database.
        Err(_) => { return internal_server_error_response(); }
        // Everything went fine.
        Ok(_) => ()
    };

    let mut blob = match database.blob_open(rusqlite::MAIN_DB, "entries", "image", entry_id, false) {
        Ok(b) => b,
        Err(_) => { return internal_server_error_response(); }
    };

    if blob.write_at(&normalized_image_bytes, 0).is_err() {
        return internal_server_error_response();
    };

    Response::builder()
        .status(StatusCode::OK)
        .header("Access-Control-Allow-Origin", "*")
        .header("Access-Control-Allow-Headers", "*")
        .header("Content-Type", "application/json")
        .body(Body::from(format!(r#"{{"link":"http://localhost:8080/api/texts/{}/image"}}"#, entry_id)))
        .or_else(|_| internal_server_error_response())
}

pub fn delete_entry_image(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let entry_id = paths::get_entry_id_from_path(req.uri().path());

    let state = global_state().lock().unwrap();
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
        return not_found_404_response();
    }

    Response::builder()
        .status(StatusCode::NO_CONTENT)
        .header("Access-Control-Allow-Origin", "*")
        .header("Access-Control-Allow-Headers", "*")
        .body(Body::from(""))
        .or_else(|_| internal_server_error_response())
}

pub fn get_entry_backup(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let entry_id = paths::get_entry_id_from_path(req.uri().path());

    let state = global_state().lock().unwrap();
    let database = state.database.as_ref().unwrap();

    let mut statement = match database.prepare("SELECT backup FROM entries WHERE entry_id = ?") {
        Ok(s) => s,
        Err(_) => { return internal_server_error_response(); }
    };
    let mut rows = match statement.query([entry_id]) {
        Ok(r) => r,
        Err(_) => { return internal_server_error_response(); }
    };
        
    let first = match rows.next() {
        Ok(r) => r,
        Err(_) => { return internal_server_error_response(); }
    };

    if let Some(found_row) = first {
        let blob_ref = match found_row.get_ref(0) {
            Ok(r) => r,
            Err(_) => { return internal_server_error_response(); }
        };

        let maybe_blob = match blob_ref.as_blob_or_null() {
            Ok(r) => r,
            Err(_) => { return internal_server_error_response(); }
        };

        if let Some(blob) = maybe_blob {
            let content_type_length = blob[0] as usize;
            let content_type = &blob[1..content_type_length + 1];
            let content_data = Vec::from(&blob[content_type_length + 1..]);

            Response::builder()
                .status(StatusCode::OK)
                .header("Access-Control-Allow-Origin", "*")
                .header("Access-Control-Allow-Headers", "*")
                .header("Cache-Control", "public, max-age=31919000, immutable")
                .header("Content-Type", content_type)
                .body(Body::from(content_data))
                .or_else(|_| internal_server_error_response())
        } else {
            not_found_404_response()
        }
    } else {
        not_found_404_response()
    }
}

fn write_entry_backup_to_database(entry_id : i64, body : hyper::body::Bytes, content_type : &str) -> Result<Response<Body>, hyper::Error>
{
    let blob_header_length = content_type.len() + 1; // + 1 for storing the size.
    let blob_length = blob_header_length + body.len();

    let state = global_state().lock().unwrap();
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
        Ok(0) => { return not_found_404_response(); }
        // Something went wrong within the database.
        Err(_) => { return internal_server_error_response(); }
        // Everything went fine.
        Ok(_) => ()
    };

    let mut blob = match database.blob_open(rusqlite::MAIN_DB, "entries", "backup", entry_id, false) {
        Ok(b) => b,
        Err(_) => { return internal_server_error_response(); }
    };

    // Write the length of the content type string in 1 byte. Content type strings are very short
    // so 1 byte should always be enough.
    if blob.write_at(&[content_type.len() as u8], 0).is_err() {
        return internal_server_error_response();
    };

    // Write the content type string. This way, when a client requests the backup, we can return it with
    // the correct content type.
    if blob.write_at(content_type.as_bytes(), 1).is_err() {
        return internal_server_error_response();
    };

    // Write the actual backup data.
    if blob.write_at(&body, blob_header_length).is_err() {
        return internal_server_error_response();
    };

    Response::builder()
        .status(StatusCode::OK)
        .header("Access-Control-Allow-Origin", "*")
        .header("Access-Control-Allow-Headers", "*")
        .header("Content-Type", "application/json")
        .body(Body::from(format!(r#"{{"link":"http://localhost:8080/api/texts/{}/backup"}}"#, entry_id)))
        .or_else(|_| internal_server_error_response())
}

pub async fn put_entry_backup(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let entry_id = paths::get_entry_id_from_path(req.uri().path());

    let content_type = match http::get_header_case_insensitive(req.headers(), "Content-Type") {
        None => { return bad_request_response("Missing content type header"); }
        Some(value) => String::from(value.to_str().unwrap())
    };

    let whole_body = hyper::body::to_bytes(req.into_body()).await?;

    if content_type == "application/json" {
        if let Ok(form) = serde_json::from_slice(&whole_body) as Result<BackupLinkForm, serde_json::Error> {
            let response = match http::get(&form.backup_url).await {
                Ok(x) => x,
                Err(err) => return bad_request_response(&format!("Request to url {} failed: {}", form.backup_url, err))
            };
                
            if response.status() != StatusCode::OK {
                return bad_request_response(&format!("Could not get resource at url {}", form.backup_url));
            }
                
            let response_content_type = match http::get_header_case_insensitive(response.headers(), "Content-Type") {
                None => { return bad_request_response(&format!("Missing content type header in resource at url {}", form.backup_url)); }
                Some(value) => String::from(value.to_str().unwrap())
            };

            let response_body = hyper::body::to_bytes(response.into_body()).await?;

            return write_entry_backup_to_database(entry_id, response_body, &response_content_type)
        }
    }

    write_entry_backup_to_database(entry_id, whole_body, &content_type)
}

pub fn delete_entry_backup(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let entry_id = paths::get_entry_id_from_path(req.uri().path());

    let state = global_state().lock().unwrap();
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
        return not_found_404_response();
    }

    Response::builder()
        .status(StatusCode::NO_CONTENT)
        .header("Access-Control-Allow-Origin", "*")
        .header("Access-Control-Allow-Headers", "*")
        .body(Body::from(""))
        .or_else(|_| internal_server_error_response())
}

fn strings_as_http_response(strings : &Vec<String>) -> Result<Response<Body>, hyper::Error>
{
    if let Ok(json) = serde_json::to_string(strings) {
        Response::builder()
            .status(StatusCode::OK)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .header("Content-Type", "application/json")
            .body(Body::from(json))
            .or_else(|_| internal_server_error_response())
    } else {
        internal_server_error_response()
    }
}

pub fn get_categories() -> Result<Response<Body>, hyper::Error>
{
    match &get_all_strings_of("categories") {
        Ok(strings) => strings_as_http_response(strings),
        Err(_) => internal_server_error_response()
    }
}

pub fn get_authors() -> Result<Response<Body>, hyper::Error>
{
    match &get_all_strings_of("authors") {
        Ok(strings) => strings_as_http_response(strings),
        Err(_) => internal_server_error_response()
    }
}

pub fn get_themes() -> Result<Response<Body>, hyper::Error>
{
    match &get_all_strings_of("themes") {
        Ok(strings) => strings_as_http_response(strings),
        Err(_) => internal_server_error_response()
    }
}

pub fn get_works() -> Result<Response<Body>, hyper::Error>
{
    match &get_all_strings_of("works") {
        Ok(strings) => strings_as_http_response(strings),
        Err(_) => internal_server_error_response()
    }
}

pub fn get_tags() -> Result<Response<Body>, hyper::Error>
{
    match &get_all_strings_of("tags") {
        Ok(strings) => strings_as_http_response(strings),
        Err(_) => internal_server_error_response()
    }
}

pub async fn post_texts(req : Request<Body>) -> Result<Response<Body>, hyper::Error>
{
    let whole_body = hyper::body::to_bytes(req.into_body()).await?;
    match serde_json::from_slice(&whole_body) as Result<NewEntryForm, serde_json::Error> {
        Ok(form) => {
            let state = global_state().lock().unwrap();

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
                        , &date::format_as_sql_date(form.date_published)
                        , &date::format_as_sql_date(date::today())
                        , form.exceptional
                        , entry_type::index(form.entry_type)
                        , entry_type::metadata(form.entry_type)
                        ]
                );

                if let Err(err) = result {
                    println!("Insert to database failed: {}", err);
                    return internal_server_error_response();
                }

                let last_insert_row_id = database.last_insert_rowid();

                // Ignore errors when inserting an element that is already present in tables of unique values.
                _ = run_sql(database, "INSERT INTO authors (value) VALUES (?)", [&form.author]);
                _ = run_sql(database, "INSERT INTO categories (value) VALUES (?)", [&form.category]);
                for theme in &form.themes {
                    _ = run_sql(database, "INSERT INTO themes (value) VALUES (?)", [theme]);
                }
                for work in &form.works_mentioned {
                    _ = run_sql(database, "INSERT INTO works (value) VALUES (?)", [work]);
                }
                for tag in &form.tags {
                    _ = run_sql(database, "INSERT INTO tags (value) VALUES (?)", [tag]);
                }

                Response::builder()
                    .status(StatusCode::OK)
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "*")
                    .header("Content-Type", "application/json")
                    .body(Body::from(format!(r#"{{"id":{},"link":"http://localhost:8080/api/texts/{}"}}"#, last_insert_row_id, last_insert_row_id)))
                    .or_else(|_| internal_server_error_response())
            } else {
                internal_server_error_response()
            }
        }
        Err(err) => {
            bad_request_response(&format!("{}", err))
        }
    }
}

pub fn serve_page(path : &str) -> Result<Response<Body>, hyper::Error>
{
    serve_file(path, "text/html; charset=utf-8", false)
}

pub fn serve_file(path : &str, content_type : &str, cache : bool) -> Result<Response<Body>, hyper::Error>
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
                .or_else(|_| internal_server_error_response())
        }
        Err(_) => internal_server_error_response()
    }
}

pub async fn forward_get_request(url : &str) -> Result<Response<Body>, hyper::Error>
{
    let mut response = http::get(url).await?;

    response.headers_mut().insert("Access-Control-Allow-Origin", hyper::header::HeaderValue::from_static("*"));
    response.headers_mut().insert("Access-Control-Allow-Headers", hyper::header::HeaderValue::from_static("*"));

    Ok(response)
}

pub async fn get_meta_headers_at_url(request_uri : &hyper::Uri) -> Result<Response<Body>, hyper::Error>
{
    let url = {
        let mut url = String::from(request_uri.path().strip_prefix("/api/meta_headers/").unwrap());
        if let Some(query) = request_uri.query() {
            url += "?";
            url += query;
        }
        url
    };

    let response = match http::get(&url).await {
        Ok(r) => r,
        Err(_) => return not_found_404_response()
    };

    // If the requested resource is not able to return a succesful response, return a 404.
    if response.status() != StatusCode::OK {
        println!("Response status: {}", response.status().as_u16());
        return not_found_404_response();
    }

    // If the requested resource is not html, return a 404.
    let content_type_header = http::get_header_case_insensitive(response.headers(), "Content-Type");

    println!("{}", content_type_header.map(|x| x.to_str().unwrap()).unwrap_or("No content type"));

    if !content_type_header.map_or(false, |h| h.to_str().unwrap().starts_with("text/html")) {
        return not_found_404_response();
    }

    let whole_body = hyper::body::to_bytes(response.into_body()).await?;
    let whole_text = match String::from_utf8(whole_body.to_vec()) {
        Ok(x) => x,
        Err(_) => return internal_server_error_response()
    };

    let headers = html_meta_headers(&whole_text);

    if let Ok(json) = serde_json::to_string(&headers) {
        Response::builder()
            .status(StatusCode::OK)
            .header("Access-Control-Allow-Origin", "*")
            .header("Access-Control-Allow-Headers", "*")
            .header("Content-Type", "application/json")
            .body(Body::from(json))
            .or_else(|_| internal_server_error_response())
    } else {
        internal_server_error_response()
    }
}

#[derive(Serialize)]
struct GetTextsResponse
{
    entries : Vec<Entry>,
    current_offset : usize,
    next_offset : usize,
    total_size : usize,
}

fn select_texts<Params : rusqlite::Params>(database : &rusqlite::Connection, where_query : &str, sql_params : Params, offset : usize) -> rusqlite::Result<GetTextsResponse> {
    let mut found_entries : Vec<Entry> = Vec::new();

    let sql_query = if where_query.is_empty() {
        format!("SELECT *, count(*) OVER() AS full_count FROM entries LIMIT 10 OFFSET {}", offset)
    } else {
        format!("SELECT *, count(*) OVER() AS full_count FROM entries WHERE {} LIMIT 10 OFFSET {}", where_query, offset)
    };

    let mut statement = database.prepare(&sql_query)?;
    let mut rows = statement.query(sql_params)?;
    let mut total_size : usize = 0;
    while let Some(row) = rows.next()? {
        let id : i64 = row.get(0)?;
        let entry_type_index : i32 = row.get(12)?;
        let entry_type_metadata : i32 = row.get(13)?;
        total_size = row.get(16)?;

        found_entries.push(Entry{
            id,
            link : row.get(1)?,
            title : row.get(2)?,
            description : row.get(3)?,
            author : row.get(4)?,
            category : row.get(5)?,
            themes : read_from_sql_array(&row.get::<_, String>(6)?),
            works_mentioned : read_from_sql_array(&row.get::<_, String>(7)?),
            tags : read_from_sql_array(&row.get::<_, String>(8)?),
            date_published : date::read_sql_date(&row.get::<_, String>(9)?).unwrap(),
            date_saved : date::read_sql_date(&row.get::<_, String>(10)?).unwrap(),
            exceptional : row.get(11)?,
            entry_type : entry_type::from_index_and_metadata(entry_type_index, entry_type_metadata),
            image : if row.get_ref(14)?.as_blob_or_null()?.is_some() { Some(format!("http://localhost:8080/api/texts/{}/image", id)) } else { None },
            backup : if row.get_ref(15)?.as_blob_or_null()?.is_some() { Some(format!("http://localhost:8080/api/texts/{}/backup", id)) } else { None },
        });
    }

    Ok(GetTextsResponse{
        next_offset : offset + found_entries.len(),
        entries : found_entries,
        current_offset : offset,
        total_size
    })
}

fn run_sql<Params : rusqlite::Params>(database : &rusqlite::Connection, command : &str, params : Params) -> rusqlite::Result<usize>
{
    let result = database.execute(command, params);
    if let Err(err) = &result {
        println!("SQL error: {}", err);
    }
    result
}
