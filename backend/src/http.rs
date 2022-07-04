use hyper_tls::HttpsConnector;

pub fn get_header_case_insensitive<'a>(headers: &'a hyper::HeaderMap, target_header: &str) -> Option<&'a hyper::header::HeaderValue> 
{
    let target_lowercase = target_header.to_lowercase();
    return headers.iter()
        .find(|(name, _)| name.as_str().to_lowercase() == target_lowercase)
        .map(|(_, value)| value);
}

pub async fn request(request : hyper::Request<hyper::Body>) -> hyper::Result<hyper::Response<hyper::Body>>
{
    let https = HttpsConnector::new();
    let client = hyper::Client::builder().build::<_, hyper::Body>(https);

    return client.request(request).await;
}

pub async fn get(url : &str) -> hyper::Result<hyper::Response<hyper::Body>>
{
    let req = hyper::Request::builder()
        .method(hyper::Method::GET)
        .uri(url)
        .header("user-agent", "localhost/0.1.0")
        .body(hyper::Body::from(""))
        .unwrap();

    return request(req).await;
}
