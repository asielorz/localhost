fn get_value_between_quotes(source: &str) -> Option<&str> {
    source
        .split_once("=\"")
        .map(|(_, after_first_quote)| after_first_quote)
        .and_then(|s| s.split_once('"'))
        .map(|(before_second_quote, _)| before_second_quote)
}

fn get_name_and_value_of_meta_string(source: &str) -> Option<(&str, &str)> {
    let name_index = source
        .find("name=\"")
        .or_else(|| source.find("property=\""))?;
    let content_index = source.find("content=\"")?;
    let name = get_value_between_quotes(source.split_at(name_index).1)?;
    let content = get_value_between_quotes(source.split_at(content_index).1)?;
    Some((name, content))
}

pub fn html_meta_headers(html_source: &str) -> Vec<(&str, &str)> {
    let mut headers = Vec::new();

    // Skip the first because that is the part before the first meta
    for part in html_source.split("<meta ").skip(1) {
        if let Some(meta) = get_name_and_value_of_meta_string(part) {
            headers.push(meta);
        }
    }

    headers
}
