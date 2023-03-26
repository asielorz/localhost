use crate::date;
use percent_encoding::percent_decode_str;
use std::fmt::Write;

pub fn url_to_sql_query(query_text: &str) -> Option<(String, Vec<String>, usize)> {
    let mut result = String::new();
    let mut params: Vec<String> = Vec::new();
    let mut offset: usize = 0;

    if let Ok(decoded_query_text) = percent_decode_str(query_text).decode_utf8() {
        for query_argument in decoded_query_text.split('&') {
            let key_value: Vec<_> = query_argument.split('=').collect();
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
                        _ = write!(
                            &mut result,
                            "entry_type = {}",
                            parse_type_query_argument(key_value[1])?
                        );
                    }
                    "works_mentioned" => {
                        for s in key_value[1].split('|') {
                            result += "works_mentioned LIKE ? AND ";
                            params.push(sql_arg_list_contains(s));
                        }
                        // Remove last " AND "
                        for _ in 0..5 {
                            result.pop();
                        }
                    }
                    "themes" => {
                        for s in key_value[1].split('|') {
                            result += "themes LIKE ? AND ";
                            params.push(sql_arg_list_contains(s));
                        }
                        // Remove last " AND "
                        for _ in 0..5 {
                            result.pop();
                        }
                    }
                    "tags" => {
                        for s in key_value[1].split('|') {
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
                        params.push(date::format_as_sql_date(date::read_sql_date(key_value[1])?));
                    }
                    "published_between_until" => {
                        result += "date_published <= DATE(?)";
                        params.push(date::format_as_sql_date(date::read_sql_date(key_value[1])?));
                    }
                    "saved_between_from" => {
                        result += "date_saved >= DATE(?)";
                        params.push(date::format_as_sql_date(date::read_sql_date(key_value[1])?));
                    }
                    "saved_between_until" => {
                        result += "date_saved <= DATE(?)";
                        params.push(date::format_as_sql_date(date::read_sql_date(key_value[1])?));
                    }
                    "exceptional" => {
                        result += "exceptional = ";
                        result += if key_value[1] == "true" {
                            "TRUE"
                        } else {
                            "FALSE"
                        };
                    }
                    "offset" => {
                        offset = key_value[1].parse::<usize>().ok()?;

                        // We treat the offset in a special way in this function. It is not part of the query string,
                        // but a special value that is returned as an integer and will be added later to the string.
                        // However, since the loop adds a " AND " at the end of every iteration, it may happen that
                        // parsing the offset leaves us with two repeated ands or a trailing and in the query if we
                        // don't remove one, which would cause a parse error in SQL.
                        if result.ends_with(" AND ") {
                            result.truncate(result.len() - " AND ".len());
                        }
                    }
                    _ => {
                        return None;
                    }
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

    Some((result, params, offset))
}

fn sql_arg_string_contains(string: &str) -> String {
    String::from("%") + string + "%"
}

fn sql_arg_list_contains(string: &str) -> String {
    String::from("%|") + string + "|%"
}

// Returns type index
fn parse_type_query_argument(argument: &str) -> Option<i32> {
    match argument {
        "article" => Some(0),
        "paper" => Some(1),
        "book" => Some(2),
        "video" => Some(3),
        "audio" => Some(4),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // url_to_sql_query

    #[test]
    fn test_url_to_sql_query_link_checks_for_containment() {
        let url_params = "link=wikipedia";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "link LIKE ?");
                assert_eq!(params, ["%wikipedia%"]);
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_title_checks_for_containment() {
        let url_params = "title=Hello";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "title LIKE ?");
                assert_eq!(params, ["%Hello%"]);
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_author_checks_for_equality() {
        let url_params = "author=Pauline%20Kael";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "author = ?");
                assert_eq!(params, ["Pauline Kael"]);
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_description_checks_for_containment() {
        let url_params = "description=compiler";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "description LIKE ?");
                assert_eq!(params, ["%compiler%"]);
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_category_checks_for_equality() {
        let url_params = "category=Programming";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "category = ?");
                assert_eq!(params, ["Programming"]);
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_themes_checks_for_containment_of_each() {
        let url_params = "themes=Rust%7CTesting";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "themes LIKE ? AND themes LIKE ?");
                assert_eq!(params, ["%|Rust|%", "%|Testing|%"]);
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_works_checks_for_containment_of_each() {
        let url_params = "works_mentioned=Hamlet%7CMacBeth%7CKing%20Lear";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(
                    query,
                    "works_mentioned LIKE ? AND works_mentioned LIKE ? AND works_mentioned LIKE ?"
                );
                assert_eq!(params, ["%|Hamlet|%", "%|MacBeth|%", "%|King Lear|%"]);
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_tags_checks_for_containment_of_each() {
        let url_params = "tags=Soulslike%7CGreat%20soundtrack%7CFemale%20protagonist";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "tags LIKE ? AND tags LIKE ? AND tags LIKE ?");
                assert_eq!(
                    params,
                    [
                        "%|Soulslike|%",
                        "%|Great soundtrack|%",
                        "%|Female protagonist|%"
                    ]
                );
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_date_is_formated_as_yyyy_mm_dd() {
        let url_params = "published_between_from=1967-5-3";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "date_published >= DATE(?)");
                assert_eq!(params, ["1967-05-03"]);
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_published_between_until_checks_for_less_equal() {
        let url_params = "published_between_until=1967-11-24";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "date_published <= DATE(?)");
                assert_eq!(params, ["1967-11-24"]);
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_bool_true_is_formated_as_uppercase_true() {
        let url_params = "exceptional=true";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "exceptional = TRUE");
                assert!(params.is_empty());
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_bool_false_is_formated_as_uppercase_false() {
        let url_params = "exceptional=false";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "exceptional = FALSE");
                assert!(params.is_empty());
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_two_or_more_predicates_are_anded() {
        let url_params = "link=wikipedia&author=Pauline%20Kael&tags=Soulslike%7CGreat%20soundtrack%7CFemale%20protagonist";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(
                    query,
                    "link LIKE ? AND author = ? AND tags LIKE ? AND tags LIKE ? AND tags LIKE ?"
                );
                assert_eq!(
                    params,
                    [
                        "%wikipedia%",
                        "Pauline Kael",
                        "%|Soulslike|%",
                        "%|Great soundtrack|%",
                        "%|Female protagonist|%"
                    ]
                );
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_quotes_are_escaped() {
        let url_params = "link=%22wikipedia%22";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "link LIKE ?");
                assert_eq!(params, [r#"%"wikipedia"%"#]);
                assert_eq!(offset, 0);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_type_is_converted_to_an_index() {
        {
            let url_params = "type=article";
            match url_to_sql_query(url_params) {
                Some((query, params, offset)) => {
                    assert_eq!(query, "entry_type = 0");
                    assert!(params.is_empty());
                    assert_eq!(offset, 0);
                }
                None => unreachable!(),
            }
        }
        {
            let url_params = "type=paper";
            match url_to_sql_query(url_params) {
                Some((query, params, offset)) => {
                    assert_eq!(query, "entry_type = 1");
                    assert!(params.is_empty());
                    assert_eq!(offset, 0);
                }
                None => unreachable!(),
            }
        }
        {
            let url_params = "type=book";
            match url_to_sql_query(url_params) {
                Some((query, params, offset)) => {
                    assert_eq!(query, "entry_type = 2");
                    assert!(params.is_empty());
                    assert_eq!(offset, 0);
                }
                None => unreachable!(),
            }
        }
        {
            let url_params = "type=video";
            match url_to_sql_query(url_params) {
                Some((query, params, offset)) => {
                    assert_eq!(query, "entry_type = 3");
                    assert!(params.is_empty());
                    assert_eq!(offset, 0);
                }
                None => unreachable!(),
            }
        }
        {
            let url_params = "type=audio";
            match url_to_sql_query(url_params) {
                Some((query, params, offset)) => {
                    assert_eq!(query, "entry_type = 4");
                    assert!(params.is_empty());
                    assert_eq!(offset, 0);
                }
                None => unreachable!(),
            }
        }
    }

    #[test]
    fn test_url_to_sql_query_offset() {
        let url_params = "offset=10";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "");
                assert!(params.is_empty());
                assert_eq!(offset, 10);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_offset_and_other_parameter() {
        let url_params = "type=article&offset=10";
        match url_to_sql_query(url_params) {
            Some((query, params, offset)) => {
                assert_eq!(query, "entry_type = 0");
                assert!(params.is_empty());
                assert_eq!(offset, 10);
            }
            None => unreachable!(),
        }
    }

    #[test]
    fn test_url_to_sql_query_bad_type() {
        let url_params = "type=snafucated";
        assert!(url_to_sql_query(url_params).is_none());
    }
}
