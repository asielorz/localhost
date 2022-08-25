pub fn format_as_sql_array(strings: &[String]) -> String {
    if strings.is_empty() {
        String::from("")
    } else {
        String::from("|") + &strings.join("|") + "|"
    }
}

pub fn read_from_sql_array(string: &str) -> Vec<String> {
    if string.is_empty() || string == "||" {
        Vec::new()
    } else {
        string[1..string.len() - 1]
            .split('|')
            .map(String::from)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // format_as_sql_array

    #[test]
    fn test_format_as_sql_array_empty_array_is_formatted_as_empty_string() {
        let strings = [];
        let as_sql_array = format_as_sql_array(&strings);
        assert_eq!(as_sql_array, "");
    }

    #[test]
    fn test_format_as_sql_array_a_single_element_is_surrounded_by_or_bars_and_quotes() {
        let strings = [String::from("Iruña")];
        let as_sql_array = format_as_sql_array(&strings);
        assert_eq!(as_sql_array, "|Iruña|");
    }

    #[test]
    fn test_format_as_sql_array_several_elements_are_separated_by_or_bars() {
        let strings = [
            String::from("Iruña"),
            String::from("Bilbo"),
            String::from("Gasteiz"),
            String::from("Donostia"),
        ];
        let as_sql_array = format_as_sql_array(&strings);
        assert_eq!(as_sql_array, "|Iruña|Bilbo|Gasteiz|Donostia|");
    }

    // read_from_sql_array

    #[test]
    fn test_read_from_sql_array_empty_string_is_read_as_empty_array() {
        let strings = read_from_sql_array("");
        let expected_output: Vec<String> = Vec::new();
        assert_eq!(strings, expected_output);
    }

    #[test]
    fn test_read_from_sql_array_single_element_must_be_surrounded_by_or_bars() {
        let strings = read_from_sql_array("|Iruña|");
        let expected_output = [String::from("Iruña")];
        assert_eq!(strings, expected_output);
    }

    #[test]
    fn test_read_from_sql_array_several_elements_must_be_separated_by_or_bars() {
        let strings = read_from_sql_array("|Iruña|Bilbo|Gasteiz|Donostia|");
        let expected_output = [
            String::from("Iruña"),
            String::from("Bilbo"),
            String::from("Gasteiz"),
            String::from("Donostia"),
        ];
        assert_eq!(strings, expected_output);
    }
}
