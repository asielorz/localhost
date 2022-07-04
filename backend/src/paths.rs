pub fn is_single_entry_path(path : &str) -> bool {
    if let Some(p) = path.strip_prefix("/api/texts/") {
        return p.parse::<i64>().is_ok();
    }
    return false;
}

pub fn is_entry_subpath(path : &str, subpath : &str) -> bool 
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

pub fn is_entry_image_path(path : &str) -> bool 
{
    return is_entry_subpath(path, "image");
}

pub fn is_entry_backup_path(path : &str) -> bool 
{
    return is_entry_subpath(path, "backup");
}

pub fn get_entry_id_from_path(path : &str) -> i64 {
    if let Some(p) = path.strip_prefix("/api/texts/") {
        let parts = p.split("/").collect::<Vec<_>>();
        return parts[0].parse::<i64>().unwrap();
    }

    unreachable!();
}

#[cfg(test)]
mod tests 
{
    use super::*;

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
