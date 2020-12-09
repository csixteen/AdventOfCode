use std::fs::File;
use std::io::Read;


pub fn get_file_contents(name: &str) -> std::io::Result<Vec<String>> {
    let mut buffer = String::new();
    let mut file = File::open(name)?;

    file.read_to_string(&mut buffer).unwrap();
    
    Ok(buffer.trim().split("\n").map(|x| String::from(x)).collect())
}
