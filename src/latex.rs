use nom::{
    bytes::{tag, take_until1},
    character::complete::space0,
    combinator::map,
    error::Error,
    Parser,
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Includes {
    files: HashMap<String, HashMap<usize, String>>,
    include_commands: Vec<String>,
}

impl Includes {
    pub fn new(include_commands: Vec<String>) -> Self {
        Self {
            files: Default::default(),
            include_commands,
        }
    }

    pub fn get_include(&mut self, filename: impl ToString, line_num: usize) -> Option<&String> {
        self.files
            .entry(filename.to_string())
            .or_insert_with_key(|f| Self::read_file(&self.include_commands, f))
            .get(&line_num)
    }

    fn read_file(commands: &[String], filename: &str) -> HashMap<usize, String> {
        let content = std::fs::read_to_string(filename)
            .unwrap_or_else(|e| panic!("Cannot read file {filename}: {e}"));
        let mut result = HashMap::new();
        for (i, line) in content.lines().enumerate() {
            let line_num = i + 1;

            // try to match any of the commands
            if let Some(file) = commands.iter().find_map(|c| get_include_file(line, c)) {
                result.insert(line_num, file.to_string());
            }
        }
        result
    }
}

fn get_include_file(line: &str, command: &str) -> Option<String> {
    match include_parser(command).parse(line) {
        Ok((_, file)) => Some(file.to_string()),
        Err(_) => None,
    }
}

fn include_parser(command: &str) -> impl Parser<&str, Output = &str, Error = Error<&str>> {
    map(
        (
            space0,
            tag("\\"),
            tag(command),
            tag("{"),
            take_until1("}"),
            tag("}"),
        ),
        |(_, _, _, _, name, _)| name,
    )
}
