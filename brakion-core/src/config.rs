#[derive(Debug, Clone)]
pub struct Config {
    pub max_string_length: usize,
    pub max_identifier_length: usize,
    pub max_number_length: usize,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            max_string_length: 4096,
            max_identifier_length: 1024,
            max_number_length: 1024,
        }
    }
}
