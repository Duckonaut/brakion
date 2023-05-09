use crate::unit::READ_INCREMENT;
use crate::Unit;

#[test]
fn utf8_misalignment_boundary() {
    let mut s = String::new();
    for _ in 0..(READ_INCREMENT - 2) {
        s.push('a');
    }

    let mut bytes = Vec::from(s.as_bytes());

    assert_eq!(bytes.len(), READ_INCREMENT - 2);

    // Add a 4-byte UTF-8 sequence
    bytes.push(0xF0);
    bytes.push(0x90);
    bytes.push(0x80);
    bytes.push(0x80);

    assert_eq!(bytes.len(), READ_INCREMENT + 2);

    bytes.push(b'a');

    assert_eq!(bytes.len(), READ_INCREMENT + 3);

    let bytes_reader = std::io::Cursor::new(bytes);

    let mut unit = Unit::new("test".to_string(), 0, Box::new(bytes_reader));

    for i in 0..(READ_INCREMENT - 2) {
        println!("{:x}", i);
        assert_eq!(unit.read(), Some('a'));
    }

    let c = unit.read().unwrap();

    assert_eq!(c.len_utf8(), 4);

    dbg!(&unit);

    assert_eq!(unit.read(), Some('a'));
}

#[test]
#[should_panic]
fn utf8_misalignment_end() {
    let mut s = String::new();
    for _ in 0..(READ_INCREMENT - 2) {
        s.push('a');
    }

    let mut bytes = Vec::from(s.as_bytes());

    assert_eq!(bytes.len(), READ_INCREMENT - 2);

    // Add a 4-byte UTF-8 sequence
    bytes.push(0xF0);
    bytes.push(0x90);

    assert_eq!(bytes.len(), READ_INCREMENT);

    let bytes_reader = std::io::Cursor::new(bytes);

    let mut unit = Unit::new("test".to_string(), 0, Box::new(bytes_reader));

    for i in 0..(READ_INCREMENT - 2) {
        println!("{:x}", i);
        assert_eq!(unit.read(), Some('a'));
    }

    let _ = unit.read();
}
