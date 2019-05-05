#[derive(Clone, Debug, Eq, PartialEq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

pub fn literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

#[cfg(test)]
mod tests {
    use crate::literal;

    #[test]
    fn literal0() {
        let parser = literal("a");
        assert_eq!(parser("a str"), Ok((" str", ())));
    }

    #[test]
    fn literal1() {
        let parser = literal("prefix");
        assert_eq!(parser("prefix"), Ok(("", ())));
    }

    #[test]
    fn literal2() {
        let parser = literal("long prefix");
        assert_eq!(parser("long prefix with stuff"), Ok((" with stuff", ())));
    }

    #[test]
    fn literal3() {
        let parser = literal("begins");
        assert_eq!(parser("doesn't really begin"), Err("doesn't really begin"));
    }
}
