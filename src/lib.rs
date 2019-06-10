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

// Here are the rules for an identifier:
//  -> first character must be an alphabet
//  -> this can be followed by zero or more alphabets, numbers or the literal '-'
pub fn identifier(input: &str) -> Result<(&str, String), &str> {
    let mut chars = input.chars();
    let mut end;

    // if the first character is not an alphabet then we bail
    match chars.next() {
        Some(ch) if ch.is_alphabetic() => end = 0,
        _ => return Err(input),
    }

    // keep going till we have alphanumerics or the literal '-'
    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            end = end + 1;
        } else {
            break;
        }
    }
    end = end + 1;

    Ok((&input[end..], String::from(&input[..end])))
}

pub fn pair<P1, P2, R1, R2>(
    parser1: P1,
    parser2: P2,
) -> impl Fn(&str) -> Result<(&str, (R1, R2)), &str>
where
    P1: Fn(&str) -> Result<(&str, R1), &str>,
    P2: Fn(&str) -> Result<(&str, R2), &str>,
{
    move |input| match parser1(input) {
        Ok((input, result1)) => match parser2(input) {
            Ok((input, result2)) => Ok((input, (result1, result2))),
            Err(input) => Err(input),
        },
        Err(input) => Err(input),
    }
}

pub fn map<P, F, T, U>(parser: P, predicate: F) -> impl Fn(&str) -> Result<(&str, U), &str>
where
    P: Fn(&str) -> Result<(&str, T), &str>,
    F: Fn(T) -> U,
{
    move |input| match parser(input) {
        Ok((input, val)) => Ok((input, predicate(val))),
        Err(input) => Err(input),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pairs() {
        let parser1 = pair(literal("<"), literal(">"));
        assert_eq!(Ok(("", ((), ()))), parser1("<>"));

        let parser2 = pair(literal("<"), identifier);
        assert_eq!(
            Ok((" />", ((), "the-tag".to_string()))),
            parser2("<the-tag />")
        );
        assert_eq!(Err("boo"), parser2("boo"));
    }

    #[test]
    fn identifiers() {
        assert_eq!(
            Ok(("", "an-identifier".to_string())),
            identifier("an-identifier")
        );
        assert_eq!(
            Ok((" than one identifier", "more".to_string())),
            identifier("more than one identifier")
        );
        assert_eq!(
            Err("!Umm, no identifier here"),
            identifier("!Umm, no identifier here")
        );
        assert_eq!(Ok(("", "a".to_string())), identifier("a"));
    }

    #[test]
    fn literals() {
        let parser = literal("a");
        assert_eq!(parser("a str"), Ok((" str", ())));

        let parser = literal("prefix");
        assert_eq!(parser("prefix"), Ok(("", ())));

        let parser = literal("long prefix");
        assert_eq!(parser("long prefix with stuff"), Ok((" with stuff", ())));

        let parser = literal("begins");
        assert_eq!(parser("doesn't really begin"), Err("doesn't really begin"));
    }
}
