pub type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

pub fn literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

// Here are the rules for an identifier:
//  -> first character must be an alphabet
//  -> this can be followed by zero or more alphabets, numbers or the literal '-'
pub fn identifier(input: &str) -> ParseResult<String> {
    let mut chars = input.chars();
    let mut end;

    // if the first character is not an alphabet then we bail
    match chars.next() {
        Some(ch) if ch.is_alphabetic() => end = 0,
        _ => return Err(input),
    }

    // keep going till we have alphanumerics or the literal '-'
    for next in chars {
        if next.is_alphanumeric() || next == '-' {
            end += 1;
        } else {
            break;
        }
    }
    end += 1;

    let r = Ok((&input[end..], String::from(&input[..end])));
    dbg!(&r);
    r
}

pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(input, result1)| {
            parser2
                .parse(input)
                .map(|(input, result2)| (input, (result1, result2)))
        })
    }
}

pub fn map<'a, P, F, T, U>(parser: P, predicate: F) -> impl Parser<'a, U>
where
    P: Parser<'a, T>,
    F: Fn(T) -> U,
{
    move |input| {
        parser
            .parse(input)
            .map(|(input, val)| (input, predicate(val)))
    }
}

pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(result, _)| result)
}

pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, result)| result)
}

pub fn zero_or_more<'a, P, R>(parser: P) -> impl Parser<'a, Vec<R>>
where
    P: Parser<'a, R>,
{
    move |input| {
        let mut tokens = vec![];
        let mut remaining = input;
        while let Ok((input, result)) = parser.parse(remaining) {
            tokens.push(result);
            remaining = input;
        }

        Ok((remaining, tokens))
    }
}

pub fn one_or_more<'a, P, R>(parser: P) -> impl Parser<'a, Vec<R>>
where
    P: Parser<'a, R>,
{
    let parser = zero_or_more(parser);
    move |input| {
        let (input, tokens) = parser.parse(input)?;
        if tokens.is_empty() {
            Err(input)
        } else {
            Ok((input, tokens))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_or_mores() {
        let parser = one_or_more(literal("<"));
        assert_eq!(Err("nothing to parse"), parser.parse("nothing to parse"));
        assert_eq!(Ok(("boo", vec![()])), parser.parse("<boo"));
        assert_eq!(Ok(("boo", vec![(); 4])), parser.parse("<<<<boo"));

        let parser = one_or_more(left(identifier, zero_or_more(literal(" "))));
        assert_eq!(Err("!no identifiers"), parser.parse("!no identifiers"));
        assert_eq!(
            Ok((
                "",
                vec![
                    "the".to_owned(),
                    "three".to_owned(),
                    "identifiers".to_owned()
                ]
            )),
            parser.parse("the      three   identifiers")
        );
        assert_eq!(
            Ok((
                "",
                vec![
                    "the".to_owned(),
                    "three".to_owned(),
                    "identifiers".to_owned()
                ]
            )),
            parser.parse("the      three   identifiers     ")
        );
    }

    #[test]
    fn zero_or_mores() {
        let parser = zero_or_more(literal("<"));
        assert_eq!(
            Ok(("nothing to parse", vec![])),
            parser.parse("nothing to parse")
        );
        assert_eq!(Ok(("boo", vec![()])), parser.parse("<boo"));
        assert_eq!(Ok(("boo", vec![(); 4])), parser.parse("<<<<boo"));

        let parser = zero_or_more(left(identifier, zero_or_more(literal(" "))));
        assert_eq!(
            Ok(("!no identifiers", vec![])),
            parser.parse("!no identifiers")
        );
        assert_eq!(
            Ok((
                "",
                vec![
                    "the".to_owned(),
                    "three".to_owned(),
                    "identifiers".to_owned()
                ]
            )),
            parser.parse("the      three   identifiers")
        );
        assert_eq!(
            Ok((
                "",
                vec![
                    "the".to_owned(),
                    "three".to_owned(),
                    "identifiers".to_owned()
                ]
            )),
            parser.parse("the      three   identifiers     ")
        );
    }

    #[test]
    fn rights() {
        let parser = right(literal("<"), identifier);
        assert_eq!(Ok(("/>", "the-tag".to_owned())), parser.parse("<the-tag/>"));
    }

    #[test]
    fn lefts() {
        let parser = left(literal("prefix"), literal(".suffix"));
        assert_eq!(Ok(("", ())), parser.parse("prefix.suffix"));

        let parser = left(identifier, literal(".suffix"));
        assert_eq!(Ok(("", "boo".to_owned())), parser.parse("boo.suffix"));
        assert_eq!(Err("!boo.suffix"), parser.parse("!boo.suffix"));
        assert_eq!(Err(".notsuffix"), parser.parse("boo.notsuffix"));
    }

    #[test]
    fn maps() {
        let parser = map(literal("a"), |_| "ooh, nice");
        assert_eq!(Ok(("", "ooh, nice")), parser.parse("a"));

        let parser = map(literal("b"), |_| 10i32);
        assert_eq!(Ok(("", 10)), parser.parse("b"));
        assert_eq!(Err("a"), parser.parse("a"));
    }

    #[test]
    fn pairs() {
        let parser1 = pair(literal("<"), literal(">"));
        assert_eq!(Ok(("", ((), ()))), parser1.parse("<>"));

        let parser2 = pair(literal("<"), identifier);
        assert_eq!(
            Ok((" />", ((), "the-tag".to_string()))),
            parser2.parse("<the-tag />")
        );
        assert_eq!(Err("boo"), parser2.parse("boo"));
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
        assert_eq!(parser.parse("a str"), Ok((" str", ())));

        let parser = literal("prefix");
        assert_eq!(parser.parse("prefix"), Ok(("", ())));

        let parser = literal("long prefix");
        assert_eq!(
            parser.parse("long prefix with stuff"),
            Ok((" with stuff", ()))
        );

        let parser = literal("begins");
        assert_eq!(
            parser.parse("doesn't really begin"),
            Err("doesn't really begin")
        );
    }
}
