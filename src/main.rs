use nom::{Err, IResult, Parser, branch::alt, bytes::complete::{tag}, character::{complete::{alphanumeric1, digit1, multispace0}}, combinator::{not, opt, recognize, value}, error::{Error, ErrorKind, ParseError}, multi::many0, sequence::{delimited, pair}};


/// Represents a value assignment
/// Grammar:
/// assignment = {
///	   "let" ~ identifier ~ type_annotation? ~ "=" ~ expression
/// }
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assignment<'a> {
    /// The name the value was assigned to
    name: &'a str,
    /// The type of the variable, if supplied
    type_annotation: Option<&'a str>,
    /// The value that was assigned to `name`
    expression: Expression<'a>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression<'a> {
    Identifier(&'a str),
    /// Binding of a value to a string
    /// E.g.: `let x = 5;`  
    Binding(Box<Assignment<'a>>),
    Boolean(bool),
    Integer(i64)
}

/// Stolen from nom Recipes
/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and 
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
  where
  F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
  delimited(
    multispace0,
    inner,
    multispace0
  )
}

/// Parses a type annotation of the form:
/// `[pest] type_annotation = { ":" ~ identifier }`
/// Returns the type identifier only
pub fn parse_type_annotation<'a>(input: &'a str) -> IResult<&str, Expression<'a>> {
    // Parse the colon (':')
    let (remainder, _colon) = ws(tag(":"))(input)?;
    
    // Parse the identifier
    let (remainder, type_annotation) = ws(parse_identifier)(remainder)?;

    Ok(
        (remainder, type_annotation)
    )
}

/// Parses an assignment of the form
/// assignment = {
///	   "let" ~ identifier ~ type_annotation? ~ "=" ~ expression
/// }
/// (WIP)
pub fn parse_assignment<'a>(input: &'a str) -> IResult<&str, Expression<'a>> {
    // Parsing the "let" keyword
    let (remainder, _let) = ws(tag("let"))(input)?;

    // Parsing the identifier for this assignment
    let (remainder, identifier) = ws(parse_identifier)(remainder)?;

    // Parse the type annotation, if it exists
    let (remainder, maybe_type_annotation) = opt(parse_type_annotation)(remainder)?;

    // Parsing the "=" symbol
    let (remainder, _let) = ws(tag("="))(remainder)?;

    // Parse the expression being bound
    // TODO

    dbg!(remainder, &identifier, maybe_type_annotation);

    Ok((remainder, identifier))
}

fn parse_raw_bool(input: &str) -> IResult<&str, bool> {
    // Returns true if the given input is "true"
    let parse_true = value(true, tag("true"));

    // Returns false if the given input is "false"
    let parse_false = value(false, tag("false"));

    alt(
        (parse_true, parse_false)
    )(input)
}

pub fn parse_bool<'a>(input: &'a str) -> IResult<&str, Expression<'a>> {
    let (remainder, parsed_bool) = parse_raw_bool(input)?;

    Ok(
        (remainder, Expression::Boolean(parsed_bool))
    )
}

pub fn parse_integer<'a>(input: &'a str) -> IResult<&str, Expression<'a>> {

    // recognize will return the matched string of the underlying parser
    let (tail, parsed_int) = recognize(
        // pair returns a tuple of the result of both parsers
        pair(
            // Either a minus or a sequence of digits
            alt((tag("-"), digit1)),
            // A sequence of digits or nothing
            many0(digit1)
        )
    )(input)?;

    // TODO: make this an error whenever we set up custom error treatment
    let parsed_int = parsed_int.parse().expect("Number too large!");

    Ok(
        (tail, Expression::Integer(parsed_int))
    )
}

/// Parses an alphanumeric identifier whose name starts with 
pub fn identifier_parser<'a, E: ParseError<&'a str>>() -> impl Parser<&'a str, &'a str, E> {
    recognize(
        pair(
            // Make sure that the identifier does not _start_ with a digit
            not(digit1),
            // Get an alphanumeric sequence of at least one element
            alphanumeric1
        )
    )   
}

/// Parses an alphanumeric identifier whose name starts with 
pub fn parse_identifier<'a>(input: &'a str) -> IResult<&str, Expression<'a>> {

    let (tail, identifier) = identifier_parser().parse(input)?;

    Ok((tail, Expression::Identifier(identifier)))
}

#[cfg(test)]
mod tests {

    use nom::Parser;

    use crate::{Expression, identifier_parser, parse_bool, parse_identifier, parse_integer, parse_raw_bool, parse_type_annotation};

    #[test]
    fn parses_integer() {

        assert_eq!(
            parse_integer("123456789 false"),
            Ok((" false", Expression::Integer(123456789)))
        );

        assert_eq!(
            parse_integer("9999999999 false"),
            Ok((" false", Expression::Integer(9999999999)))
        );

        assert_eq!(
            parse_integer("0"),
            Ok(("", Expression::Integer(0)))
        );

        assert_eq!(
            parse_integer("-24"),
            Ok(("", Expression::Integer(-24)))
        );
    }

    #[test]
    fn parses_bool() {
        assert_eq!(
            parse_bool("true false"),
            Ok((" false", Expression::Boolean(true)))
        );

        assert_eq!(
            parse_bool("false true"),
            Ok((" true", Expression::Boolean(false)))
        );

        assert_eq!(
            parse_bool("false"),
            Ok(("", Expression::Boolean(false)))
        );

        assert_eq!(
            parse_bool("true"),
            Ok(("", Expression::Boolean(true)))
        );

        let wrong_bools = ["tru", "fals", "truth", "True", "False"];
        for wrong in wrong_bools {
            parse_bool(wrong).unwrap_err();
        }
    }

    #[test]
    fn parses_raw_bool() {
        assert_eq!(
            parse_raw_bool("true false"),
            Ok((" false", true))
        );

        assert_eq!(
            parse_raw_bool("false true"),
            Ok((" true", false))
        );

        assert_eq!(
            parse_raw_bool("false"),
            Ok(("", false))
        );

        assert_eq!(
            parse_raw_bool("true"),
            Ok(("", true))
        );

        // TODO: what about "truee" and "falsee" ?
        let wrong_bools = ["tru", "fals", "truth", "True", "False"];
        for wrong in wrong_bools {
            parse_raw_bool(wrong).unwrap_err();
        }

    }

    #[test]
    fn parses_valid_identifier() {
        let (tail, identifier) = parse_identifier("username").unwrap();

        assert_eq!(tail, "");
        assert_eq!(identifier, Expression::Identifier("username"));
    }

    #[test]
    fn parses_valid_identifier_prepended_text() {
        let (tail, identifier) = parse_identifier("username othername").unwrap();
        

        assert_eq!(tail, " othername");
        assert_eq!(identifier, Expression::Identifier("username"));
    }


    #[test]
    fn parses_identifier_starting_with_digit_fails() {
        let err = parse_identifier("123username").unwrap_err();

        let err = err.to_string();

        assert_eq!(
            err,
            "Parsing Error: Error { input: \"123username\", code: Not }"
        )
    }

    #[test]
    fn parses_type_annotation() {
        assert_eq!(
            parse_type_annotation(": int"),
            Ok(("", Expression::Identifier("int")))
        );

        assert!(
            parse_type_annotation(":: int").is_err(),
        );

        assert!(
            parse_type_annotation("int").is_err(),
        );
    }
}

fn main() {
    parse_assignment("let hm = 4");

    parse_assignment("let hm: int = 4");
}
