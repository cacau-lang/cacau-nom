use nom::{IResult, branch, bytes::complete, character::complete::{alphanumeric1, digit1, multispace0}, combinator::{not, opt, recognize, value}, error::ParseError, multi::many0, sequence::{self, delimited, pair}};

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
    expression: Expression<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression<'a> {
    Identifier(&'a str),
    /// Binding of a value to a string
    /// E.g.: `let x = 5;`  
    Assignment(Box<Assignment<'a>>),
    /// A boolean value, either true or false
    /// 
    Boolean(bool),
    Integer(i64),
}

impl<'a> Expression<'a> {
    /// Returns the inner identifier of this expression.
    /// Panics if the expression variant is _not_ Identifier
    pub fn identifier(&self) -> &str {
        match self {
            Expression::Identifier(ident) => ident,
            _ => panic!("called Expression::identifier on non-Identifier variant"),
        }
    }
}

pub fn parse_expression<'a>(input: &'a str) -> IResult<&str, Expression<'a>> {
    branch::alt((
        parse_bool,
        parse_integer,
        parse_assignment,
        parse_identifier,
    ))(input)
}

/// Stolen from nom Recipes
/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    sequence::delimited(multispace0, inner, multispace0)
}

/// Parses a type annotation of the form:
/// `[pest] type_annotation = { ":" ~ identifier }`
/// Returns the type identifier only
pub fn parse_type_annotation<'a>(input: &'a str) -> IResult<&str, &str> {
    // Parse the colon (':')
    let (remainder, _colon) = ws(complete::tag(":"))(input)?;

    // Parse the identifier
    let (remainder, type_annotation) = ws(parse_identifier_str)(remainder)?;

    Ok((remainder, type_annotation))
}

/// Parses an assignment of the form
/// assignment = {
///	   "let" ~ identifier ~ type_annotation? ~ "=" ~ expression
/// }
/// (WIP)
pub fn parse_assignment<'a>(input: &'a str) -> IResult<&str, Expression<'a>> {
    // Parsing the "let" keyword
    let (remainder, _let) = ws(complete::tag("let"))(input)?;

    // Parsing the identifier for this assignment
    let (remainder, identifier) = ws(parse_identifier_str)(remainder)?;

    // Parse the type annotation, if it exists
    let (remainder, maybe_type_annotation) = opt(parse_type_annotation)(remainder)?;

    // Parsing the "=" symbol
    let (remainder, _let) = ws(complete::tag("="))(remainder)?;

    // Parse the expression being bound
    let (remainder, expression) = parse_expression(remainder)?;

    let assignment = Box::new(Assignment {
        // Safety: the Expressioon returned by `parse_identifier` is always of the variant `Identifier`
        name: identifier,
        type_annotation: maybe_type_annotation,
        expression,
    });

    dbg!(remainder, &identifier, maybe_type_annotation);

    Ok((remainder, Expression::Assignment(assignment)))
}

fn parse_raw_bool(input: &str) -> IResult<&str, bool> {
    // Returns true if the given input is "true"
    let parse_true = value(true, complete::tag("true"));

    // Returns false if the given input is "false"
    let parse_false = value(false, complete::tag("false"));

    branch::alt((parse_true, parse_false))(input)
}

pub fn parse_bool<'a>(input: &'a str) -> IResult<&str, Expression<'a>> {
    let (remainder, parsed_bool) = parse_raw_bool(input)?;

    Ok((remainder, Expression::Boolean(parsed_bool)))
}

pub fn parse_integer<'a>(input: &'a str) -> IResult<&str, Expression<'a>> {
    // recognize will return the matched string of the underlying parser
    let (tail, parsed_int) = recognize(
        // pair returns a tuple of the result of both parsers
        pair(
            // Either a minus or a sequence of digits
            branch::alt((complete::tag("-"), digit1)),
            // A sequence of digits or nothing
            many0(digit1),
        ),
    )(input)?;

    // TODO: make this an error whenever we set up custom error treatment
    let parsed_int = parsed_int.parse().expect("Number too large!");

    Ok((tail, Expression::Integer(parsed_int)))
}

/// Parses an alphanumeric identifier whose name does not start with a digit
pub fn parse_identifier_str<'a>(input: &'a str) -> IResult<&str, &'a str> {
    let (tail, identifier) = recognize(pair(
        // Make sure that the identifier does not _start_ with a digit
        not(digit1),
        // Get an alphanumeric sequence of at least one element
        alphanumeric1,
    ))(input)?;

    Ok((tail, identifier))
}

/// Parses an alphanumeric identifier whose name does not start with a digit
pub fn parse_identifier<'a>(input: &'a str) -> IResult<&str, Expression<'a>> {
    let (tail, identifier) = parse_identifier_str(input)?;

    Ok((tail, Expression::Identifier(identifier)))
}

#[cfg(test)]
mod tests {

    use crate::{Assignment, Expression, parse_assignment, parse_bool, parse_identifier, parse_integer, parse_raw_bool, parse_type_annotation};

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

        assert_eq!(parse_integer("0"), Ok(("", Expression::Integer(0))));

        assert_eq!(parse_integer("-24"), Ok(("", Expression::Integer(-24))));
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

        assert_eq!(parse_bool("false"), Ok(("", Expression::Boolean(false))));

        assert_eq!(parse_bool("true"), Ok(("", Expression::Boolean(true))));

        let wrong_bools = ["tru", "fals", "truth", "True", "False"];
        for wrong in wrong_bools {
            parse_bool(wrong).unwrap_err();
        }
    }

    #[test]
    fn parses_raw_bool() {
        assert_eq!(parse_raw_bool("true false"), Ok((" false", true)));

        assert_eq!(parse_raw_bool("false true"), Ok((" true", false)));

        assert_eq!(parse_raw_bool("false"), Ok(("", false)));

        assert_eq!(parse_raw_bool("true"), Ok(("", true)));

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
    fn parses_assignment() {

        let assign_5_to_x = Assignment {
            name: "x",
            type_annotation: None,
            expression: Expression::Integer(5),
        };

        assert_eq!(
            parse_assignment("let x = 5"),
            Ok(("", Expression::Assignment(Box::new(assign_5_to_x))))
        );

        let assign_8_to_y = Assignment {
            name: "y",
            type_annotation: Some("int"),
            expression: Expression::Integer(8),
        };

        assert_eq!(
            parse_assignment("let y: int = 8"),
            Ok(("", Expression::Assignment(Box::new(assign_8_to_y.clone()))))
        );

        // Experimenting with whitespace
        assert_eq!(
            parse_assignment("let y:int = 8"),
            Ok(("", Expression::Assignment(Box::new(assign_8_to_y.clone()))))
        );

        assert_eq!(
            parse_assignment("let y:int= 8"),
            Ok(("", Expression::Assignment(Box::new(assign_8_to_y.clone()))))
        );

        assert_eq!(
            parse_assignment("let y:int=8"),
            Ok(("", Expression::Assignment(Box::new(assign_8_to_y.clone()))))
        );
    }

    #[test]
    fn parses_type_annotation() {
        assert_eq!(parse_type_annotation(": int"), Ok(("", "int")));

        assert!(parse_type_annotation(":: int").is_err(),);

        assert!(parse_type_annotation("int").is_err(),);
    }
}

fn main() {
    dbg!(parse_assignment("let x = 5"));

    dbg!(parse_assignment("let x: int = 5"));
}
