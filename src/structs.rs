/// A field of a structure
/// Grammar:
/// struct_field = {
///     "pub"? ~ identifier ~ ":" ~ identifier
/// }
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Field<'a> {
    /// Whether or not this field is public
    is_public: bool,
    /// The name of this field
    name: &'a str,
    /// The type of this field
    typ: &'a str,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructDefinition<'a> {
    /// Whether or not this field is public
    is_public: bool,
    /// The name of this struct
    name: &'a str,
    /// The fields of this struct
    fields: Vec<Field<'a>>,
}

/// Parses the definition of a struct
/// 
/// Grammar:
/// ```text
/// struct_definition = {
///     "pub"? ~ "struct" ~ identifier ~ "{" ~ (struct_field ~ ",")* ~ struct_field? ~ "}"
/// }```
/// 
pub fn parse_struct_definition() {

}