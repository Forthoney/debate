structure BaseArgument =
struct
  datatype 'a arity =
    Zero of unit -> 'a option
  | Optional of string option -> 'a option
  | AtLeastOne of string list -> 'a option
  | Any of string list -> 'a option
  | Exactly of int * (string list -> 'a option)
end
