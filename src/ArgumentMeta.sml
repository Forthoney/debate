structure ArgumentMeta =
struct
  type 'a parser = string -> 'a Action.t Combinator.parser
  type 'a arg = {metavar: string, parser: 'a parser}
end
