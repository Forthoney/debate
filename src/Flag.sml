signature FLAG =
sig
  type t = {name: string, alias: string list, help: string, args: Parser.args}

  val toCombinator: t -> unit Combinator.parser
  val toHelp: t -> string
end

structure Flag: FLAG =
struct
  type t = {name: string, alias: string list, help: string, args: Parser.args}

  fun normalize {name, alias, args, help} =
    {name=name, args=args}::map (fn s => {name=s, args=args}) alias
  
  val toCombinator = 
    Combinator.or' o map Parser.toCombinator o normalize

  fun toHelp {name, alias, help, args} =
    "  " ^ String.concatWith ", " (name::alias) ^ "\t" ^ help
end
