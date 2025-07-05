structure Command =
struct
  fun new flags =
    let
      val helpMsg = String.concatWith "\n" (map Flag.toHelp flags)
      val help =
        { name = "--help"
        , alias = []
        , help = "show help"
        , args = Parser.Zero (fn () => print (helpMsg ^ "\n"))
        }
      open Combinator
      infix or
    in
      (fmap ignore o repeat o or' o map Flag.toCombinator) flags
      or Flag.toCombinator help
    end
end
