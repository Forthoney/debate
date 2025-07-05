structure Command =
struct
  fun new flags =
    let
      val helpMsg = String.concatWith "\n" (map Flag.toHelp flags)
      val help =
        { name = "--help"
        , short = NONE
        , help = "show help"
        , args = Flag.Zero (fn () => print (helpMsg ^ "\n"))
        }
      open Combinator
      infix or
    in
      (fmap ignore o repeat o or' o map Flag.toCombinator) flags
      or Flag.toCombinator help
    end
end

val config = {a = ref false, b = ref false}
fun mut prop () =
  (prop config) := true

val version =
  { name = "--version"
  , short = SOME #"v"
  , help = "show version"
  , args = Flag.Zero (mut #a)
  }
val thing =
  { name = "--thing"
  , short = SOME #"t"
  , help = "thing"
  , args = Flag.Zero (mut #b)
  }
val it = Command.new [version, thing] ["--version", "--thing"]
val _ = print (Bool.toString (! (#a config)))
val _ =
  case it of
    Result.Ok res => print "success\n"
  | Result.Err _ => print "fail\n"
