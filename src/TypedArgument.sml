signature TYPED_ARGUMENT =
sig
  type src
  exception Conversion of {name: string, cause: string}
  val optional: string * (src option -> 'a option) -> 'a ArgumentMeta.arg
  val optionalWithDefault: string * (src -> 'a option) * src
                           -> 'a ArgumentMeta.arg
  val one: string * (src -> 'a option) -> 'a ArgumentMeta.arg
end

functor TypedArgumentFn
  (Cvt:
   sig
     type t
     val fromString: string -> t option
   end): TYPED_ARGUMENT =
struct
  type src = Cvt.t
  exception Conversion of {name: string, cause: string}

  open Combinator

  fun optional (varName, f) =
    let
      fun helper name NONE () = Action.unwrap (name, NONE) (f NONE)
        | helper name (SOME v) () =
          case Cvt.fromString v of
            SOME v' => Action.unwrap (name, SOME v) (f (SOME v'))
          | NONE => raise Conversion {name=name, cause=v}
    in
    { metavar = "[" ^ varName ^ "]"
    , parser = fn name => fmap (helper name) (try (satisfy (not o String.isPrefix "-")))
    }
    end

  fun optionalWithDefault (varName, f, default) =
    optional (varName, fn v => f (Option.getOpt (v, default)))

  fun one (varName, f) =
    { metavar = varName
    , parser = fn name =>
        fmap
          (fn v =>
             fn () =>
               (Action.unwrap (name, SOME v)
                o Option.composePartial (f, Cvt.fromString)) v)
          (satisfy (not o String.isPrefix "-"))
    }
end
