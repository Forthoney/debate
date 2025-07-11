signature VALIDATED_ARGUMENT =
sig
  type t
  val optional: (t option -> 'a option)
                -> string
                -> 'a Action.t Combinator.parser
  val optionalWithDefault: (t -> 'a option) * t
                           -> string
                           -> 'a Action.t Combinator.parser
  val one: (t -> 'a option) -> string -> 'a Action.t Combinator.parser
end

functor TypedArgumentFn
  (Cvt:
   sig
     type t
     val fromString: string -> t option
   end): VALIDATED_ARGUMENT =
struct
  type t = Cvt.t

  open Combinator

  fun optional f name =
    fmap
      (fn v =>
         fn () =>
           (Action.unwrap (name, v) o f
            o Option.mapPartial Cvt.fromString) v)
      (try (satisfy (not o String.isPrefix "-")))

  fun optionalWithDefault (f, default) =
    optional (fn v => f (Option.getOpt (v, default)))

  fun one f name =
    fmap
      (fn v =>
         fn () =>
           (Action.unwrap (name, SOME v)
            o Option.composePartial (f, Cvt.fromString)) v)
      (satisfy (not o String.isPrefix "-"))
end
