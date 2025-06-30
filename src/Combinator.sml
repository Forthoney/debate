structure Arity =
struct
  type t = {low: int, hi: int option}
  val zero = {low = 0, hi = SOME 0}
  val one = {low = 1, hi = SOME 1}
  val zeroOrMore = {low = 0, hi = NONE}
  val oneOrMore = {low = 1, hi = NONE}
  val optional = {low = 0, hi = SOME 1}
end

signature COMBINATOR =
sig
  eqtype token
  type 'a parser = token list -> ('a * token list, string) Result.result

  val terminator: unit parser
  val satisfy: (token -> bool) -> token parser
  val exact: token -> token parser
  val or: 'a parser * 'a parser -> 'a parser
  val andThen: 'a parser * 'b parser -> 'b parser
  val consumeRange: {hi: int option, low: int} -> token list parser
end

local open Result
in
  structure Combinator: COMBINATOR =
  struct
    type token = string
    type 'a parser = token list -> ('a * token list, string) result

    fun terminator [] = Ok ((), [])
      | terminator _ = Err "endOfArg"

    fun satisfy _ [] = Err "empty"
      | satisfy predicate (hd :: rest) =
          if predicate hd then Ok (hd, rest) else Err "predicate"

    fun exact s =
      satisfy (fn arg => arg = s)

    fun or (fst, snd) xs =
      case fst xs of
        Ok res => Ok res
      | Err _ => snd xs

    fun andThen (fst, snd) xs =
      case fst xs of
        Ok res => snd xs
      | Err e => Err e

    fun consumeRange {hi = NONE, low} xs =
          if length xs < low then Err "not enough" else Ok (xs, [])
      | consumeRange {hi = SOME hi, low} xs =
          let
            fun loop acc cnt xs =
              case (Int.compare (hi, cnt), xs) of
                (GREATER, x :: xs) => loop (x :: acc) (cnt + 1) xs
              | (GREATER, []) =>
                  if cnt < low then Err "not enough" else Ok (rev acc, xs)
              | (EQUAL, _) => Ok (rev acc, xs)
              | (LESS, _) => Err "unreachable"
          in
            loop [] 0 xs
          end
  end
end
