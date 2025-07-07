signature COMBINATOR =
sig
  eqtype token
  type 'a parser = token list -> ('a * token list, string) Result.result

  val terminator: unit parser
  val noOp: unit parser
  val satisfy: (token -> bool) -> token parser
  val exact: token -> token parser
  val or: 'a parser * 'a parser -> 'a parser
  val or': 'a parser list -> 'a parser
  val bind: 'a parser * ('a -> 'b parser) -> 'b parser
  val andThen: 'a parser * 'b parser -> 'b parser
  val try: 'a parser -> 'a option parser
  val fmap: ('a -> 'b) -> 'a parser -> 'b parser
  val consumeRange: {low: int, hi: int option} -> token list parser
  val repeat: 'a parser -> 'a list parser
end

local open Result
in
  structure Combinator: COMBINATOR =
  struct
    type token = string
    type 'a parser = token list -> ('a * token list, string) result

    fun terminator [] = Ok ((), [])
      | terminator _ = Err "endOfArg"

    fun noOp tokens = Ok ((), tokens)

    fun satisfy _ [] = Err "empty"
      | satisfy predicate (hd :: rest) =
          if predicate hd then Ok (hd, rest) else Err "predicate"

    fun exact s =
      satisfy (fn arg => arg = s)

    fun or' [] xs = Err "or'"
      | or' (fst :: rest) xs =
          case fst xs of
            Ok res => Ok res
          | Err _ => or' rest xs

    fun or (fst, snd) xs =
      case fst xs of
        Ok res => Ok res
      | Err _ => snd xs

    fun bind (fst, snd) xs =
      case fst xs of
        Ok (v, rest) => snd v rest
      | Err e => Err e

    fun andThen (fst, snd) =
      bind (fst, fn _ => snd)

    fun fmap f comb xs =
      case comb xs of
        Ok (v, rest) => Ok (f v, rest)
      | Err e => Err e

    fun try comb xs =
      case comb xs of
        Ok (v, rest) => Ok (SOME v, rest)
      | Err _ => Ok (NONE, xs)

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

    fun repeat comb xs =
      let
        fun loop acc xs =
          case comb xs of
            Ok (v, rest) => loop (v :: acc) rest
          | Err e => Ok (rev acc, xs)
      in
        loop [] xs
      end
  end
end
