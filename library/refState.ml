module MakeT(Wrapped: Monad.MONAD)(R: sig type t end) = struct

  type s = R.t

  module ReaderMonad = Reader.MakeT(Wrapped)(struct type t = R.t ref end)

  include ReaderMonad

  open Syntax

  let put v = let+ x = peek in x := v

  let get = let+ x = peek in !x
end

module Make = MakeT(Identity)
