module MakeT(Wrapped: Monad.MONAD)(R: sig type t end) = struct

  type s = R.t

  module ReaderMonad = Reader.MakeT(Wrapped)(struct type t = R.t ref end)

  include ReaderMonad

  open Syntax

  let put v = let+ x = peek in x := v

  let get = let+ x = peek in !x

  let run m ~init = let r = ref init in
                    let result = run m ~init:r in
                    result, !r

  let lift f = lift (fun r ->
                   let open Monad.MonadSyntax(Wrapped) in
                   let+ (result, new_state) = f !r in
                   r := new_state;
                   result)
end

module Make = MakeT(Identity)
