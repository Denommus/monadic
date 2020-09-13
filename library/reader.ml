module MakeT(Wrapped: Monad.MONAD)(R: sig type t end) = struct
  type r = R.t

  module WrappedSyntax = Monad.MonadSyntax(Wrapped)

  open WrappedSyntax

  module ReaderMonad: Monad.MONAD with type 'a t = r -> 'a Wrapped.t = struct

    type 'a t = r -> 'a Wrapped.t

    let pure v = fun _ -> Wrapped.pure v

    let map f x = fun r ->
      let+ result = x r in
      f result

    let apply fa va = fun r ->
      let+ f = fa r
      and+ v = va r
      in f v

    let bind m k = fun r ->
      let* v = m r in k v r

    let join m = bind m (fun x -> x)
  end

  include ReaderMonad

  let elevate w = fun _ -> w

  include Monad.MonadInfix(ReaderMonad)

  module Syntax = Monad.MonadSyntax(ReaderMonad)

  let peek = fun r -> Wrapped.pure r

  let runReader m ~init = m init
end

module Make = MakeT(Identity)
