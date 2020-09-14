module MakeT
    (Wrapped : Monad.MONAD) (R : sig
      type t
    end) =
struct
  type r = R.t

  module WrappedSyntax = Monad.MonadSyntax (Wrapped)
  open WrappedSyntax

  module ReaderMonad : Monad.MONAD with type 'a t = r -> 'a Wrapped.t = struct
    type 'a t = r -> 'a Wrapped.t

    let pure v _ = Wrapped.pure v

    let map f x r =
      let+ result = x r in
      f result

    let apply fa va r =
      let+ f = fa r and+ v = va r in
      f v

    let bind m k r =
      let* v = m r in
      k v r

    let join m = bind m (fun x -> x)
  end

  include ReaderMonad

  let elevate w _ = w

  include Monad.MonadInfix (ReaderMonad)
  module Syntax = Monad.MonadSyntax (ReaderMonad)

  let peek r = Wrapped.pure r

  let run m ~init = m init

  let lift x = x [@@inline]
end

module Make = MakeT (Identity)
