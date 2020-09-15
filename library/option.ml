module MakeT (Wrapped : Monad.MONAD) = struct
  module WrappedSyntax = Monad.MonadSyntax (Wrapped)
  open WrappedSyntax

  module OptionMonad : Monad.MONAD with type 'a t = 'a option Wrapped.t = struct
    type 'a t = 'a option Wrapped.t

    let pure v = Some v |> Wrapped.pure

    let map f x =
      let+ v = x in
      Stdlib.Option.map f v

    let apply fa xa =
      let+ f = fa and+ x = xa in
      match (f, x) with Some f', Some x' -> Some (f' x') | _ -> None

    let join v =
      let* x = v in
      match x with Some x' -> x' | _ -> Wrapped.pure None

    let bind m f = join (map f m)
  end

  include OptionMonad

  let elevate v =
    let+ x = v in
    Some x

  include Monad.MonadInfix (OptionMonad)
  module Syntax = Monad.MonadSyntax (OptionMonad)

  let run m = m [@@inline]

  let lift x = x [@@inline]

  let none _ = Wrapped.pure None
end

module Make = MakeT (Identity)
