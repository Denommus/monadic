module MakeT
    (Wrapped : Monad.MONAD) (E : sig
      type t
    end) =
struct
  type e = E.t

  module WrappedInfix = Monad.MonadInfix (Wrapped)
  open WrappedInfix.Syntax

  module ResultMonad : Monad.MONAD with type 'a t = ('a, e) result Wrapped.t =
  struct
    type 'a t = ('a, e) result Wrapped.t

    let pure v = Ok v |> Wrapped.pure

    let map f x =
      let+ v = x in
      Stdlib.Result.map f v

    let apply fa xa =
      let+ f = fa and+ x = xa in
      match (f, x) with
      | Ok f', Ok x' -> Ok (f' x')
      | Error e, _ | _, Error e -> Error e

    let join v =
      let* x = v in
      match x with Ok x' -> x' | Error e -> Wrapped.pure @@ Error e

    let bind m f = join (map f m)
  end

  include ResultMonad

  let elevate v =
    let+ x = v in
    Ok x

  include Monad.MonadInfix (ResultMonad)

  let run m = m [@@inline]

  let create x = x [@@inline]

  let error x = Error x |> Wrapped.pure
end

module Make = MakeT (Identity)
