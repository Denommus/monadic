module MakeFT
    (Wrapped : Monad.FUNCTOR) (E : sig
      type t
    end) =
struct
  module WrappedInfix = Monad.FunctorInfix (Wrapped)
  open WrappedInfix.Syntax

  type e = E.t

  module ResultFunctor :
    Monad.FUNCTOR with type 'a t = ('a, e) result Wrapped.t = struct
    type 'a t = ('a, e) result Wrapped.t

    let map f x =
      let+ v = x in
      Stdlib.Result.map f v
  end

  include ResultFunctor
  include Monad.FunctorInfix (ResultFunctor)

  let run m = m [@@inline]

  let lift x = x [@@inline]

  let elevate v =
    let+ x = v in
    Ok x
end

module MakeF = MakeFT (Identity)

module MakeAT
    (Wrapped : Monad.APPLICATIVE) (E : sig
      type t
    end) =
struct
  module WrappedInfix = Monad.ApplicativeInfix (Wrapped)
  open WrappedInfix.Syntax
  module Functor = MakeFT (Wrapped) (E)

  type e = Functor.e

  module ResultApplicative :
    Monad.APPLICATIVE with type 'a t = ('a, e) result Wrapped.t = struct
    include Functor.ResultFunctor

    let pure v = Ok v |> Wrapped.pure

    let apply fa xa =
      let+ f = fa and+ x = xa in
      match (f, x) with
      | Ok f', Ok x' -> Ok (f' x')
      | Error e, _ | _, Error e -> Error e
  end

  include ResultApplicative
  include Monad.ApplicativeInfix (ResultApplicative)

  let run = Functor.run

  let lift = Functor.lift

  let elevate = Functor.elevate

  let error e = Error e |> Wrapped.pure

  let ok x = Ok x |> Wrapped.pure
end

module MakeA = MakeAT (Identity)

module MakeT
    (Wrapped : Monad.MONAD) (E : sig
      type t
    end) =
struct
  module WrappedInfix = Monad.MonadInfix (Wrapped)
  open WrappedInfix.Syntax
  module Applicative = MakeAT (Wrapped) (E)

  type e = Applicative.e

  module ResultMonad : Monad.MONAD with type 'a t = ('a, e) result Wrapped.t =
  struct
    include Applicative.ResultApplicative

    let join v =
      let* x = v in
      match x with Ok x' -> x' | Error e -> Wrapped.pure @@ Error e

    let bind m f = join (map f m)
  end

  include ResultMonad

  let elevate = Applicative.elevate

  include Monad.MonadInfix (ResultMonad)

  let run = Applicative.run

  let lift = Applicative.run

  let error = Applicative.error

  let ok = Applicative.ok
end

module Make = MakeT (Identity)
