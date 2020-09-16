module MakeFT (Wrapped : Monad.FUNCTOR) = struct
  module WrappedInfix = Monad.FunctorInfix (Wrapped)
  open WrappedInfix.Syntax

  module OptionFunctor : Monad.FUNCTOR with type 'a t = 'a option Wrapped.t =
  struct
    type 'a t = 'a option Wrapped.t

    let map f x =
      let+ v = x in
      Stdlib.Option.map f v
  end

  include OptionFunctor
  include Monad.FunctorInfix (OptionFunctor)

  let run m = m [@@inline]

  let lift x = x [@@inline]

  let elevate v =
    let+ x = v in
    Some x
end

module MakeF = MakeFT (Identity)

module MakeAT (Wrapped : Monad.APPLICATIVE) = struct
  module WrappedInfix = Monad.ApplicativeInfix (Wrapped)
  open WrappedInfix.Syntax
  module Functor = MakeFT (Wrapped)

  module OptionApplicative :
    Monad.APPLICATIVE with type 'a t = 'a option Wrapped.t = struct
    include Functor.OptionFunctor

    let pure v = Some v |> Wrapped.pure

    let apply fa xa =
      let+ f = fa and+ x = xa in
      match (f, x) with Some f', Some x' -> Some (f' x') | _ -> None
  end

  include OptionApplicative
  include Monad.ApplicativeInfix (OptionApplicative)

  let run = Functor.run

  let lift = Functor.lift

  let elevate = Functor.elevate

  let none _ = Wrapped.pure None

  let some x = Some x |> Wrapped.pure
end

module MakeA = MakeAT (Identity)

module MakeT (Wrapped : Monad.MONAD) = struct
  module WrappedInfix = Monad.MonadInfix (Wrapped)
  open WrappedInfix.Syntax
  module Applicative = MakeAT (Wrapped)

  module OptionMonad : Monad.MONAD with type 'a t = 'a option Wrapped.t = struct
    include Applicative.OptionApplicative

    let join v =
      let* x = v in
      match x with Some x' -> x' | _ -> Wrapped.pure None

    let bind m f = join (map f m)
  end

  include OptionMonad
  include Monad.MonadInfix (OptionMonad)

  let run = Applicative.run

  let lift = Applicative.lift

  let elevate = Applicative.elevate

  let none = Applicative.none

  let some = Applicative.some
end

module Make = MakeT (Identity)
