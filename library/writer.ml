module type MONOID = sig
  type t

  val empty : t

  val append : t -> t -> t
end

module MakeFT (Wrapped : Monad.FUNCTOR) (W : MONOID) = struct
  module WrappedInfix = Monad.FunctorInfix (Wrapped)
  open WrappedInfix.Syntax

  type w = W.t

  module WriterFunctor : Monad.FUNCTOR with type 'a t = ('a * w) Wrapped.t =
  struct
    type 'a t = ('a * w) Wrapped.t

    let map f x =
      let+ v, w = x in
      (f v, w)
  end

  include WriterFunctor
  include Monad.FunctorInfix (WriterFunctor)

  module Utils = struct
    let run m = m [@@inline]

    let lift x = x [@@inline]

    let elevate v =
      let+ x = v in
      (x, W.empty)
  end

  include Utils
end

module MakeF = MakeFT (Identity)

module MakeAT (Wrapped : Monad.APPLICATIVE) (W : MONOID) = struct
  module WrappedInfix = Monad.ApplicativeInfix (Wrapped)
  open WrappedInfix.Syntax
  module Functor = MakeFT (Wrapped) (W)

  type w = Functor.w

  module WriterApplicative :
    Monad.APPLICATIVE with type 'a t = ('a * w) Wrapped.t = struct
    include Functor.WriterFunctor

    let pure v = Wrapped.pure (v, W.empty)

    let apply fa xa =
      let+ f, w1 = fa and+ x, w2 = xa in
      (f x, W.append w1 w2)
  end

  include WriterApplicative
  include Monad.ApplicativeInfix (WriterApplicative)

  module Utils = struct
    include Functor.Utils

    let tell w = ((), w) |> Wrapped.pure
  end

  include Utils
end

module MakeA = MakeAT (Identity)

module MakeT (Wrapped : Monad.MONAD) (W : MONOID) = struct
  module WrappedInfix = Monad.MonadInfix (Wrapped)
  open WrappedInfix.Syntax
  module Applicative = MakeAT (Wrapped) (W)

  type w = Applicative.w

  module WriterMonad : Monad.MONAD with type 'a t = ('a * w) Wrapped.t = struct
    include Applicative.WriterApplicative

    let bind m f =
      let* x1, w1 = m in
      let+ x2, w2 = f x1 in
      (x2, W.append w1 w2)

    let join m = bind m (fun x -> x)
  end

  include WriterMonad
  include Monad.MonadInfix (WriterMonad)
  include Applicative.Utils
end

module Make = MakeT (Identity)
