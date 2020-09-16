module MakeFT
    (Wrapped : Monad.FUNCTOR) (R : sig
      type t
    end) =
struct
  type r = R.t

  module WrappedInfix = Monad.FunctorInfix (Wrapped)
  open WrappedInfix.Syntax

  module ReaderFunctor : Monad.FUNCTOR with type 'a t = r -> 'a Wrapped.t =
  struct
    type 'a t = r -> 'a Wrapped.t

    let map f x r =
      let+ result = x r in
      f result
  end

  include ReaderFunctor
  include Monad.FunctorInfix (ReaderFunctor)

  module Utils = struct
    let run m ~init = m init [@@inline]

    let lift x = x [@@inline]

    let elevate v _ = v
  end

  include Utils
end

module MakeF = MakeFT (Identity)

module MakeAT
    (Wrapped : Monad.APPLICATIVE) (R : sig
      type t
    end) =
struct
  module WrappedInfix = Monad.ApplicativeInfix (Wrapped)
  open WrappedInfix.Syntax
  module Functor = MakeFT (Wrapped) (R)

  type r = Functor.r

  module ReaderApplicative :
    Monad.APPLICATIVE with type 'a t = r -> 'a Wrapped.t = struct
    include Functor.ReaderFunctor

    let pure v _ = Wrapped.pure v

    let apply fa va r =
      let+ f = fa r and+ v = va r in
      f v
  end

  include ReaderApplicative
  include Monad.ApplicativeInfix (ReaderApplicative)

  module Utils = struct
    include Functor.Utils

    let peek r = Wrapped.pure r
  end

  include Utils
end

module MakeA = MakeAT (Identity)

module MakeT
    (Wrapped : Monad.MONAD) (R : sig
      type t
    end) =
struct
  module WrappedInfix = Monad.MonadInfix (Wrapped)
  open WrappedInfix.Syntax
  module Applicative = MakeAT (Wrapped) (R)

  type r = Applicative.r

  module ReaderMonad : Monad.MONAD with type 'a t = r -> 'a Wrapped.t = struct
    include Applicative.ReaderApplicative

    let bind m k r =
      let* v = m r in
      k v r

    let join m = bind m (fun x -> x)
  end

  include ReaderMonad
  include Monad.MonadInfix (ReaderMonad)
  include Applicative.Utils
end

module Make = MakeT (Identity)
