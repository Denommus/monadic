module MakeFT (Wrapped : Monad.FUNCTOR) = struct
  module WrappedInfix = Monad.FunctorInfix (Wrapped)
  open WrappedInfix.Syntax

  module ListFunctor : Monad.FUNCTOR with type 'a t = 'a list Wrapped.t = struct
    type 'a t = 'a list Wrapped.t

    let map f x =
      let+ v = x in
      Stdlib.List.map f v
  end

  include ListFunctor
  include Monad.FunctorInfix (ListFunctor)

  module Utils = struct
    let run m = m [@@inline]

    let lift x = x [@@inline]

    let elevate v =
      let+ x = v in
      [ x ]
  end

  include Utils
end

module MakeF = MakeFT (Identity)

module MakeAT (Wrapped : Monad.APPLICATIVE) = struct
  module WrappedInfix = Monad.ApplicativeInfix (Wrapped)
  open WrappedInfix.Syntax
  module Functor = MakeFT (Wrapped)

  module ListApplicative :
    Monad.APPLICATIVE with type 'a t = 'a list Wrapped.t = struct
    include Functor.ListFunctor

    let pure v = [ v ] |> Wrapped.pure

    let apply fa xa =
      let+ fs = fa and+ xs = xa in
      let accum curr acc =
        let l1 = Stdlib.List.map curr xs in
        l1 @ acc
      in
      Stdlib.List.fold_right accum fs []
  end

  include ListApplicative
  include Monad.ApplicativeInfix (ListApplicative)
  module Utils = Functor.Utils
  include Utils
end

module MakeA = MakeAT (Identity)

module MakeT (Wrapped : Monad.MONAD) = struct
  module WrappedInfix = Monad.MonadInfix (Wrapped)
  open WrappedInfix.Syntax
  module Applicative = MakeAT (Wrapped)

  module ListMonad : Monad.MONAD with type 'a t = 'a list Wrapped.t = struct
    include Applicative.ListApplicative

    let sequence ms =
      let open Monad.ApplicativeFunctions (Wrapped) in
      sequence ms

    let join v =
      let* (x : 'a t list) = v in
      let+ xs = sequence x in
      Stdlib.List.concat xs

    let bind m f = join (map f m)
  end

  include ListMonad
  include Monad.MonadInfix (ListMonad)
  include Applicative.Utils
end

module Make = MakeT (Identity)
