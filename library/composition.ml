module ComposeFunctor (F1 : Monad.FUNCTOR) (F2 : Monad.FUNCTOR) :
  Monad.FUNCTOR with type 'a t = 'a F2.t F1.t = struct
  type 'a t = 'a F2.t F1.t

  let map f x = F1.map (F2.map f) x
end

module ComposeApplicative (A1 : Monad.APPLICATIVE) (A2 : Monad.APPLICATIVE) :
  Monad.APPLICATIVE with type 'a t = 'a A2.t A1.t = struct
  module Functor = ComposeFunctor (A1) (A2)
  include Functor

  let pure x = A1.pure (A2.pure x)

  let apply fa xa = A1.apply (A1.map A2.apply fa) xa
end
