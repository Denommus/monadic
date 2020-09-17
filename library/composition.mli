module ComposeFunctor : functor (F1 : Monad.FUNCTOR) (F2 : Monad.FUNCTOR) ->
  Monad.FUNCTOR with type 'a t = 'a F2.t F1.t

module ComposeApplicative : functor
  (A1 : Monad.APPLICATIVE)
  (A2 : Monad.APPLICATIVE)
  -> Monad.APPLICATIVE with type 'a t = 'a A2.t A1.t
