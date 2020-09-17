module ComposeFunctor :
functor (F1 : Monad.FUNCTOR) (F2 : Monad.FUNCTOR)
->
sig
  include Monad.FUNCTOR

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end
with type 'a t = 'a F2.t F1.t

module ComposeApplicative : functor
  (A1 : Monad.APPLICATIVE)
  (A2 : Monad.APPLICATIVE)
  -> sig
  include Monad.APPLICATIVE

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end with type 'a t = 'a A2.t A1.t
