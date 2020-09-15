(* Zip is not actually a monad transformer. It's an applicative transformer.
   That means you cannot use it as a parameter for another MakeT.

   But you CAN use other transformers as parameter for Zip.
 *)

module MakeT : functor
  (Wrapped : Monad.MONAD)
  -> sig
  include Monad.APPLICATIVE

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

  val run : 'a t -> 'a list Wrapped.t

  val lift : 'a list Wrapped.t -> 'a t

  val elevate : 'a Wrapped.t -> 'a t
end
with type 'a t = 'a list Wrapped.t

module Make : sig
  include Monad.APPLICATIVE

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end
with type 'a t = 'a list
