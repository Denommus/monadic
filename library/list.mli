module MakeT : functor
  (Wrapped : Monad.MONAD)
  -> sig
  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a list Wrapped.t

  val create : 'a list Wrapped.t -> 'a t

  val append : 'a t -> 'a t -> 'a t

  val empty : unit -> 'a t

  val ( <|> ) : 'a t -> 'a t -> 'a t
end
with type 'a t = 'a list Wrapped.t

module Make : sig
  include Monad.MAKE

  val run : 'a t -> 'a list

  val create : 'a list -> 'a t

  val append : 'a t -> 'a t -> 'a t

  val empty : unit -> 'a t

  val ( <|> ) : 'a t -> 'a t -> 'a t
end
with type 'a t = 'a list
