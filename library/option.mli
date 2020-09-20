module MakeT : functor
  (Wrapped : Monad.MONAD)
  -> sig
  include Monad.MAKE_T with type 'a wrapped := 'a Wrapped.t

  val run : 'a t -> 'a option Wrapped.t

  val create : 'a option Wrapped.t -> 'a t

  val choice : 'a t -> 'a t -> 'a t

  val empty : unit -> 'a t

  val ( <|> ) : 'a t -> 'a t -> 'a t
end
with type 'a t = 'a option Wrapped.t

module Make : sig
  include Monad.MAKE

  val run : 'a t -> 'a option

  val create : 'a option -> 'a t

  val none : unit -> 'a t

  val choice : 'a t -> 'a t -> 'a t

  val empty : unit -> 'a t

  val ( <|> ) : 'a t -> 'a t -> 'a t
end
with type 'a t = 'a option
