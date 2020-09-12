include Monad.MONAD
val pure: 'a -> 'a t
val ( <$> ): ('a -> 'b) -> 'a t -> 'b t
val ( <*> ): ('a -> 'b) t -> 'a t -> 'b t
val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
val unlift: 'a t -> 'a
module Syntax: sig
  val ( let+ ): 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ): 'a t -> 'b t -> 'a * 'b t
  val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
end
