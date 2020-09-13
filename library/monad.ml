(* Common definitions for all monads *)

module type FUNCTOR = sig
  type 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
end

module type APPLICATIVE = sig
  include FUNCTOR

  val apply: ('a -> 'b) t -> 'a t -> 'b t
  val pure: 'a -> 'a t
end

module type MONAD = sig
  include APPLICATIVE

  val bind: 'a t -> ('a -> 'b t) -> 'b t

  val join: 'a t t -> 'a t
end

module FunctorInfix(F: FUNCTOR) = struct
  let ( <$> ) = F.map
end

module ApplicativeInfix(A: APPLICATIVE) = struct
  include FunctorInfix(A)
  let ( <*> ) = A.apply
end

module MonadInfix(M: MONAD) = struct
  include ApplicativeInfix(M)
  let ( >>= ) = M.bind
end

module ApplicativeSyntax(A: APPLICATIVE) = struct

  module Helper = ApplicativeInfix(A)

  let ( let+ ) x f = A.map f x

  let ( and+ ) xa ya = let open Helper in
                       A.pure (fun x y -> (x, y)) <*> xa <*> ya

end

module MonadSyntax(M: MONAD) = struct
  include ApplicativeSyntax(M)

  let ( let* ) = M.bind
end
