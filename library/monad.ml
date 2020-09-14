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

module type MAKE = sig
  include MONAD
  val ( <$> ): ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ): ('a -> 'b) t -> 'a t -> 'b t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  module Syntax: sig
    val ( let+ ): 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ): 'a t -> 'b t -> ('a * 'b) t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type MAKE_T = sig
  type 'a wrapped
  include MAKE
  val elevate: 'a wrapped -> 'a t
end

module ApplicativeFunctions(A: APPLICATIVE) = struct
  open A
  open ApplicativeSyntax(A)

  let ( *> ) m m' = let+ _ = m
                    and+ x' = m'
                    in x'

  let ( <* ) m m' = let+ x' = m
                    and+ _ = m'
                    in x'

  let sequence ms =
    let k m m' =
      let+ x = m
      and+ xs = m' in
      (x::xs) in
    List.fold_right k ms (pure [])

  let sequence_ ms = List.fold_right ( *> ) ms (pure ())

  let a_map f ms = sequence (List.map f ms)

  let a_map_ f ms = sequence_ (List.map f ms)

  let a_filter f xs =
    let k curr acc =
      let+ flg = f curr
      and+ ys = acc
      in if flg then curr::xs else ys in
    List.fold_right k xs (pure [])

  let traverse f xs = List.map f xs |> sequence

  let a_for xs f = traverse f xs

  let a_for_ xs f = a_for xs f *> pure ()
end

module MonadFunctions(M: MONAD) = struct
  open M
  open MonadSyntax(M)

  open ApplicativeFunctions(M)

  let m_fold f initial ms =
    let c x k z = let* m' = f z x in k m' in
    List.fold_right c ms pure initial

  let m_fold_ f initial ms =
    m_fold f initial ms *> pure ()

  let ( >=> ) f1 f2 s = let* s' = f1 s in
                        let+ s'' = f2 s' in
                        s''
end
