(* Common definitions for all monads *)

module type FUNCTOR = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type APPLICATIVE = sig
  include FUNCTOR

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t
end

module type MONAD = sig
  include APPLICATIVE

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val join : 'a t t -> 'a t
end

module FunctorInfix (F : FUNCTOR) = struct
  let ( <$> ) f xa = F.map f xa [@@inline]

  module Syntax = struct
    let ( let+ ) x f = F.map f x [@@inline]
  end
end

module ApplicativeInfix (A : APPLICATIVE) = struct
  module InfixF = FunctorInfix (A)
  include InfixF

  let ( <*> ) fa xa = A.apply fa xa [@@inline]

  module Syntax = struct
    include InfixF.Syntax

    let ( and+ ) xa ya = (fun x y -> (x, y)) <$> xa <*> ya [@@inline]
  end
end

module MonadInfix (M : MONAD) = struct
  module InfixA = ApplicativeInfix (M)
  include InfixA

  let ( >>= ) m f = M.bind m f [@@inline]

  module Syntax = struct
    include InfixA.Syntax

    let ( let* ) m f = M.bind m f [@@inline]
  end
end

module type MAKE_F = sig
  include FUNCTOR

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module type MAKE_F_T = sig
  type 'a wrapped

  include MAKE_F

  val elevate : 'a wrapped -> 'a t
end

module type MAKE_A = sig
  include APPLICATIVE

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end

module type MAKE_A_T = sig
  type 'a wrapped

  include MAKE_A

  val elevate : 'a wrapped -> 'a t
end

module type MAKE = sig
  include MONAD

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type MAKE_T = sig
  type 'a wrapped

  include MAKE

  val elevate : 'a wrapped -> 'a t
end

module ApplicativeFunctions (A : APPLICATIVE) = struct
  open A
  module Infix = ApplicativeInfix (A)
  open Infix.Syntax

  let ( *> ) m m' =
    let+ _ = m and+ x' = m' in
    x'

  let ( <* ) m m' =
    let+ x' = m and+ _ = m' in
    x'

  let sequence ms =
    let k m m' =
      let+ x = m and+ xs = m' in
      x :: xs
    in
    Stdlib.List.fold_right k ms (pure [])

  let sequence_ ms = Stdlib.List.fold_right ( *> ) ms (pure ())

  let a_map f ms = sequence (Stdlib.List.map f ms) [@@inline]

  let a_map_ f ms = sequence_ (Stdlib.List.map f ms) [@@inline]

  let a_filter f xs =
    let k curr acc =
      let+ flg = f curr and+ ys = acc in
      if flg then curr :: xs else ys
    in
    Stdlib.List.fold_right k xs (pure [])

  let traverse f xs = Stdlib.List.map f xs |> sequence [@@inline]

  let a_for xs f = traverse f xs [@@inline]

  let a_for_ xs f = a_for xs f *> pure () [@@inline]
end

module MonadFunctions (M : MONAD) = struct
  open M
  module Infix = MonadInfix (M)
  open Infix.Syntax

  open ApplicativeFunctions (M)

  let m_fold f initial ms =
    let c x k z =
      let* m' = f z x in
      k m'
    in
    Stdlib.List.fold_right c ms pure initial

  let m_fold_ f initial ms = m_fold f initial ms *> pure () [@@inline]

  let ( >=> ) f1 f2 s =
    let* s' = f1 s in
    let+ s'' = f2 s' in
    s''
end
