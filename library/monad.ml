(* Common definitions for all monads *)
module type MONOID = sig
  type t

  val empty : t

  val append : t -> t -> t
end

module type FUNCTOR = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type APPLICATIVE = sig
  include FUNCTOR

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t
end

module type ALTERNATIVE = sig
  include APPLICATIVE

  val empty : unit -> 'a t

  val append : 'a t -> 'a t -> 'a t
end

module type MONAD = sig
  include APPLICATIVE

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val join : 'a t t -> 'a t
end

module type MONAD_PLUS = sig
  include ALTERNATIVE

  include MONAD with type 'a t := 'a t
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

  let ( <*> ) = A.apply

  module Syntax = struct
    include InfixF.Syntax

    let ( and+ ) xa ya = (fun x y -> (x, y)) <$> xa <*> ya [@@inline]
  end
end

module AlternativeInfix (A : ALTERNATIVE) = struct
  module InfixA = ApplicativeInfix (A)
  include InfixA

  let ( <|> ) = A.append
end

module MonadInfix (M : MONAD) = struct
  module InfixA = ApplicativeInfix (M)
  include InfixA

  let ( >>= ) = M.bind

  module Syntax = struct
    include InfixA.Syntax

    let ( let* ) = M.bind
  end
end

module MonadPlusInfix (M : MONAD_PLUS) = struct
  module InfixM = MonadInfix (M)
  include InfixM

  let ( <|> ) = M.append
end

module type MAKE_T = sig
  type 'a wrapped

  include MONAD

  val elevate : 'a wrapped -> 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type MAKE_PLUS_T = sig
  include MAKE_T

  val ( <|> ) : 'a t -> 'a t -> 'a t

  val append : 'a t -> 'a t -> 'a t

  val empty : unit -> 'a t
end

module CreateMonadPlus
    (M : MONAD) (C : sig
      type 'a t = 'a M.t

      val append : 'a t -> 'a t -> 'a t

      val empty : unit -> 'a t
    end) : sig
  include MONAD_PLUS with type 'a t = 'a M.t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( <|> ) : 'a t -> 'a t -> 'a t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end = struct
  module MonadPlus = struct
    include M
    include C
  end

  module Infix = MonadPlusInfix (MonadPlus)
  include MonadPlus
  include Infix
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

  let sequence_array ms =
    let k m m' =
      let+ x = m and+ xs = m' in
      Stdlib.Array.append [| x |] xs
    in
    Stdlib.Array.fold_right k ms (pure [||])

  let sequence_array_ ms = Stdlib.Array.fold_right ( *> ) ms (pure ())

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

  let lift2 fa aa ba =
    let+ f = fa and+ a = aa and+ b = ba in
    f a b

  let lift3 fa aa ba ca =
    let+ f = fa and+ a = aa and+ b = ba and+ c = ca in
    f a b c

  let lift4 fa aa ba ca da =
    let+ f = fa and+ a = aa and+ b = ba and+ c = ca and+ d = da in
    f a b c d

  let m_when condition action = if condition then action else pure ()

  let m_unless condition = m_when (not condition)

  let m_replicate i m = Stdlib.List.init i (fun _ -> m)
                        |> sequence

  let m_replicate_ i m = m_replicate i m *> pure ()
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

module AlternativeFunctions (A: ALTERNATIVE) = struct
  let guard c = if c then A.pure () else A.empty ()

  module Infix = AlternativeInfix (A)

  (* some and many are still untested, they are completely based on the Haskell definitions *)
  let some v =
    let open Infix in
    let open Infix.Syntax in
    let rec some_v () = let+ v_ = v and+ many_v_ = many_v () in v_ :: many_v_
    and many_v () = some_v () <|> A.pure [] in
    some_v ()

  let many v =
    let open Infix in
    let open Infix.Syntax in
    let rec some_v () = let+ v_ = v and+ many_v_ = many_v () in v_ :: many_v_
    and many_v () = some_v () <|> A.pure [] in
    many_v ()

  let optional v =
    let open Infix in
    Stdlib.Option.some <$> v <|> A.pure None
end
