module type MONOID = sig
  type t
  val empty: t
  val append: t -> t -> t
end

module MakeT(Wrapped: Monad.MONAD)(W: MONOID) = struct
  type w = W.t

  module InternalWriterMonad = struct
    type 'a t = ('a * w) Wrapped.t

    module WrappedSyntax = Monad.MonadSyntax(Wrapped)

    open WrappedSyntax

    let pure v = Wrapped.pure (v, W.empty)

    let map f x = let+ v, w = x in f v, w

    let apply fa xa =
      let+ f, w1 = fa
      and+ x, w2 = xa in
      f x, W.append w1 w2

    let bind m f = let* x1, w1 = m in
                   let+ x2, w2 = f x1 in
                   x2, W.append w1 w2
  end

  module WriterMonad = Monad.DefaultJoin(InternalWriterMonad)

  include WriterMonad

  include Monad.MonadInfix(WriterMonad)

  module Syntax = Monad.MonadSyntax(WriterMonad)

  let tell w = Wrapped.pure ((), w)

  let runWriter m = m
end

module Make(W: MONOID) = MakeT(Identity)(W)
