module MakeT (Wrapped : Monad.MONAD) = struct
  module WrappedSyntax = Monad.MonadSyntax (Wrapped)
  open WrappedSyntax
  module ListMonad = List.MakeT (Wrapped)

  module ZipApplicative : Monad.APPLICATIVE with type 'a t = 'a list Wrapped.t =
  struct
    type 'a t = 'a list Wrapped.t

    let pure = ListMonad.pure

    let map = ListMonad.map

    let rec zip xs ys =
      match (xs, ys) with
      | l1 :: ls1, l2 :: ls2 -> (l1, l2) :: zip ls1 ls2
      | _ -> []

    let apply : ('a -> 'b) t -> 'a t -> 'b t =
     fun fa xa ->
      let* f = fa in
      let* x = xa in
      let c (x, y) = x y in
      ListMonad.apply (ListMonad.pure c) (Wrapped.pure @@ zip f x)
  end

  include ZipApplicative

  let elevate = ListMonad.elevate

  include Monad.ApplicativeInfix (ZipApplicative)
  module Syntax = Monad.ApplicativeSyntax (ZipApplicative)

  let run = ListMonad.run

  let lift = ListMonad.lift
end

module Make = MakeT (Identity)
