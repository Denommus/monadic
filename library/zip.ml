module MakeFT = List.MakeFT
module MakeF = List.MakeF

module MakeAT (Wrapped : Monad.APPLICATIVE) = struct
  module WrappedInfix = Monad.ApplicativeInfix (Wrapped)
  open WrappedInfix.Syntax
  module ListApplicative = List.MakeAT (Wrapped)

  module ZipApplicative : Monad.APPLICATIVE with type 'a t = 'a list Wrapped.t =
  struct
    type 'a t = 'a list Wrapped.t

    let pure = ListApplicative.pure

    let map = ListApplicative.map

    let rec zip xs ys =
      match (xs, ys) with
      | l1 :: ls1, l2 :: ls2 -> (l1, l2) :: zip ls1 ls2
      | _ -> []

    let apply : ('a -> 'b) t -> 'a t -> 'b t =
     fun fa xa ->
      let+ f = fa and+ x = xa in
      let c (x, y) = x y in
      zip f x |> Stdlib.List.map c
  end

  include ZipApplicative
  include Monad.ApplicativeInfix (ZipApplicative)

  let elevate = ListApplicative.elevate

  let lift = ListApplicative.lift

  let run = ListApplicative.run
end

module MakeA = MakeAT (Identity)
