module ZipApplicative = struct
  type 'a t = 'a list

  let rec zip xs ys =
    match (xs, ys) with
    | l1 :: ls1, l2 :: ls2 -> (l1, l2) :: zip ls1 ls2
    | _ -> []

  let pure = List.Make.pure

  let map = List.Make.map

  let apply f x =
    let c (x, y) = x y in
    zip f x |> Stdlib.List.map c
end

include ZipApplicative
include Monad.ApplicativeInfix (ZipApplicative)
