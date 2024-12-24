module ZipSeqAlternative = struct
  type 'a t = 'a Stdlib.Seq.t

  let zip = Stdlib.Seq.zip

  let drop = Stdlib.Seq.drop

  let length seq = Stdlib.Seq.fold_left (fun i _ -> i + 1) 0 seq
  let pure = Seq.Make.pure
  let map = Seq.Make.map

  let apply f x =
    let c (x, y) = x y in
    zip f x |> Stdlib.Seq.map c

  let append x y = Seq.Make.append x (drop (length x) y)
  let empty () = Stdlib.Seq.empty
end

include ZipSeqAlternative
include Monad.AlternativeInfix (ZipSeqAlternative)
