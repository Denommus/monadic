open OUnit2

let empty_list = []

module WriterString = Monadic.Writer.Make(struct
                          type t = string
                          let empty = ""
                          let append = (^)
                        end)

module ReaderWriter = Monadic.Reader.MakeT(WriterString)(Int)

let reader_writer =
  let open ReaderWriter.Syntax in
  let open WriterString in
  let open ReaderWriter in
  let* foo = peek in
  let* _ = string_of_int foo |> tell |> elevate in
  let* _ = tell "bar" |> elevate in
  ReaderWriter.pure foo

let test_transform _ =
  let reader_result = ReaderWriter.run ~init:10 reader_writer in
  let writer_result = WriterString.run reader_result in
  assert_equal (10, "10bar") writer_result

let suite =
  "Example" >::: [
      "test_transform" >:: test_transform
    ]


let () =
  run_test_tt_main suite
