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
  ReaderWriter.pure 20

let test_transform _ =
  let reader_result = ReaderWriter.run ~init:10 reader_writer in
  let writer_result = WriterString.run reader_result in
  assert_equal (20, "10bar") writer_result

module RefState = Monadic.RefState.Make(String)

let test_ref_state _ =
  let reference = ref "Foo" in
  let state = let open RefState.Syntax in
              let open RefState in
              let+ foo = get
              and+ _ = put "Bar"
              in foo in
  let result = RefState.run state ~init:reference in
  assert_equal result "Foo";
  assert_equal !reference "Bar"

let suite =
  "Example" >::: [
      "test_transform" >:: test_transform;
      "test_ref_state" >:: test_ref_state
    ]


let () =
  run_test_tt_main suite
