open OUnit2
open Product

let tests = "test suite for product" >::: [
  "empty" >:: (fun _ -> assert_equal 1 (product []));
  "without zero" >:: (fun _ -> assert_equal 60 (product [3; 4; 5]));
  "with zero" >:: (fun _ -> assert_equal 0 (List.init 100 Fun.id |> product));
]

let _ = run_test_tt_main tests
