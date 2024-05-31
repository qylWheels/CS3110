open OUnit2
open Library

let tests = "library test suite" >::: [
  "fifth(): empty list" >:: (fun _ -> assert_equal 0 (fifth []));
  "fifth(): element count less than 5" >:: (fun _ -> assert_equal 0 (fifth [3; 4; 5]));
  "fifth(): element count equal to 5" >:: (fun _ -> assert_equal 5 (fifth [1; 2; 3; 4; 5]));
  "fifth(): element count more than 5" >:: (fun _ -> assert_equal 5 (fifth [1; 2; 3; 4; 5; 6; 7]));
  "desc_sort(): empty list" >:: (fun _ -> assert_equal [] (desc_sort []));
  "desc_sort(): arbitrary" >:: (fun _ -> assert_equal [6; 5; 4; 3] (desc_sort [4; 3; 6; 5]));
]

let _ = run_test_tt_main tests
