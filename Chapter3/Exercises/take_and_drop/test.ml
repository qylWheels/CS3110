open OUnit2
open Main

let tests = "test suite" >::: [
  "take(): empty list, n > 0" >:: (fun _ -> assert_equal [] (take 100 []));
  "take(): non-empty list, n = 0" >:: (fun _ -> assert_equal [] (take 0 [1; 2; 3]));
  "take(): length of list is larger than n" >:: (fun _ -> assert_equal [1; 2] (take 2 [1; 2; 3]));
  "take(): length of list is less than n" >:: (fun _ -> assert_equal [1; 2] (take 3 [1; 2]));
  "drop(): empty list, n > 0" >:: (fun _ -> assert_equal [] (drop 100 []));
  "drop(): non-empty list, n = 0" >:: (fun _ -> assert_equal [1; 2; 3] (drop 0 [1; 2; 3]));
  "drop(): length of list is larger than n" >:: (fun _ -> assert_equal [3] (drop 2 [1; 2; 3]));
  "drop(): length of list is less than n" >:: (fun _ -> assert_equal [] (drop 3 [1; 2]));
  "take_tr(): empty list, n > 0" >:: (fun _ -> assert_equal [] (take_tr 100 []));
  "take_tr(): non-empty list, n = 0" >:: (fun _ -> assert_equal [] (take_tr 0 [1; 2; 3]));
  "take_tr(): length of list is larger than n"
    >:: (fun _ -> assert_equal [1; 2] (take_tr 2 [1; 2; 3]));
  "take_tr(): length of list is less than n" >:: (fun _ -> assert_equal [1; 2] (take_tr 3 [1; 2]));
  "take_tr(): deal with list having length 10_000_000"
    >:: (fun _ -> assert_equal (0 -- 9_999_999) (take_tr 10_000_000 (0 -- 10_000_000)))
]

let _ = run_test_tt_main tests
