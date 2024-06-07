open OUnit2
open Fraction

let tests = "test suite for fraction" >::: [
  "make" >:: (fun _ -> assert_raises ZeroDenominator (fun () -> Fraction.make 1 0));
  "numerator" >:: (fun _ -> assert_equal 1 (Fraction.(make 1 2 |> numerator)));
  "denominator" >:: (fun _ -> assert_equal 2 (Fraction.(make 1 2 |> denominator)));
  "to_string" >:: (fun _ -> assert_equal "1/2" (Fraction.(make 1 2 |> to_string)));
  "to_float" >:: (fun _ -> assert_equal 0.5 (Fraction.(make 1 2 |> to_float)));
  "add" >:: (fun _ -> assert_equal (Fraction.make 1 1) (
    let frac = Fraction.make 1 2 in
    Fraction.add frac frac
  ));
  "mul" >:: (fun _ -> assert_equal (Fraction.make 1 4) (
    let frac = Fraction.make 1 2 in
    Fraction.mul frac frac
  ));
]

let _ = run_test_tt_main tests
