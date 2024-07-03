open Sequence
open Printf

(* test [sift] *)
let test_sift () =
  printf "--- test_sift ---\n";
  let s = sift 2 nats in
  printf "[%i] %i\n" 1 (nth 1 s);
  printf "[%i] %i\n" 6 (nth 6 s)
let () = test_sift ()

(* test [primes] *)
let test_primes () =
  printf "--- test_primes ---\n";
  printf "[%i] %i\n" 0 (nth 0 primes);
  printf "[%i] %i\n" 3 (nth 3 primes);
  printf "[%i] %i\n" 7 (nth 7 primes);
  printf "[%i] %i\n" 113 (nth 113 primes)
let () = test_primes ()
