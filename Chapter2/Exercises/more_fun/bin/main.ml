(* [cube x] is the cube of [x] *)
let cube x = x *. x *. x in
Printf.printf "cube 1 = %F\n" (cube 1.);
Printf.printf "cube 2.5 = %F\n" (cube 2.5);

(* [sgn x] is the sign of [x] *)
let sgn n =
  if n < 0 then -1
  else if n = 0 then 0
  else 1
in
Printf.printf "sgn 114514 = %i\n" (sgn 114514);
Printf.printf "sgn -1919810 = %i\n" (-1919810 |> sgn);
Printf.printf "sgn 0 = %i\n" (sgn 0);

(* [area r] is the area of a circle given its radius [r] *)
let area r = 3.1415926 *. r *. r in
assert (abs_float ((area 1.) -. 3.1415926) < 1e-5)
