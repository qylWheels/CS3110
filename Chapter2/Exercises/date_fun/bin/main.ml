let date d m =
  if d < 0 || d > 31 then false
  else if d <= 31 && List.mem m ["Jan"; "Mar"; "May"; "Jul"; "Aug"; "Oct"; "Dec"] then true
  else if d <= 30 && List.mem m ["Apr"; "Jun"; "Sept"; "Nov"] then true
  else if d <= 28 && List.mem m ["Feb"] then true
  else false
in
  assert (date 31 "Jan");
  assert (date 30 "Jun");
  assert (date 28 "Feb");
  (* assert (date 32 "Jan"); *)
  (* assert (date 31 "Jun"); *)
  (* assert (date 29 "Feb"); *)
