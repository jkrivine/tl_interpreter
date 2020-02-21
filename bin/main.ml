open Dec_lib.Dec
(*let () = show_call NEW*)
let () =
  let pe = print_endline in
  let m0 = empty in
  let user0,user1,user2 = 0,1,2 in
  let token0,token1 = 0,1 in
  let current_time = 0 in
  pe "Initial state";
  pe (show m0);
  let m1,source = init_tl m0 user0 in
  pe "\nUser 0 owns a singleton source";
  pe (show m1);
  let fwd_contract = MP.of_alist_exn [(0,[make_clause None None [] [Pay (token0, 10)]])] in
  let grow_call = call_grow_A source fwd_contract 5 token0 1 9 in
  let m2 = exec m1 current_time [(user0, grow_call)] in
  let pos1 = 1 in (* Currently no return value is given, so we guess the next pos name. *)
  let m3 = transfer_pos m2 pos1 user1 in
  pe "\nUser 0 has grown the tradeline and given the new pos to User 1.";
  pe (show m3)
