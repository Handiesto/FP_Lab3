open! OUnit2
open! Approx

let float_cmp a b = (a -. b) *. (a -. b) < 0.01

let points =
        [ (1., 2.); (3., 4.); (5., 6.); (7., 8.); (9., 10.); (11., 12.); (13., 14.) ]

let test_linear _ =
  let f = Approx.linear points in
  OUnit2.assert_equal ~cmp:float_cmp 2. (f 1.);
  OUnit2.assert_equal ~cmp:float_cmp 5.5 (f 4.5);
  OUnit2.assert_equal ~cmp:float_cmp 6.7 (f 5.7)

let test_segment _ =
  let f = Approx.segment points in
  OUnit2.assert_equal ~cmp:float_cmp 10.0 (f 2.5);
  OUnit2.assert_equal ~cmp:float_cmp 4.0 (f 1.023);
  OUnit2.assert_equal ~cmp:float_cmp 6.0 (f 4.6)

let test =
  "test" >::: ["test_linear" >:: test_linear; "test_segment" >:: test_segment]

let () = run_test_tt_main test