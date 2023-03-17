open! OUnit2
open! Approx

let float_cmp a b = (a -. b) *. (a -. b) < 0.01

let test_linear _ =
  let points =
    [ (1., 2.)
    ; (3., 4.)
    ; (5., 6.)
    ; (7., 8.)
    ; (9., 10.)
    ; (11., 12.)
    ; (13., 14.) ]
  in
  let f = Approx.linear points in
  OUnit2.assert_equal ~cmp:float_cmp 2. (f 1.) ;
  OUnit2.assert_equal ~cmp:float_cmp 5.5 (f 4.5) ;
  OUnit2.assert_equal ~cmp:float_cmp 6.7 (f 5.7)

let test_segment _ =
  let points = [(3., 4.); (2., 4.); (1., 1.)] in
  let f = Approx.segment points in
  OUnit2.assert_equal ~cmp:float_cmp 4. (f 2.5) ;
  OUnit2.assert_equal ~cmp:float_cmp 2.5 (f 1.5)

let test_logarifm _ =
  let points =
    [ (1., 2.)
    ; (3., 4.)
    ; (5., 6.)
    ; (7., 8.)
    ; (9., 10.)
    ; (11., 12.)
    ; (13., 14.) ]
  in
  let f = Approx.logarifm points in
  OUnit2.assert_equal ~cmp:float_cmp 0.4406432356126949 (f 1.02) ;
  OUnit2.assert_equal ~cmp:float_cmp 4.503721892587115 (f 2.5) ;
  OUnit2.assert_equal ~cmp:float_cmp 7.364782591706943 (f 4.7)

let test =
  "test"
  >::: [ "test_linear" >:: test_linear
       ; "test_segment" >:: test_segment
       ; "test_logarifm" >:: test_logarifm ]

let () = run_test_tt_main test
