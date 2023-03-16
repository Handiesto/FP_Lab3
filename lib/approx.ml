open Printf

let rec generate_list_impl size = function
  | index when index == size -> []
  | i -> i :: generate_list_impl size (i + 1)

let generate_list = function 0 -> [] | n -> generate_list_impl n 0

let linear points =
  let sx = List.fold_left (fun acc (x, _) -> acc +. x) 0. points in
  let sxx = List.fold_left (fun acc (x, _) -> acc +. x *. x) 0. points in
  let sy = List.fold_left (fun acc (_, y) -> acc +. y) 0. points in
  let sxy = List.fold_left (fun acc (x, y) -> acc +. x *. y) 0. points in
  let n = float_of_int (List.length points) in
  let a = (sxy *. n -. sx *. sy) /. (sxx *. n -. sx *. sx) in
  let b = (sxx *. sy -. sx *. sxy) /. (sxx *. n -. sx *. sx) in
  fun x -> a *. x +. b

let segment points x =
  let rec findBorder i v f =
    if i < 0 || i >= List.length points then -1
    else
      let x', _ = List.nth points i in
      if f x' v then i else findBorder (i + (if f x' v then -1 else 1)) v f
  in
  let f x =
    let top = findBorder (List.length points - 1) x (<=) in
    let bottom = findBorder 0 x (<) in
    match top, bottom with
    | -1, _ -> List.nth points 0 |> snd
    | _, -1 -> List.nth points (List.length points - 1) |> snd
    | _ ->
        let xi, yi = List.nth points top in
        let xiPrev, yiPrev = List.nth points bottom in
        let a = (yi -. yiPrev) /. (xi -. xiPrev) in
        let b = yi -. (a *. xi) in
        (a *. x) +. b
  in
  f x


let logarifm points =
  let sum_by f lst = List.fold_left (fun state (x, y) -> state +. f x y) 0. lst in
  let sx = sum_by (fun x _ -> log x) points in
  let sxx = sum_by (fun x _ -> (log x *. log x)) points in
  let sy = sum_by (fun _ y -> y) points in
  let sxy = sum_by (fun x y -> (log x *. y)) points in
  let n = List.length points |> float_of_int in
  let delta = (sxx *. n) -. (sx *. sx) in
  let delta1 = (sxy *. n) -. (sx *. sy) in
  let delta2 = (sxx *. sy) -. (sx *. sxy) in
  let a = delta1 /. delta in
  let b = delta2 /. delta in
  let f x = (a *. log x) +. b in
  f


let getFunc points = function
  | 1 -> segment points
  | 2 -> logarifm points
  | 3 -> linear points
  | _ -> failwith "invalid points"

let genPoint n = function
  | (x2, _) :: (x1, _) :: _ ->
      let mult = (x2 -. x1) /. float_of_int n in
      (fun i -> x1 +. float_of_int i *. mult)
  | _ -> (fun _ -> 0.)

let printData funcs pointGen count =
  generate_list count
  |> List.map (fun i ->
         funcs
         |> Array.map (fun f ->
                print_string "x: " ;
                printf "%.5f" (pointGen i) ;
                print_string ", y: " ;
                printf "%.5f" (f (pointGen i)) ;
                print_string " | " )
         |> ignore ;
         print_newline () )
  |> ignore