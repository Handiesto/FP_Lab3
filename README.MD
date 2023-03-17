# Функциональное программирование
## Лабораторная работа 3

<b>Выполнил:</b> Плясов Климентий Евгеньевич \
<b>Группа:</b> p34102 \
<b>Преподаватель:</b> Пенской Александр Владимирович

### Реализация
Функция читает из стандартного потока пока не будет пустая строка.
В результате получаем список пар (x,y)
```ocaml
let read k pList =
  match read_line () with
  | "" -> pList
  | input -> (
      let data = String.split_on_char ';' input |> List.filter (( <> ) "") in
      match data with
      | x_str :: y_str :: _ ->
          let x = float_of_string x_str in
          let y = float_of_string y_str in
          (x, y) :: truncate (k - 1) pList
      | _ -> pList )

```
Полученный список точек обрабатывается, параллельно вычисляются функции, Печатается результат.\
Ожидается ввод новой пары чисел и алгоритм повторяется.

`linear` - аппроксимация линейной функцией
`logarithm` - аппроксимация логарифмической функцией
`segment` - аппроксимация отрезками

```ocaml
let linear points =
  let sx = List.fold_left (fun acc (x, _) -> acc +. x) 0. points in
  let sxx = List.fold_left (fun acc (x, _) -> acc +. (x *. x)) 0. points in
  let sy = List.fold_left (fun acc (_, y) -> acc +. y) 0. points in
  let sxy = List.fold_left (fun acc (x, y) -> acc +. (x *. y)) 0. points in
  let n = float_of_int (List.length points) in
  let a = ((sxy *. n) -. (sx *. sy)) /. ((sxx *. n) -. (sx *. sx)) in
  let b = ((sxx *. sy) -. (sx *. sxy)) /. ((sxx *. n) -. (sx *. sx)) in
  fun x -> (a *. x) +. b

let segment points =
  let rec findBottomBorder i v =
    if i < List.length points then
      let x, _ = List.nth points i in
      if x < v then i else findBottomBorder (i + 1) v
    else -1
  in
  let rec findTopBorder i v =
    if i >= 0 then
      let x, _ = List.nth points i in
      if x >= v then i else findTopBorder (i - 1) v
    else -1
  in
  let f x =
    let top = findTopBorder (List.length points - 1) x in
    let bottom = findBottomBorder 0 x in
    if top = -1 then
      let _, yi = List.nth points 0 in
      yi
    else if bottom = -1 then
      let _, yi = List.nth points (List.length points - 1) in
      yi
    else
      let xi, yi = List.nth points top in
      let xiPrev, yiPrev = List.nth points bottom in
      let a = (yi -. yiPrev) /. (xi -. xiPrev) in
      let b = yi -. (a *. xi) in
      (a *. x) +. b
  in
  f

let logarifm points =
  let sum_by f lst =
    List.fold_left (fun state (x, y) -> state +. f x y) 0. lst
  in
  let sx = sum_by (fun x _ -> log x) points in
  let sxx = sum_by (fun x _ -> log x *. log x) points in
  let sy = sum_by (fun _ y -> y) points in
  let sxy = sum_by (fun x y -> log x *. y) points in
  let n = List.length points |> float_of_int in
  let delta = (sxx *. n) -. (sx *. sx) in
  let delta1 = (sxy *. n) -. (sx *. sy) in
  let delta2 = (sxx *. sy) -. (sx *. sxy) in
  let a = delta1 /. delta in
  let b = delta2 /. delta in
  let f x = (a *. log x) +. b in
  f

```

Функция `printData` выводит на экран результат аппроксимации.

```
cat test.csv | dune exec approx 5 5 1 2 3

x: 4.00000, y: 10.54518 | x: 4.00000, y: 10.54518 | x: 4.00000, y: 10.39480 | 
x: 4.20000, y: 10.72369 | x: 4.20000, y: 10.74034 | x: 4.20000, y: 10.70776 | 
x: 4.40000, y: 10.90221 | x: 4.40000, y: 10.92642 | x: 4.40000, y: 11.02073 | 
x: 4.60000, y: 11.08072 | x: 4.60000, y: 11.10423 | x: 4.60000, y: 11.33369 | 
x: 4.80000, y: 11.25924 | x: 4.80000, y: 11.27446 | x: 4.80000, y: 11.64665 | 
x: 5.00000, y: 11.43775 | x: 5.00000, y: 11.43775 | x: 5.00000, y: 11.34662 | 
x: 5.20000, y: 11.58361 | x: 5.20000, y: 11.59463 | x: 5.20000, y: 11.56327 | 
x: 5.40000, y: 11.72947 | x: 5.40000, y: 11.74560 | x: 5.40000, y: 11.77991 | 
x: 5.60000, y: 11.87532 | x: 5.60000, y: 11.89107 | x: 5.60000, y: 11.99655 | 
x: 5.80000, y: 12.02118 | x: 5.80000, y: 12.03143 | x: 5.80000, y: 12.21320 | 
x: 6.00000, y: 12.16704 | x: 6.00000, y: 12.16704 | x: 6.00000, y: 12.10564 | 
x: 6.20000, y: 12.29036 | x: 6.20000, y: 12.29820 | x: 6.20000, y: 12.27364 | 
x: 6.40000, y: 12.41368 | x: 6.40000, y: 12.42519 | x: 6.40000, y: 12.44165 | 
x: 6.60000, y: 12.53700 | x: 6.60000, y: 12.54828 | x: 6.60000, y: 12.60965 | 
x: 6.80000, y: 12.66032 | x: 6.80000, y: 12.66769 | x: 6.80000, y: 12.77766 | 
x: 7.00000, y: 12.78364 | x: 7.00000, y: 12.78364 | x: 7.00000, y: 12.73938 | 
x: 7.20000, y: 12.89047 | x: 7.20000, y: 12.89632 | x: 7.20000, y: 12.87720 | 
x: 7.40000, y: 12.99729 | x: 7.40000, y: 13.00592 | x: 7.40000, y: 13.01502 | 
x: 7.60000, y: 13.10412 | x: 7.60000, y: 13.11259 | x: 7.60000, y: 13.15285 | 
x: 7.80000, y: 13.21094 | x: 7.80000, y: 13.21649 | x: 7.80000, y: 13.29067 | 
x: 8.00000, y: 13.31777 | x: 8.00000, y: 13.31777 | x: 8.00000, y: 13.28432 | 
x: 8.20000, y: 13.41199 | x: 8.20000, y: 13.41654 | x: 8.20000, y: 13.40138 | 
x: 8.40000, y: 13.50622 | x: 8.40000, y: 13.51293 | x: 8.40000, y: 13.51844 | 
x: 8.60000, y: 13.60045 | x: 8.60000, y: 13.60705 | x: 8.60000, y: 13.63550 | 
x: 8.80000, y: 13.69467 | x: 8.80000, y: 13.69901 | x: 8.80000, y: 13.75256 | 
x: 9.00000, y: 13.78890 | x: 9.00000, y: 13.78890 | x: 9.00000, y: 13.76272 | 
x: 9.20000, y: 13.87319 | x: 9.20000, y: 13.87681 | x: 9.20000, y: 13.86456 | 
x: 9.40000, y: 13.95748 | x: 9.40000, y: 13.96284 | x: 9.40000, y: 13.96640 | 
x: 9.60000, y: 14.04176 | x: 9.60000, y: 14.04705 | x: 9.60000, y: 14.06823 | 
x: 9.80000, y: 14.12605 | x: 9.80000, y: 14.12953 | x: 9.80000, y: 14.17007 | 
x: 10.00000, y: 14.21034 | x: 10.00000, y: 14.21034 | x: 10.00000, y: 14.18929 | 
x: 10.20000, y: 14.28659 | x: 10.20000, y: 14.28955 | x: 10.20000, y: 14.27946 | 
x: 10.40000, y: 14.36284 | x: 10.40000, y: 14.36722 | x: 10.40000, y: 14.36963 | 
x: 10.60000, y: 14.43908 | x: 10.60000, y: 14.44342 | x: 10.60000, y: 14.45980 | 
x: 10.80000, y: 14.51533 | x: 10.80000, y: 14.51818 | x: 10.80000, y: 14.54997 | 
x: 11.00000, y: 14.59158 | x: 11.00000, y: 14.59158 | x: 11.00000, y: 14.57428 | 
x: 11.20000, y: 14.66119 | x: 11.20000, y: 14.66366 | x: 11.20000, y: 14.65521 | 
x: 11.40000, y: 14.73080 | x: 11.40000, y: 14.73445 | x: 11.40000, y: 14.73614 | 
x: 11.60000, y: 14.80041 | x: 11.60000, y: 14.80402 | x: 11.60000, y: 14.81707 | 
x: 11.80000, y: 14.87002 | x: 11.80000, y: 14.87240 | x: 11.80000, y: 14.89800 | 


```

### Выводы
Я вспомнил методы аппроксимации и узнал о работе с потоками на Ocaml при помощи Lwt. Не очень удобно, если программа написана не полностью на данной библиотеке, а лишь небольшая ее часть.