(** PROJECT: ORIGAMI **)
(** Author: Antoni Koszowski **)

open List;;

(** type representing point on a plane **)
type point = float * float

(** type representing (folded) sheet; returns number of layers at the given point **)
type kartka = point -> int

(** returns [1] if the given point is on the rectangle sheet, otherwise [0] **)
let rect (xp1, yp1) (xp2, yp2) =
  function (x1, y1) ->
    let eps = 0.000001 in
    if ((x1 +. eps) >= xp1 && (x1 -. eps) <= xp2) &&
       ((y1 +. eps) >= yp1 && (y1 -. eps) <= yp2) then 1 else 0

(** returns [1] if the given point is on the circle sheet, otherwise [0] **)
let circ (xp, yp) r =
  function (x1, y1) ->
    let eps = 0.000001 in
    let a = abs_float (y1 -. yp) in
    let b = abs_float (x1 -. xp) in
    let a = a *. a in
    let b = b *. b in
    if (sqrt (a +. b)) <= r +. eps then 1 else 0

(** returns coefficients of the line determined by the point [p1] [p2] **)
let get_coeffs (xp1, yp1) (xp2, yp2) =
  let a = yp1 -. yp2 in
  let b = xp2 -. xp1 in
  let c = (yp1 *. (xp1 -. xp2)) +. (xp1 *. (yp2 -. yp1))
  in (a, b, c)

(** returns x coordinate of point [p] in respect to a line with coefficients [(a,b,c)] **)
let get_refx (xp, yp) (a, b, c) =
  let refx =
    ((xp *. ((b *. b) -. (a *. a))) -. (2. *. a *. ((b *. yp) +. c)))
    /. ((a *. a) +. (b *. b))
  in refx

(** returns y coordinate of point [p] in respect to a line with coefficients [(a,b,c)] **)
let get_refy (xp, yp) (a, b, c) =
  let refy =
    ((yp *. ((a *. a) -. (b *. b))) -. (2. *. b *. ((a *. xp) +. c)))
    /. ((a *. a) +. (b *. b))
  in refy

(** checks on which side of the line determined by points [p1] [p2] lies point [p] **)
let side (xp1, yp1) (xp2, yp2) (xp, yp) =
  let eps = 0.000001 in
  let crossp = ((xp -. xp1) *. (yp2 -. yp1)) -. ((yp -. yp1) *. (xp2 -. xp1)) in
  if abs_float crossp <= eps then 0 else
  if crossp >= 0. then 1 else -1

(** returns a rectangle sheet, with left-bottom corner [p1] and righ-upper corner [p2] **)
let prostokat p1 p2 =
  function x ->
    rect p1 p2 x

(** returns a circle sheet, with centre in [p] and radius [r] **)
let kolko p r =
  function x ->
    circ p r x

(** returns sheet [k] folded along line determined by points [p1], [p2] **)
let zloz p1 p2 k =
  function x->
    if side p1 p2 x = 0 then k x else
    if side p1 p2 x = -1 then
      let coeffs = get_coeffs p1 p2 in
      let refx = get_refx x coeffs in
      let refy = get_refy x coeffs in
      k (refx, refy) + k x
    else
      0
(** returns sheet [k] folded along lines determined by points on list [l] respectively **)
let skladaj l k =
  function x ->
    let f = fold_left (fun pomk (p1, p2) -> zloz p1 p2 pomk) k l
    in f x

(**  ////////////////////////          TESTY         //////////////////////   **)

let test a b msg = if a<>b then (print_int a; print_string "<>"; print_int b; print_string " test: "; print_endline msg);;

let p1 = prostokat (0., 0.) (10., 10.)
let k1 = kolko (5., 5.) 5.
let l1 = [((0., 0.), (10., 10.));
          ((5., 0.), (10., 5.));
          ((10., 0.), (0., 10.));
          ((2.5, 0.), (2.5, 10.))];;
let l2 = [((8., 0.), (10., 2.));
          ((6., 0.), (10., 4.));
          ((4., 0.), (10., 6.));
          ((2., 0.), (10., 8.));
          ((0., 0.), (10., 10.));
          ((0., 2.), (8., 10.));
          ((0., 4.), (6., 10.));
          ((0., 6.), (4., 10.));
          ((0., 8.), (2., 10.))];;

let p2 = skladaj l1 p1
let p3 = skladaj l2 p1
let k2 = skladaj l1 k1;;

test (p2 (7., 3.)) 0 "0.1: p2";;
test (p2 (5., 8.)) 0 "0.2: p2";;
test (p2 (3., 5.)) 0 "0.3: p2";;
test (p2 (5., 5.)) 0 "0.4: p2";;
test (p2 (0., 0.)) 2 "1: p2";;
test (p2 (0., 10.)) 2  "2: p2";;
test (p2 (2.5, 2.5)) 2 "3: p2";;
test (p2 (2.5, 7.5)) 2 "4: p2";;
test (p2 (2.5, 5.)) 4 "5: p2";;
test (p2 (0., 5.)) 5 "6: p2";;
test (p2 (1., 2.)) 4 "7: p2";;
test (p2 (1., 5.)) 8 "8: p2";;
test (p2 (1., 8.)) 4 "9: p2";;

test (k2 (7., 3.)) 0 "0.1: k2";;
test (k2 (5., 8.)) 0 "0.2: k2";;
test (k2 (3., 5.)) 0 "0.3: k2";;
test (k2 (5., 5.)) 0 "0.4: k2";;
test (k2 (2.5, 2.5)) 2 "1: k2";;
test (k2 (2.5, 7.5)) 2 "2: k2";;
test (k2 (2.5, 5.)) 4 "3: k2";;
test (k2 (0., 5.)) 5 "4: k2";;
test (k2 (1., 3.)) 4 "5: k2";;
test (k2 (1., 5.)) 8 "6: k2";;
test (k2 (1., 7.)) 4 "7: k2";;

test (p3 ((-4.), 6.)) 2 "1: p3";;
test (p3 ((-3.), 5.)) 1 "2: p3";;
test (p3 ((-3.), 7.)) 2 "3: p3";;
test (p3 ((-2.), 6.)) 3 "4: p3";;
test (p3 ((-2.5), 6.5)) 4 "5: p3";;
test (p3 ((-2.), 8.)) 4 "6: p3";;
test (p3 ((-1.), 7.)) 3 "7: p3";;
test (p3 ((-1.5), 7.5)) 6 "8: p3";;
test (p3 (0., 8.)) 5 "9: p3";;
test (p3 ((-1.), 9.)) 4 "10: p3";;
test (p3 ((-0.5), 8.5)) 8 "11: p3";;
test (p3 (0., 10.)) 6 "12: p3";;
test (p3 (1., 9.)) 5 "13: p3";;
test (p3 (0.5, 9.5)) 10 "14: p3";;

let kolo = kolko (0.,0.) 10. in
assert (kolo (1000., 0.) = 0);
let poziomo = zloz (0.,0.) (1.,0.) kolo in
assert (poziomo (0.,0.) = 1);
assert (poziomo (0.,1.) = 2);
assert (poziomo (0.,-1.) = 0);
let pionowo = zloz (0.,0.) (0.,1.) kolo in
assert (pionowo (0.,0.) = 1);
assert (pionowo (-1.,0.) = 2);
assert (pionowo (1.,0.) = 0);
let cwiartka = zloz (0.,0.) (0.,1.) poziomo in
assert (cwiartka (0.,0.) = 1);
assert (cwiartka (-1.,1.) = 4);
assert (cwiartka (-1.,0.) = 2);
