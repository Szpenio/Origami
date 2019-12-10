(** PROJECT: ORIGAMI **)
(** Author: Antoni Koszowski **)


(** type representing point on a plane **)
type point = float * float

(** type representing (folded) sheet; returns number of layers at the given point **)
type kartka = point -> int

let eps = 0.000001

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
  let crossp = ((xp -. xp1) *. (yp2 -. yp1)) -. ((yp -. yp1) *. (xp2 -. xp1)) in
  if abs_float crossp <= eps then 0 else
  if crossp >= 0. then 1 else -1

(** returns a rectangle sheet, with left-bottom corner [p1] and right-upper [p2];
    given the point sheet returns [1] if the given point is on the rectangle sheet,
    otherwise [0] **)
let prostokat (xp1, yp1) (xp2, yp2) =
  function (x1, y1) ->
    if ((x1 +. eps) >= xp1 && (x1 -. eps) <= xp2) &&
       ((y1 +. eps) >= yp1 && (y1 -. eps) <= yp2) then 1 else 0

(** returns a circle sheet, with centre in [p] and radius r;
    sheet returns [1] if the given point is on the circle sheet, otherwise [0] **)
let kolko (xp, yp) r =
  function (x1, y1) ->
    let a = abs_float (y1 -. yp) in
    let b = abs_float (x1 -. xp) in
    if (sqrt ((a *. a) +. (b *. b))) <= r +. eps then 1 else 0

(** returns sheet [k] folded along line determined by points [p1], [p2] **)
let zloz p1 p2 k =
  function x->
    if side p1 p2 x = 0 then k x else
    if side p1 p2 x = -1 then
      let coeffs = get_coeffs p1 p2 in
      let refx = get_refx x coeffs in
      let refy = get_refy x coeffs in
      k (refx, refy) + k x
    else 0

(** returns sheet [k] folded along lines determined by points on list [l] respectively **)
let skladaj l k =
  function x ->
    (List.fold_left (fun pomk (p1, p2) -> zloz p1 p2 pomk) k l) x
