type point = float * float
(** type representing point on a plane **)

type kartka = point -> int
(** type representing (folded) sheet;
    returns number of layers at the given point **)

val prostokat : point -> point -> kartka
(** returns a rectangle sheet, with left-bottom corner [p1]
    and righ-upper corner [p2] **)


val kolko : point -> float -> kartka
(** returns a circle sheet, with centre in [p] and radius [r] **)


val zloz : point -> point -> kartka -> kartka
(** returns sheet [k] folded along line determined by points [p1], [p2] **)


val skladaj : (point * point) list -> kartka -> kartka
(** returns sheet [k] folded along lines determined by points on list [l] respectively **)
