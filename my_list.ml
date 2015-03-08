type 'a my_list =
  | Item of ('a * 'a my_list)
  | Empty

(* tail-rec *)
let rec length = function
  | Empty		-> 0
  | Item(hd, tl)	-> (length tl) + 1

(* not-rec *)
let hd = function
  | Empty		-> raise(Failure "hd")
  | Item(hd, tl)	-> hd

(* not-rec *)
let tl = function
  | Empty		-> raise(Failure "tl")
  | Item(hd, tl)	-> tl

(* tail-rec *)
let rec nth list nb =
  if (nb < 0)
  then raise(Invalid_argument "List.nth")
  else match list with
    | Empty		-> raise(Failure "nth")
    | Item(hd, tl)	-> if nb = 0 then hd else nth tl (nb - 1)

(* tail-rec *)
let rec rev_append a b = match a with
  | Empty		-> b
  | Item(hd, tl)	-> rev_append tl (Item(hd, b))

(* tail-rec *)
let rec rev a =
  rev_append a Empty

(* tail-rec *)
let append a b =
  rev_append (rev a) b

(* not-tail-rec *)
let rec flatten = function
  | Empty		-> Empty
  | Item(hd, tl)	-> append hd (flatten tl)

(* not-tail-rec *)
let concat list =
  flatten list

(* tail-rec *)
let rec iter func = function
  | Empty		-> ()
  | Item(hd, tl)	-> begin
    func hd;
    iter func tl;
  end

(* tail-rec *)
let rec iteri i func = function
  | Empty		-> ()
  | Item(hd, tl)	-> begin
    func i hd;
    iteri (i + 1) func tl;
  end

(* not-tail-rec *)
let iteri func list =
  iteri 0 func list

(* tail-rec *)
let rec iter2 func a b = match (a, b) with
  | (Empty, Empty)			-> ()
  | (Item(hda, tla), Item(hdb, tlb))	-> begin
    func hda hdb;
    iter2 func tla tlb;
  end
  | (_, _)				-> raise(Invalid_argument "List.iter2")

(* not-tail-rec *)
let rec map func = function
  | Empty		-> Empty
  | Item(hd, tl)	-> let b = func hd in Item(b, map func tl)

(* not-tail-rec *)
let rec mapi i func = function
  | Empty		-> Empty
  | Item(hd, tl)	-> let b = func i hd in Item(b, mapi (i + 1) func tl)

(* not-tail-rec *)
let mapi func list =
  mapi 0 func list

(* not-tail-rec *)
let rec map2 func a b = match(a, b) with
  | (Empty, Empty)			-> Empty
  | (Item(hda, tla), Item(hdb, tlb))	-> let c = func hda hdb in Item(c, map2 func tla tlb)
  | (_, _)				-> raise(Invalid_argument "List.map2")


(* tail-rec *)
let rev_map func list =
  let rec __rev_map a = function
    | Empty		-> a
    | Item(hd, tl)	-> __rev_map (Item(func hd, a)) tl
  in __rev_map Empty list

(* tail-rec *)
let rev_map2 func la lb =
  let rec __rev_map2 ret a b = match (a, b) with
    | (Empty, Empty)			-> ret
    | (Item(hda, tla), Item(hdb, tlb))	-> __rev_map2 (Item(func hda hdb, ret)) tla tlb
    | (_, _)				-> raise(Invalid_argument "List.map2")
  in __rev_map2 Empty la lb


(* tail-rec *)
let rec fold_left func a = function
  | Empty		-> a
  | Item(hd, tl)	-> fold_left func (func a hd) tl

(* not-tail-rec *)
let rec fold_right func list a = match list with
  | Empty		-> a
  | Item(hd, tl)	-> func hd (fold_right func tl a)

(* tail-rec *)
let rec fold_left2 func a la lb = match (la, lb) with
  | (Empty, Empty)			-> a
  | (Item(hda, tla), Item(hdb, tlb))	-> fold_left2 func (func a hda hdb) tla tlb
  | (_, _)				-> raise(Invalid_argument "List.fold_left2")

(* not-tail-rec *)
let rec fold_right2 func la lb a = match (la, lb) with
  | (Empty, Empty)			-> a
  | (Item(hda, tla), Item(hdb, tlb))	-> func hda hdb (fold_right2 func tla tlb a)
  | (_, _)				-> raise(Invalid_argument "List.fold_right2")

(* tail-rec *)
let rec for_all func = function
  | Empty		-> true
  | Item(hd, tl)	-> func hd && for_all func tl

(* tail-rec *)
let rec exists func = function
  | Empty		-> false
  | Item(hd, tl)	-> func hd || exists func tl

(* tail-rec *)
let rec for_all2 func la lb = match (la, lb) with
  | (Empty, Empty)			-> true
  | (Item(hda, tla), Item(hdb, tlb))	-> func hda hdb && for_all2 func tla tlb
  | (_, _)                              -> raise(Invalid_argument "List.for_all2")

(* tail-rec *)
let rec exists2 func la lb = match (la, lb) with
  | (Empty, Empty)			-> false
  | (Item(hda, tla), Item(hdb, tlb))	-> func hda hdb || exists2 func tla tlb
  | (_, _)                              -> raise(Invalid_argument "List.exists2")

(* tail-rec *)
let rec mem i = function
  | Empty		-> false
  | Item(hd, tl)	-> hd = i || mem i tl

(* tail-rec *)
let rec memq i = function
  | Empty		-> false
  | Item(hd, tl)	-> hd == i || mem i tl

(* tail-rec *)
let rec find func = function
  | Empty		-> raise(Not_found)
  | Item(hd, tl)	-> if func hd then hd else find func tl

(* tail-rec *)
let filter func list =
  let rec __filter a = function
    | Empty		-> rev a
    | Item(hd, tl)	-> if func hd then __filter (Item(hd, a)) tl else __filter a tl
  in __filter Empty list

(* tail-rec *)
let find_all func list = filter func list

(* tail-rec *)
(* let partition func list = *)
(*   let rec __partition a b = function *)
(*     | Empty		-> Item((rev a), (rev b)) *)
(*     | Item(hd, tl) -> if func hd then __partition (Item(hd, a)) b tl else __partition a (Item(hd, b)) tl *)
(*   in __partition Empty Empty list *)

(* tail-rec *)
let rec assoc key = function
  | Empty		-> raise(Not_found)
  | Item((k, v), tl)	-> if k = key then v else assoc key tl

(* tail-rec *)
let rec assq key = function
  | Empty		-> raise(Not_found)
  | Item((k, v), tl)	-> if k == key then v else assq key tl

(* tail-rec *)
let rec mem_assoc key = function
  | Empty		-> false
  | Item((k, v), tl)	-> if k = key then true else mem_assoc key tl

(* tail-rec *)
let rec mem_assq key = function
  | Empty		-> false
  | Item((k, v), tl)	-> if k == key then true else mem_assq key tl

(* not-tail-rec *)
let rec remove_assoc key = function
  | Empty		-> Empty
  | Item((k, v), tl)	-> if k = key then tl else (Item((k, v), (remove_assoc key tl)))

(* not-tail-rec *)
let rec remove_assq key = function
  | Empty		-> Empty
  | Item((k, v), tl)	-> if k == key then tl else (Item((k, v), (remove_assq key tl)))

(* not-tail-rec *)
let rec split = function
  | Empty		-> (Empty, Empty)
  | Item((k, v), tl)	-> let (x, y) = split tl
			   in (Item(k, x), Item(v, y))

(* not-tail-rec *)
let rec combine la lb = match (la, lb) with
  | (Empty, Empty)			-> Empty
  | (Item(hda, tla), Item(hdb, tlb))	-> Item((hda, hdb), combine tla tlb)
  | (_, _)				-> raise(Invalid_argument "List.combine")
