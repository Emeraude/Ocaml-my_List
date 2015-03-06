type 'a my_list =
  | Item of ('a * 'a my_list)
  | Empty

let rec length = function
  | Empty	-> 0
  | Item(a, b)	-> (length b) + 1

let hd = function
  | Empty	-> raise(Failure "hd")
  | Item(a, b)	-> a

let tl = function
  | Empty	-> raise(Failure "tl")
  | Item(a, b)	-> b

let rec nth list nb =
  if (nb < 0)
  then raise(Invalid_argument "List.nth")
  else match list with
    | Empty		-> raise(Failure "nth")
    | Item(a, b)	-> if nb = 0 then a else nth b (nb - 1)

let rec rev_append a b = match a with
  | Empty		-> b
  | Item(hd, tl)	-> rev_append tl (Item(hd, b))

let rec rev a =
  rev_append a Empty

let append a b =
  rev_append (rev a) b

let rec flatten = function
  | Empty		-> Empty
  | Item(hd, tl)	-> append hd (flatten tl)

let concat list =
  flatten list

let rec iter func list = match list with
  | []		-> ()
  | hd::tl	-> begin
    func hd;
    iter func tl;
  end
