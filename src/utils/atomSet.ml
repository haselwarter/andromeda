include
  Set.Make(
      struct type t = Name.atom
             let compare (Name.Gensym (_, x)) (Name.Gensym (_, y)) =
               if x < y then -1 else if x > y then 1 else 0
      end)

let sublist s l = subset s (of_list l)
