type t
val empty : t
val is_empty : t -> bool
(* val mem : Name.atom -> t -> bool *)
val add : Name.atom -> t -> t
val singleton : Name.atom -> t
(* val remove : Name.atom -> t -> t *)
val union : t -> t -> t
(* val inter : t -> t -> t *)
(* val diff : t -> t -> t *)
(* val compare : t -> t -> int *)
(* val equal : t -> t -> bool *)
(* val subset : t -> t -> bool *)
val sublist : t -> Name.atom list -> bool
(* val iter : (Name.atom -> unit) -> t -> unit *)
(* val fold : (Name.atom -> 'a -> 'a) -> t -> 'a -> 'a *)
(* val for_all : (Name.atom -> bool) -> t -> bool *)
(* val exists : (Name.atom -> bool) -> t -> bool *)
(* val filter : (Name.atom -> bool) -> t -> t *)
(* val partition : (Name.atom -> bool) -> t -> t * t *)
(* val cardinal : t -> int *)
val elements : t -> Name.atom list
(* val min_Name.atom : t -> Name.atom *)
(* val max_Name.atom : t -> Name.atom *)
(* val choose : t -> Name.atom *)
(* val split : Name.atom -> t -> t * bool * t *)
(* val find : Name.atom -> t -> Name.atom *)
(* val of_list : Name.atom list -> t *)
