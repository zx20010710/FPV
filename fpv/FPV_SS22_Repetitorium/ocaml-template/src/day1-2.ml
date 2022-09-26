let rec fold_left f acc =function
|[]->acc
|x::xs ->fold_left f(f acc x) xs

let length =fold_left (fun acc _-> acc+1) 0

let sum_int =fold_left (fun acc x->acc+x) 0

(* let sum_int =fold_left (+) 0 *)

let sum_float =fold_left(fun acc x->acc+.x) 0.

(* let sum_float =float_left (+.) 0 *)

let prod_int =fold_left ( * ) 1

let map f =fold_left (fun acc x->acc@[f x]) []

let filter f= fold_left (fun list x->list @if f x then [x] else []) []

let partiton f =fold_left 
  (fun (t,f) x -> if x then (t@[x],f) else (t,f@[x]))
  ([],[])