let rec length =function
|[]-> 0
|_::xs -> 1+length xs

let rec sum_int =function
|[] -> 0
|x::xs -> x+ sum_int xs

let rec sum_float =function 
|[] ->0.
|x::xs ->x +. sum_float xs

let rec prod_int =function 
|[] ->1
|x::xs ->x* prod_int xs

let rec map f = function 
|[]->[]
|x::xs -> f x::map f xs

let rec fold_left f acc =function
|[]->[]
|x::xs -> if f x then fold_left f (f acc x) xs



let rec fold_right f acc=function
|[]->[]
|x::xs -> f x (fold_right f xs acc)

(*fold_right doesn't belong to tail-recursive,while fold_left does*)

let rec filter f= function
|[] -> []
|x::xs ->if f x then x:: filter f xs

