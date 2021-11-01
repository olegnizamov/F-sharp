// 41.4.1
let list_filter f xs = List.foldBack  (fun head tail -> if f head then head::tail else tail) xs []
//let f = fun x -> x > 0
//let listFilter = (list_filter f [-1; 2; 3;4;-5])
//let a =1;

// 41.4.2
let sum (p, xs) = List.fold (+) 0 (List.filter p xs)

// 41.4.3
let revrev = fun lst -> List.fold (fun head tail -> (List.rev tail)::head) [] lst

//let l = (revrev [[1;2];[3;4;5];[4;6];[2;4;7];[1;2;3]])
//let a =1