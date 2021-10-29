// 40.1
let rec sum (p, xs) =
    match xs with
    | [] -> 0
    | [ x ] when p x -> x
    | head :: tail when (p head) -> head + sum (p, tail)
    | _ :: tail -> 0 + sum (p, tail)
    | _ -> 0

// 40.2.1
let rec count (xs, n) =
    match xs, n with
    | [], n -> 0
    | head :: tail, n when head < n -> count (tail, n)
    | head :: tail, n when head = n -> 1 + count (tail, n)
    | head :: tail, n when head > n -> 0
    | _ -> 0

//let result = count ([0;1;2;3;3;4;5;5;5;7;8;9;10],5)
//let a = 1

// 40.2.2
let rec insert (xs, n) =
    match xs, n with
    | head :: tail, n when head <= n -> [ head ] @ insert (tail, n)
    | head :: tail, n when head > n -> [ n ] @ head :: tail
    | [], n -> [ n ]
    | _, n -> []

//let result = insert ([0;1;2;3;3;4;5;5;5;7;8;9;10],6)
//let a = 1


//40.2.3.
let rec intersect (xs1, xs2) =
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> head1 :: intersect (tail1, tail2)
    | (head1 :: _, head2 :: tail2) when head1 > head2 -> intersect (xs1, tail2)
    | (head1 :: tail1, head2 :: _) when head1 < head2 -> intersect (tail1, xs2)
    | ([ x ], [ y ]) when x = y -> [ x ]
    | _ -> []


//let result = intersect ([0;1;2;4;4;4;5;5;5;7;8;9;10],[1;2;3;5])
//let a = 1


// 40.2.4.
let rec plus (xs1, xs2) =
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: _) when head1 < head2 -> [ head1 ] @ plus (tail1, xs2)
    | (head1 :: _, head2 :: tail2) when head1 > head2 -> [ head2 ] @ plus (xs1, tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> [ head1 ] @ [ head2 ] @ plus (tail1, tail2)
    | (xs1, [ ]) -> xs1
    | ([], xs2) -> xs2
    | ([ x ], [ y ]) when x = y-> [ x ] @ [ y ]
    | ([ x ], [ y ]) when x > y-> [ y ] @ [ x ]
    | ([ x ], [ y ]) when x < y -> [ x ] @ [ y ]
    | _ -> []

//let result = plus ([1;2;3],[1;4;5])
//let a = 1


// 40.2.5.
let rec minus (xs1, xs2) =
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: _) when head1 < head2 -> [ head1 ] @ minus (tail1, xs2)
    | (head1 :: _, head2 :: tail2) when head1 > head2 ->  minus (xs1, tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> minus (tail1, tail2)
    | (xs1, [ ]) -> xs1
    | ([], xs2) -> []
    | ([ x ], [ y ]) when x = y-> [ ]
    | ([ x ], [ y ]) when x <> y-> [ x ]
    | _ -> []
//
//let result = minus ([1;2;3],[1;2;4;5])
//let a = 1

// 40.3.1.
let rec iterator (xs, min) =
        match xs with
        | head::tail when head > min  -> iterator(tail,min)
        | head::tail when head <= min  -> iterator(tail,head)
        | [  ] -> Some min
        | _ -> Some min

let rec smallest = fun xs ->
    let head :: tail = xs
    iterator(tail,head)
//let result = smallest ([1;2;3;4;5;6;7;1;0])
//let a = 1


//40.3.2. Напишите функцию delete: int * int list -> int list, которая удаляет из списка первое вхождение заданного элемента (если он имеется).
let rec delete (n, xs) =
    match xs with
    | [] -> []
    | head :: tail when head <> n -> [head] @ delete(n,tail)
    | head :: tail when head = n -> tail
    | _ -> []

//let result = delete (11,[1;2;3;3;3;3;4;5;6;7;1;0])
//let a = 1

// 40.3.3.
let rec sort = fun xs ->
    let rec iteratorSort (list: 'b list) result =
        if(list.Length=0) then result
        else
            let min = smallest list
            let tail = delete(min.Value,list)
            iteratorSort tail (result @ [min.Value])

    iteratorSort xs []

//let result = sort ([1;2;3;4;5;6;7;1;0])
//let a = 1


// 40.4.
let rec revrev = fun (xs: list<list<int>>) ->
   match xs with
   | [] -> []
   | [ x ] -> [ List.rev x ]
   | head :: tail -> revrev tail @ [ List.rev head ]

//let result = revrev ([[1;2];[3;4]])
//let a = 1
