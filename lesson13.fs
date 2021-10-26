// 39.1
let rec rmodd =
    function
    | [] -> []
    | [x] -> []
    | head :: head2 :: tail -> [ head2 ] @ rmodd tail

//let result = rmodd [0;2;3;6;8;9;5;3]
//let a = 1

// 39.2
let rec del_even =
    function
    | [] -> []
    | head :: tail when (head % 2 = 0) -> del_even tail
    | head :: tail when (head % 2 = 1) -> [ head ] @ del_even tail
//let result = del_even [0;2;3;6;8;9;5;3]
//let a = 1

// 39.3
let rec multiplicity x xs =
    match x, xs with
    | x, [] -> 0
    | x, head :: tail when (head <> x) -> multiplicity x tail
    | x, head :: tail when (head = x) -> 1 + multiplicity x tail
//let result = multiplicity 5 [0;5;3;6;8;9;5;5]
//let a = 1


// 39.4
let rec split =
    let rec even =
        function
        | [ x ] -> [ x ]
        | head :: (_ :: tail) -> head :: even tail
        | _ -> []



    let rec odd =
        function
        | [ _ ] -> []
        | _ :: (head :: tail) -> head :: odd tail
        | _ -> []

    fun xs -> (even xs, odd xs)

//let result = split [0;1;2;3;4;5]
//let a = 1


// 39.5
let rec zip (xs1,xs2) =
    match xs1, xs2 with
    | [], [] -> []
    | head1 :: tail1, head2 :: tail2 when (xs1.Length = xs2.Length) ->[(head1,head2)] @ zip (tail1,tail2)
    | head1 :: tail1, head2 :: tail2 when (xs1.Length <> xs2.Length) -> failwith "error"


//let result = zip ([1;2;3;4],[4;3;7;5])
//let a = 1