// 34.1
let rec upto =
    function
    | 1 -> [1]
    | n -> upto(n-1) @ [n]


// 34.2
let rec dnto =
    function
    | 1 -> [1]
    | n -> n::dnto(n-1)

//let result = dnto 10


// 34.3
let rec evenn =
    function
    | 1 -> [0]
    | n when n<1 -> []
    | n when (n%2=0)-> evenn(n-2)@[n]
    | n when (n%2=1)-> evenn(n-1)

//let result = evenn 11
//let a = 1