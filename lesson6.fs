// 17.1
let rec pow =
    function
    | (char, 0) -> ""
    | (char, n) -> char + pow (char, n - 1)
//printfn "%s" (pow ("a", 5))


// 17.2
let rec isIthChar =
    function
    | (s,n,c) when n < 0 -> false
    | (s,n,c) when ((String.length s) <= n) -> false
    | (s, n, c) -> (string s).[n] = c

//printfn "%b" (isIthChar ("aaac", 3,'d'))


// 17.3
let rec occFromIth (str,number,char)=
    (let rec iterator =
        function
        | (s, n, c, count) when ((String.length s) <= n) -> count
        | (s, n, c, count) when  ((string s).[n] = c) -> iterator(s,n+1,c,count+1)
        | (s, n, c, count) when  ((string s).[n] <> c) -> iterator(s,n+1,c,count)
        | _ -> 0

     iterator (str, number, char, 0))


//printfn "%d" (occFromIth ("abcabcabc", 0,'a'))