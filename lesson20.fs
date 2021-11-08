// 49.5.1
let even_seq = Seq.initInfinite (fun i -> 2*i)
//printfn "%d" (Seq.nth 5 even_seq)


// 49.5.2
let rec factorial = function
 | 1  -> 1
 | n  -> n * factorial(n - 1)

let fac_seq = Seq.initInfinite factorial
//printfn "%d" (Seq.nth 10 fac_seq)

// 49.5.3

let rec sequence n =
    if n = 0 then 0
    elif n % 2 = 0 then (n / 2)
    else n / 2 - n

let seq_seq = Seq.initInfinite sequence
//printfn "%d" (Seq.nth 2 seq_seq)