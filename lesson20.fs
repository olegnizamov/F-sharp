// 49.5.1
let even_seq = Seq.initInfinite (fun i -> 2*i+2)
//printfn "%d" (Seq.nth 5 even_seq)


// 49.5.2
let factorial n =
    let rec f x a =
        if x <= 1 then a
        else f (x - 1) (a * x)
    f n 1

let fac_seq = Seq.initInfinite factorial
//printfn "%d" (Seq.nth 10 fac_seq)

// 49.5.3

let rec sequence n =
    if n = 0 then 0
    elif n % 2 = 0 then (n / 2)
    else n / 2 - n

let seq_seq = Seq.initInfinite sequence
//printfn "%d" (Seq.nth 2 seq_seq)