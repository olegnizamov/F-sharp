let fac_seq =
    let rec factorial n acc = seq {
        if n <= 1 then yield 1
        else yield acc
        yield! factorial (n + 1) (acc * (n+1)) }
    factorial 0 1
//printfn "%d" (Seq.nth 10 fac_seq)

// 49.5.3

let seq_seq =
    let rec inner n = seq {
        if n % 2 = 0 then yield (n / 2)
        else yield (-(n + 1) / 2)
        yield! inner (n + 1) }
    inner 0
//printfn "%d" (Seq.nth 2 seq_seq)