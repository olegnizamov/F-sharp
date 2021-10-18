// 20.3.1
let vat n x = x + (x * (float n) / 100.0)
//printfn "%f" (vat 10 1.0)


// 20.3.2
let unvat n x = x * (100.0 / (100.0 + (float n)))
//printfn "%f" (unvat 10 1.1)


// 20.3.3
let rec min f =
    (let rec iterator number =
        if f (number) = 0 then
            number
        else
            iterator (number + 1)

     iterator 1)
