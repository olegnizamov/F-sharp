// 16.1
let notDivisible (n, m) = m % n = 0
//printfn "n isOdd m = %b" (notDivisible (5,10))

// 16.2
let prime n =
    (let rec isSimple =
        function
        | (number, bas) when (bas > (number / 2)) -> true
        | (number, bas) when (number % bas = 0) -> false
        | (number, bas) -> isSimple (number, bas+1)

     isSimple (n, 2))
//printfn "n isPrime = %b" (prime 11)