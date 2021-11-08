// 48.4.1
let rec fibo1 n n1 n2 =
    if (n = 0) then 0
    elif n = 1 then 1
    elif n = 2 then (n1 + n2)
    else fibo1 (n - 1) (n1 + n2) n1

//printfn "%d" ( fibo1 6 1 0)
//printfn "%d" (fibo1 6 5 3)

//48.4.2
let rec fibo2 n c =
  if (n = 0) then c 0
  elif (n = 1) then c 1
  else (fibo2 (n-1) (fun f-> c f) ) + (fibo2 (n-2) (fun f-> c f) )

//printfn "%d" (fibo2 8 id)
//let b =2

// 48.4.3
let rec bigList n k =
    let rec f acc lst =
        if acc = 0 then k lst
        else f (acc-1) (1 :: k lst)
    f n []


//printfn "%A" (bigList 5 id)
//printfn "%A" (bigList 230000 id)