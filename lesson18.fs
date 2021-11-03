// 47.4.1
let f n =
    if n < 2 then
        1
    else
        let mutable index = ref 1
        let mutable result = ref 1
        while !index <= n do
            result := !result * !index
            index := !index + 1
        !result
//let result = f 3
//let b =2



// 47.4.2

let fibo n =
  if(n = 0) then 0
  elif(n = 1) then 1
  else
        let mutable elemN2 = ref 0
        let mutable elemN1 = ref 1
        let mutable index = ref 2
        let mutable result = ref 0
        while !index <= n do
            result := !elemN2 + !elemN1
            elemN2 := !elemN1
            elemN1 := !result
            index := !index + 1
        !result

//let result = fibo 5
//let b = 2
