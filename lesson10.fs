type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y =
 match x.f,y.f with
 |"AM","PM"->  false
 |"PM","AM"-> true
 |_-> x>y


//let time1 = { hours = 3; minutes = 45;f= "PM" }
//let time2 = { hours = 2; minutes = 45;f="PM" }
//
//let result = time1 .>.time2
//
//printfn "%b" result
