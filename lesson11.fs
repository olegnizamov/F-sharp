type F =
  | AM
  | PM

type TimeOfDay = { hours: int; minutes: int; f: F }

let (.>.) x y =
 match x.f,y.f with
 |AM,PM->  false
 |PM,AM-> true
 |_-> x>y