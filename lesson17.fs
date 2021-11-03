// 43.3

//let map1 = Map.ofList [(128,"128128"); (32,"3232"); (24,"2424"); (18,"1818")]
let try_find key m =
      try
      Some (Map.find key m)
      with
      | :? System.Collections.Generic.KeyNotFoundException -> None

//let result = try_find 12 map1
//let b =2;