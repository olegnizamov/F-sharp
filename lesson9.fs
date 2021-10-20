let (.+.) x y =
    let (gold1, silver1, copper1) = x
    let (gold2, silver2, copper2) = y
    let resultCopper = (copper1+copper2)%12
    let resultSilver = (silver1+silver2  + (copper1+copper2)/12)%20
    let resultGold = (gold1+ gold2+ (silver1+silver2  + (copper1+copper2)/12)/20)
    (resultGold,resultSilver,resultCopper)


let (.-.) x y =
     let (gold1, silver1, copper1) = x
     let z = (-gold1,-silver1,-copper1)
     z .+. y

//let result = (1,12,12) .+. (0,12,12)
//let (gold, silver, copper) = result
//printfn  "gold = %d" gold
//printfn  "silver = %d" silver
//printfn  "copper = %d" copper

let (.+) x y =
    let (a, b) = x
    let (c, d) = y
    (a + c, b + d)

let (.*) x y =
    let (a, b) = x
    let (c, d) = y
    (a * c - b * d, b * c + a * d)

let (.-) x y =
    let (c, d) = y
    let z = (-c,-d)
    x .+ z

let (./) x y =
    let (c, d) = y
    let z = (c/(c*c+d*d),-d/(c*c+d*d))
    x .* z