let f x = x + 1
let x   = f 1
let y   = f (f (f 1))

let g x y z = x + y + z
let h x y   = g x y 5

let z = h 1 2
