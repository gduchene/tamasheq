let x = Int32.of_int 1
let y = Int64.of_int 1
let z = Int64.to_int32 y |> Int32.add x |> Nativeint.of_int32

let a = Int32.to_int     x
let b = Int64.to_int     y
let c = Nativeint.to_int z
