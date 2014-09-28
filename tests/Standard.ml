let ( $ ) f x = f x

let () = print_endline "Hello world!"
let () = print_int $ List.length [1; 2; 3] |> print_newline
let () = Printf.printf "The length of [1; 2; 3] is %d\n" $ List.length [1; 2; 3]
