exception SimpTag

let a = SimpTag


let oc = open_out "test.out"
let _ =  output_value oc a; close_out oc


