module Vec3.Interpreter.Backend.Instructions

type OP_CODE =
    | CONSTANT
    | CONSTANT_LONG
    | RETURN
    | NIL
    | TRUE
    | FALSE
    | POP
    | DEFINE_GLOBAL
    | GET_GLOBAL
    | SET_GLOBAL
    | GET_LOCAL
    | SET_LOCAL
    | JUMP
    | JUMP_IF_FALSE
    | LOOP
    | CALL
    | CLOSURE
    
    | COMPOUND_CREATE
    | COMPOUND_GET
    
let opCodeToByte =
    function
    | CONSTANT -> 0uy
    | CONSTANT_LONG -> 1uy
    | RETURN -> 7uy
    | NIL -> 8uy
    | TRUE -> 9uy
    | FALSE -> 10uy
    | POP -> 15uy
    | DEFINE_GLOBAL -> 16uy
    | GET_GLOBAL -> 17uy
    | SET_GLOBAL -> 18uy
    | GET_LOCAL -> 19uy
    | SET_LOCAL -> 20uy
    | JUMP -> 21uy
    | JUMP_IF_FALSE -> 22uy
    | LOOP -> 23uy
    | CALL -> 24uy
    | CLOSURE -> 25uy
    
    | COMPOUND_CREATE -> 26uy
    | COMPOUND_GET -> 27uy
    

let byteToOpCode byte =
    match byte with
    | 0uy -> CONSTANT
    | 1uy -> CONSTANT_LONG
    | 7uy -> RETURN
    | 8uy -> NIL
    | 9uy -> TRUE
    | 10uy -> FALSE
    | 15uy -> POP
    | 16uy -> DEFINE_GLOBAL
    | 17uy -> GET_GLOBAL
    | 18uy -> SET_GLOBAL
    | 19uy -> GET_LOCAL
    | 20uy -> SET_LOCAL
    | 21uy -> JUMP
    | 22uy -> JUMP_IF_FALSE
    | 23uy -> LOOP
    | 24uy -> CALL
    | 25uy -> CLOSURE
    | 26uy -> COMPOUND_CREATE
    | 27uy -> COMPOUND_GET
    | _ -> failwith $"Unknown OP_CODE: {byte}"

let opCodeToString =
    function
    | CONSTANT -> "CONSTANT"
    | CONSTANT_LONG -> "CONSTANT_LONG"
    | RETURN -> "RETURN"
    | NIL -> "NIL"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | POP -> "POP"
    | DEFINE_GLOBAL -> "DEFINE_GLOBAL"
    | GET_GLOBAL -> "GET_GLOBAL"
    | SET_GLOBAL -> "SET_GLOBAL"
    | GET_LOCAL -> "GET_LOCAL"
    | SET_LOCAL -> "SET_LOCAL"
    | JUMP -> "JUMP"
    | JUMP_IF_FALSE -> "JUMP_IF_FALSE"
    | LOOP -> "LOOP"
    | CALL -> "CALL"
    | CLOSURE -> "CLOSURE"
    | COMPOUND_CREATE -> "COMPOUND_CREATE"
    | COMPOUND_GET -> "COMPOUND_GET"
    
    
