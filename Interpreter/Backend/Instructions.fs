module Vec3.Interpreter.Backend.Instructions

type OP_CODE =
    | CONSTANT
    | CONSTANT_LONG
    | ADD
    | SUBTRACT
    | MULTIPLY
    | DIVIDE
    | NEGATE
    | RETURN
    | NIL
    | TRUE
    | FALSE
    | NOT
    | EQUAL
    | GREATER
    | LESS
    | PRINT
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

let opCodeToByte =
    function
    | CONSTANT -> 0uy
    | CONSTANT_LONG -> 1uy
    | ADD -> 2uy
    | SUBTRACT -> 3uy
    | MULTIPLY -> 4uy
    | DIVIDE -> 5uy
    | NEGATE -> 6uy
    | RETURN -> 7uy
    | NIL -> 8uy
    | TRUE -> 9uy
    | FALSE -> 10uy
    | NOT -> 11uy
    | EQUAL -> 12uy
    | GREATER -> 13uy
    | LESS -> 14uy
    | PRINT -> 15uy
    | POP -> 16uy
    | DEFINE_GLOBAL -> 17uy
    | GET_GLOBAL -> 18uy
    | SET_GLOBAL -> 19uy
    | GET_LOCAL -> 20uy
    | SET_LOCAL -> 21uy
    | JUMP -> 22uy
    | JUMP_IF_FALSE -> 23uy
    | LOOP -> 24uy
    | CALL -> 25uy

let byteToOpCode =
    function
    | 0uy -> CONSTANT
    | 1uy -> CONSTANT_LONG
    | 2uy -> ADD
    | 3uy -> SUBTRACT
    | 4uy -> MULTIPLY
    | 5uy -> DIVIDE
    | 6uy -> NEGATE
    | 7uy -> RETURN
    | 8uy -> NIL
    | 9uy -> TRUE
    | 10uy -> FALSE
    | 11uy -> NOT
    | 12uy -> EQUAL
    | 13uy -> GREATER
    | 14uy -> LESS
    | 15uy -> PRINT
    | 16uy -> POP
    | 17uy -> DEFINE_GLOBAL
    | 18uy -> GET_GLOBAL
    | 19uy -> SET_GLOBAL
    | 20uy -> GET_LOCAL
    | 21uy -> SET_LOCAL
    | 22uy -> JUMP
    | 23uy -> JUMP_IF_FALSE
    | 24uy -> LOOP
    | 25uy -> CALL
    | _ -> failwith "Unknown OP_CODE"
