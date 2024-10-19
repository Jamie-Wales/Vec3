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
    | CLOSURE
    | ASSERT
    | DOTPRODUCT
    | CROSSPRODUCT
    
    | COMPOUND_CREATE
    | COMPOUND_GET
    
    | BLOCK_START
    | BLOCK_END
    | BLOCK_RETURN

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
    | CLOSURE -> 26uy
    | ASSERT -> 27uy
    | DOTPRODUCT -> 28uy
    | CROSSPRODUCT -> 29uy
    
    | COMPOUND_CREATE -> 30uy
    | COMPOUND_GET -> 31uy
    
    | BLOCK_START -> 32uy
    | BLOCK_END -> 33uy
    | BLOCK_RETURN -> 34uy
    

let byteToOpCode byte =
    match byte with
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
    | 26uy -> CLOSURE
    | 27uy -> ASSERT
    | 28uy -> DOTPRODUCT
    | 29uy -> CROSSPRODUCT
    | 30uy -> COMPOUND_CREATE
    | 31uy -> COMPOUND_GET
    | 32uy -> BLOCK_START
    | 33uy -> BLOCK_END
    | 34uy -> BLOCK_RETURN
    | _ -> failwith $"Unknown OP_CODE: {byte}"

let opCodeToString =
    function
    | CONSTANT -> "CONSTANT"
    | CONSTANT_LONG -> "CONSTANT_LONG"
    | ADD -> "ADD"
    | SUBTRACT -> "SUBTRACT"
    | MULTIPLY -> "MULTIPLY"
    | DIVIDE -> "DIVIDE"
    | NEGATE -> "NEGATE"
    | RETURN -> "RETURN"
    | NIL -> "NIL"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | NOT -> "NOT"
    | EQUAL -> "EQUAL"
    | GREATER -> "GREATER"
    | LESS -> "LESS"
    | PRINT -> "PRINT"
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
    | ASSERT -> "ASSERT"
    | DOTPRODUCT -> "DOTPRODUCT"
    | CROSSPRODUCT -> "CROSSPRODUCT"
    | COMPOUND_CREATE -> "COMPOUND_CREATE"
    | COMPOUND_GET -> "COMPOUND_GET"
    
    | BLOCK_START -> "BLOCK_START"
    | BLOCK_END -> "BLOCK_END"
    | BLOCK_RETURN -> "BLOCK_RETURN"
    
    
