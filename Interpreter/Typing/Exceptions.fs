module Vec3.Interpreter.Typing.Exceptions

open Vec3.Interpreter.Token
open Vec3.Interpreter.Grammar

type TType = Type

type TypeError =
    | InvalidIf of Expr
    | UndefinedVariable of Token
    | UndefinedFunction of Token
    | UndefinedType of Token
    | TypeMismatch of Token * TType * TType
    | InvalidAssignment of Token * TType * TType
    | InvalidArgumentCount of Expr * int * int
    | InvalidArgumentType of Expr * TType * TType
    | InvalidReturnType of Token * TType * TType
    | InvalidOperandType of Token * TType * TType
    | InvalidOperator of Token * TType
    | InvalidFunctionType of Token * TType
    | InvalidFunction of Token * TType
    | InvalidFunctionArgument of Token * TType * TType
    | InvalidFunctionReturn of Token * TType * TType
    | InvalidFunctionBody of Token * TType * TType
    | InvalidBlock of Token * TType * TType
    | InvalidCall of Expr * TType
    | InvalidCallType of Expr * TType * TType
    | InvalidCallReturn of Token * TType * TType
    | InvalidCallBody of Token * TType * TType
    | NotEnoughInformation of Token
    | InvalidOpen of Token
    | InvalidIndex of Expr * TType
    | InvalidAssert of Expr * TType

type TypeErrors = TypeError list
exception TypeException of TypeErrors

let formatTypeError (error: TypeError) : string =
    match error with
    | UndefinedVariable token -> $"Undefined variable {token.Lexeme} at Line: {token.Position.Line}"
    | UndefinedFunction token -> $"Undefined function {token.Lexeme} at Line: {token.Position.Line}"
    | UndefinedType token -> $"Undefined type {token.Lexeme} at Line: {token.Position.Line}"
    | TypeMismatch(token, expected, actual) -> $"Type mismatch at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidAssignment(token, expected, actual) ->
        $"Invalid assignment at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidArgumentCount(expr, expected, actual) ->
        $"Invalid argument count at expr: {expr}, expected {expected}, got {actual}"
    | InvalidArgumentType(expr, expected, actual) ->
        $"Invalid argument type at expr: {expr}, expected {expected}, got {actual}"
    | InvalidReturnType(token, expected, actual) ->
        $"Invalid return type at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidOperandType(token, expected, actual) ->
        $"Invalid operand type at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidOperator(token, typ) -> $"Invalid operator at Line: {token.Position.Line}, got {typ}"
    | InvalidFunctionType(token, typ) -> $"Invalid function type at Line: {token.Position.Line}, got {typ}"
    | InvalidFunction(token, typ) -> $"Invalid function at Line: {token.Position.Line}, got {typ}"
    | InvalidFunctionArgument(token, expected, actual) ->
        $"Invalid function argument at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidFunctionReturn(token, expected, actual) ->
        $"Invalid function return at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidFunctionBody(token, expected, actual) ->
        $"Invalid function body at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidBlock(token, expected, actual) -> $"Invalid block at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidCall(expr, typ) -> $"Invalid call at expr: {expr}, got {typ}"
    | InvalidCallType(expr, expected, actual) ->
        $"Invalid call type at expr: {expr}, expected {expected}, got {actual}"
    | InvalidCallReturn(token, expected, actual) ->
        $"Invalid call return at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidCallBody(token, expected, actual) ->
        $"Invalid call body at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | NotEnoughInformation(token) -> $"Not enough information at Line: {token.Position.Line}"
    | InvalidOpen(token) -> $"Invalid open statement at Line: {token.Position.Line}"
    | InvalidIf(expr) -> $"Invalid if statement at expr: {expr}"
    | InvalidIndex(expr, typ) -> $"Invalid index at expr: {expr}, got {typ}"
    | InvalidAssert(expr, typ) -> $"Invalid assert at expr: {expr}, got {typ}"

let formatTypeErrors (errors: TypeError list) : string =
    List.map formatTypeError errors |> String.concat "\n"
