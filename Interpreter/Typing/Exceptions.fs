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
