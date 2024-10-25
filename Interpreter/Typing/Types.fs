module Vec3.Interpreter.Typing.Types

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open Vec3.Interpreter.Typing.Exceptions

type Scheme = Forall of TypeVar list * TType

type TypeResult<'a> = Result<'a, TypeErrors>

type Substitution = Map<TypeVar, TType>

type TypeEnv = Map<Lexeme, TType>

type ResolvedType = Map<TypeVar, TType>
type ResolvedDims = Map<TypeVar, Dims>
type AliasMap = Map<Lexeme, TType>
