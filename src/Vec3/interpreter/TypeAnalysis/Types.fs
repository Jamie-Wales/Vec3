/// <summary>
/// Types for the type checker.
/// </summary>
module Vec3.Interpreter.Typing.Types

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open Vec3.Interpreter.Typing.Exceptions

/// <summary>
/// A type scheme for a binding.
/// </summary>
type Scheme = Forall of TypeVar list * TType

/// <summary>
/// The result given by the type checker.
/// </summary>
type TypeResult<'a> = Result<'a, TypeErrors>

/// <summary>
/// A map of substitutions to apply to a type.
/// </summary>
type Substitution = Map<TypeVar, TType>

/// <summary>
/// The type environment.
/// </summary>
type TypeEnv = Map<Lexeme, TType>

/// <summary>
/// Already resolved types.
/// </summary>
type ResolvedType = Map<TypeVar, TType>

/// <summary>
/// Resolved tensor dimensions.
/// </summary>
type ResolvedDims = Map<TypeVar, Dims>

/// <summary>
/// Map of aliases to types.
/// </summary>
type AliasMap = Map<Lexeme, TType>
