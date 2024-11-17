/// <summary>
/// This module contains the prelude of the Vec3 language.
/// Definitions of built-in functions and list operations.
/// </summary>
module Vec3.Interpreter.Prelude

open Parser
open Vec3.Interpreter.Typing

let prelude = """
// wrapper functions for built-in functions
// to make them first-class
let cos = (x) -> BUILTIN_COS(x)
let sin = (x) -> BUILTIN_SIN(x)
let tan = (x) -> BUILTIN_TAN(x)
let acos = (x) -> BUILTIN_ACOS(x)
let asin = (x) -> BUILTIN_ASIN(x)
let atan = (x) -> BUILTIN_ATAN(x)
let log = (x, y) -> BUILTIN_LOG(x, y)
let exp = (x) -> BUILTIN_EXP(x)
let sqrt = (x) -> BUILTIN_SQRT(x)
let abs = (x) -> BUILTIN_ABS(x)
let floor = (x) -> BUILTIN_FLOOR(x)
let ceil = (x) -> BUILTIN_CEIL(x)
let trunc = (x) -> BUILTIN_TRUNC(x)

// list operations
let max = (x, y) -> if x > y then x else y
let min = (x, y) -> if x < y then x else y

// standard identity function
let id = (x) -> x

// find the tail of a list
let tail = (list) -> if list == [] then 
						error("tail of empty list") 
					  else 
						list[1..]

// find the length of a list
rec len(list) -> if list == [] then 
					0 
				 else 
					1 + len(tail(list))

// find the head of a list (may throw an error)
let head = (x) -> if len(x) == 0 then 
					error("head of empty list") 
				  else 
					x[0]
     
// fold a list with a function
rec fold(list, acc, func) -> if len(list) == 0 then 
								acc 
							 else 
							 	func(head(list), (fold(tail(list), acc, func)))

// map a function over a list
rec map(list, func) -> if len(list) == 0 then 
							[] 
						else
							func(head(list)) :: map(tail(list), func)

// find the sum, product, any, and all of a list
let sum = (list) -> fold(list, 0, (x, y) -> x + y)
let product = (list) -> fold(list, 1, (x, y) -> x * y)
let any = (list) -> fold(list, false, (x, y) -> x || y)
let all = (list) -> fold(list, true, (x, y) -> x && y)

// check if any or all elements of a list satisfy a predicate
let anyAre = (list, func) -> any(map(list, func))
let allAre = (list, func) -> all(map(list, func))

// find the range of numbers from start to end
rec range(start, end) -> if start >= end then 
								[]
							else
								start :: range(start + 1, end)
"""

/// <summary>
/// Preparsed prelude program.
/// </summary>
let preludeParsed =
    match parse prelude with
    | Ok(_, program) ->
        program
    | _ -> failwith "error parsing"

/// <summary>
/// Pre-typechecked prelude program.
/// </summary>
let preludeChecked =
    match Inference.inferProgram1 preludeParsed with
    | Ok(env, aliases, sub, program) -> env, aliases, sub, program
    | _ -> failwith "error type checking"