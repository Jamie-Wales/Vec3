module Vec3.Interpreter.Prelude

open Parser
open Vec3.Interpreter.Typing

let prelude = """
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

let id = (x) -> x

let tail = (list) -> if list == [] then 
						error("tail of empty list") 
					  else 
						list[1..]

rec len(list) -> if list == [] then 
					0 
				 else 
					1 + len(tail(list))

let head = (x) -> if len(x) == 0 then 
					error("head of empty list") 
				  else 
					x[0]
     
rec fold(list, acc, func) -> if len(list) == 0 then 
								acc 
							 else 
							 	func(head(list), (fold(tail(list), acc, func)))

rec map(list, func) -> if len(list) == 0 then 
							[] 
						else
							func(head(list)) :: map(tail(list), func)

let sum = (list) -> fold(list, 0, (x, y) -> x + y)
let product = (list) -> fold(list, 1, (x, y) -> x * y)
let any = (list) -> fold(list, false, (x, y) -> x || y)
let all = (list) -> fold(list, true, (x, y) -> x && y)
let anyAre = (list, func) -> any(map(list, func))
let allAre = (list, func) -> all(map(list, func))

rec range(start, end) -> if start >= end then 
								[]
							else
								start :: range(start + 1, end)
"""

let preludeParsed =
    match parse prelude with
    | Ok(_, program) ->
        program
    | _ -> failwith "error parsing"

let preludeChecked =
    match Inference.inferProgram1 preludeParsed with
    | Ok(env, aliases, sub, program) -> env, aliases, sub, program
    | _ -> failwith "error type checking"