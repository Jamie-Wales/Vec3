module Vec3.Interpreter.Prelude

let prelude = """
let cos = (x) -> BUILTIN_COS(x)
let sin = (x) -> BUILTIN_SIN(x)
let tan = (x) -> BUILTIN_TAN(x)
let acos = (x) -> BUILTIN_ACOS(x)
let asin = (x) -> BUILTIN_ASIN(x)
let atan = (x) -> BUILTIN_ATAN(x)
let log = (x, y) -> BUILTIN_LOG(x, y)
let exp = (x) -> BUILTIN_EXP(x)
let log10 = (x) -> BUILTIN_LOG10(x)
let sqrt = (x) -> BUILTIN_SQRT(x)
let len = (x) -> BUILTIN_LEN(x)
let abs = (x) -> BUILTIN_ABS(x)
let floor = (x) -> BUILTIN_FLOOR(x)
let ceil = (x) -> BUILTIN_CEIL(x)
let trunc = (x) -> BUILTIN_TRUNC(x)

let id = (x) -> x

"""

