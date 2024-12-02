module Vec3.Interpreter.Backend.VM

open System
open Vec3.Interpreter
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Value  // Added since we use Value type
open Vec3.Interpreter.Token
open Grammar

// First declare the OpCodeResult type since other functions will use it
type OpCodeResult =
    | Return of Value
    | Call of byte * byte
    | Continue

// Basic operations
val output : seq<string> ref
val createOutputStreams : unit -> OutputStreams
val getCurrentFrame : VM -> CallFrame
val readByte : VM -> VM * byte
val readConstant : VM -> Value * VM
val readConstantLong : VM -> Value * VM
val saveVMState : VM -> unit
val appendToStream : seq<string> -> string -> seq<string>
val appendOutput : VM -> StreamType -> string -> VM
val push : VM -> Value -> VM
val pop : VM -> Value * VM
val peek : VM -> int -> Value
val defineGlobal : VM -> string -> Value -> VM
val getGlobal : VM -> string -> Value option

// VM execution functions
val executeOpcode : VM -> OP_CODE -> VM * OpCodeResult
val executeOpcodeImpl : VM -> OP_CODE -> VM
val runFrameRecursive : VM -> VM * Value
val runLoop : VM -> VM
val callValue : VM -> int -> int -> VM
val run : VM -> VM
val runFunction : VM -> Function -> Value list -> Value

// Creation and initialization
val loadFunction : VM -> Function -> VM
val createNewVM : Function -> VM
val specialCasedBuiltins : unit -> Map<string, Value>