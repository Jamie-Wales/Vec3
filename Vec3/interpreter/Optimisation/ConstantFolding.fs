/// <summary>
/// Constant folding for the AST.
/// </summary>
module Vec3.Interpreter.ConstantFolding

open Token
open Grammar

/// <summary>
/// Helper function to perform arithmetic operations on numbers.
/// </summary>
/// <param name="op">The operation to perform.</param>
/// <param name="left">The left operand.</param>
/// <param name="right">The right operand.</param>
/// <param name="expr">The original expression.</param>
/// <returns>The result of the operation, or the expression itself.</returns>
let performArithmetic op left right expr =
    match left, right with
    | ELiteral(LNumber(LFloat a), _), ELiteral(LNumber(LFloat b), _) ->
        match op with
        | Plus -> ELiteral(LNumber(LFloat(a + b)), TFloat)
        | Minus -> ELiteral(LNumber(LFloat(a - b)), TFloat)
        | Star -> ELiteral(LNumber(LFloat(a * b)), TFloat)
        | Slash -> ELiteral(LNumber(LFloat(a / b)), TFloat)
        | Caret
        | StarStar -> ELiteral(LNumber(LFloat(a ** b)), TFloat)
        | _ -> expr

    | ELiteral(LNumber(LInteger a), _), ELiteral(LNumber(LInteger b), _) ->
        match op with
        | Plus -> ELiteral(LNumber(LInteger(a + b)), TInteger)
        | Minus -> ELiteral(LNumber(LInteger(a - b)), TInteger)
        | Star -> ELiteral(LNumber(LInteger(a * b)), TInteger)
        | Slash -> ELiteral(LNumber(LInteger(a / b)), TInteger)
        | Caret
        | StarStar -> ELiteral(LNumber(LInteger(pown a b)), TInteger)
        | _ -> expr

    | ELiteral(LNumber(LRational(a, b)), _), ELiteral(LNumber(LRational(c, d)), _) ->
        match op with
        | Plus -> ELiteral(LNumber(LRational(a * d + b * c, b * d)), TRational)
        | Minus -> ELiteral(LNumber(LRational(a * d - b * c, b * d)), TRational)
        | Star -> ELiteral(LNumber(LRational(a * c, b * d)), TRational)
        | Slash -> ELiteral(LNumber(LRational(a * d, b * c)), TRational)
        | Caret
        | StarStar -> ELiteral(LNumber(LRational(pown a c, pown b d)), TRational)
        | _ -> expr

    | ELiteral(LNumber(LComplex(a, b)), _), ELiteral(LNumber(LComplex(c, d)), _) ->
        match op with
        | Plus -> ELiteral(LNumber(LComplex(a + c, b + d)), TComplex)
        | Minus -> ELiteral(LNumber(LComplex(a - c, b - d)), TComplex)
        | Star -> ELiteral(LNumber(LComplex(a * c - b * d, a * d + b * c)), TComplex)
        | Slash ->
            ELiteral(LNumber(LComplex((a * c + b * d) / (c * c + d * d), (b * c - a * d) / (c * c + d * d))), TComplex)
        | Caret
        | StarStar -> ELiteral(LNumber(LComplex(0.0, 0.0)), TComplex)
        | _ -> expr

    | _, _ -> expr

/// <summary>
/// Helper function to perform equality operations on values.
/// </summary>
/// <param name="a">The left operand.</param>
/// <param name="b">The right operand.</param>
/// <param name="expr">The original expression.</param>
/// <returns>The result of the operation, or the expression itself.</returns>
let rec performEquality a b expr =
    match a, b with
    | ELiteral(LNumber(LFloat a), _), ELiteral(LNumber(LFloat b), _) -> ELiteral(LBool(a = b), TBool)
    | ELiteral(LNumber(LInteger a), _), ELiteral(LNumber(LInteger b), _) -> ELiteral(LBool(a = b), TBool)
    | ELiteral(LNumber(LRational(a, b)), _), ELiteral(LNumber(LRational(c, d)), _) ->
        ELiteral(LBool(a * d = b * c), TBool)
    | ELiteral(LNumber(LComplex(a, b)), _), ELiteral(LNumber(LComplex(c, d)), _) ->
        ELiteral(LBool(a = c && b = d), TBool)
    | ELiteral(LBool a, _), ELiteral(LBool b, _) -> ELiteral(LBool(a = b), TBool)
    | ELiteral(LString a, _), ELiteral(LString b, _) -> ELiteral(LBool(a = b), TBool)
    | ELiteral(LUnit, _), ELiteral(LUnit, _) -> ELiteral(LBool(true), TBool)
    | EList(lst1, typ1), EList(lst2, typ2) ->
        if List.length lst1 <> List.length lst2 || typ1 <> typ2 then
            ELiteral(LBool(false), TBool)
        else
            /// <summary>
            /// Check that two lists are equal.
            /// </summary>
            /// <param name="lst1">The first list.</param>
            /// <param name="lst2">The second list.</param>
            /// <returns>True if the lists are equal, false otherwise.</returns>
            let rec loop lst1 lst2 =
                match lst1, lst2 with
                | [], [] -> ELiteral(LBool(true), TBool)
                | x :: xs, y :: ys ->
                    let eq = performEquality x y expr

                    match eq with
                    | ELiteral(LBool true, _) -> loop xs ys
                    | _ -> eq
                | _, _ -> ELiteral(LBool(false), TBool)

            loop lst1 lst2
    | ERecordExtend((field1, value1, typ1), record1, _), ERecordExtend((field2, value2, typ2), record2, _) ->
        if field1 <> field2 || typ1 <> typ2 then
            ELiteral(LBool(false), TBool)
        else
            let eq = performEquality value1 value2 expr

            match eq with
            | ELiteral(LBool true, _) -> performEquality record1 record2 expr
            | _ -> eq
    | _ -> ELiteral(LBool(false), TBool)

/// <summary>
/// A helper function to compare two values.
/// </summary>
/// <param name="a">The left operand.</param>
/// <param name="b">The right operand.</param>
/// <param name="expr">The original expression.</param>
/// <returns>The result of the comparison.</returns>
let performComparision a b expr : int =
    match a, b with
    | ELiteral(LNumber(LFloat a), _), ELiteral(LNumber(LFloat b), _) -> compare a b
    | ELiteral(LNumber(LInteger a), _), ELiteral(LNumber(LInteger b), _) -> compare a b
    | ELiteral(LNumber(LRational(a, b)), _), ELiteral(LNumber(LRational(c, d)), _) -> compare (a * d) (b * c)
    | ELiteral(LNumber(LComplex(a, b)), _), ELiteral(LNumber(LComplex(c, d)), _) -> compare a c
    | _, _ -> expr // ? what to do here

/// <summary>
/// Helper function to perform unary operations on numbers.
/// </summary>
/// <param name="op">The operation to perform.</param>
/// <param name="a">The operand.</param>
/// <param name="expr">The original expression.</param>
/// <returns>The result of the operation, or the expression itself.</returns>
let performUnary op a expr =
    match a with
    | ELiteral(LNumber(LFloat a), _) ->
        match op with
        | Plus -> ELiteral(LNumber(LFloat(a)), TFloat)
        | Minus -> ELiteral(LNumber(LFloat(-a)), TFloat)
        | _ -> expr // dont fail, just return the same value
    | ELiteral(LNumber(LInteger a), _) ->
        match op with
        | Plus -> ELiteral(LNumber(LInteger(a)), TInteger)
        | Minus -> ELiteral(LNumber(LInteger(-a)), TInteger)
        | _ -> expr
    | ELiteral(LNumber(LRational(a, b)), _) ->
        match op with
        | Plus -> ELiteral(LNumber(LRational(a, b)), TRational)
        | Minus -> ELiteral(LNumber(LRational(-a, b)), TRational)
        | _ -> expr
    | ELiteral(LNumber(LComplex(a, b)), _) ->
        match op with
        | Plus -> ELiteral(LNumber(LComplex(a, b)), TComplex)
        | Minus -> ELiteral(LNumber(LComplex(-a, -b)), TComplex)
        | _ -> expr
    | ELiteral(LBool b, _) ->
        match op with
        | Bang -> ELiteral(LBool(not b), TBool)
        | _ -> expr
    | _ -> expr


/// <summary>
/// Map of builtin functions to their implementations.
/// </summary>
let builtins: Map<Lexeme, Expr list -> Expr -> Expr> =
    [ Operator(Plus, Some Infix), (fun (lst: Expr list) -> performArithmetic Plus lst[0] lst[1])
      Operator(Minus, Some Infix), (fun lst -> performArithmetic Minus lst[0] lst[1])
      Operator(Star, Some Infix), (fun lst -> performArithmetic Star lst[0] lst[1])
      Operator(Slash, Some Infix), (fun lst -> performArithmetic Slash lst[0] lst[1])
      Operator(Caret, Some Infix), (fun lst -> performArithmetic Caret lst[0] lst[1])
      Operator(StarStar, Some Infix), (fun lst -> performArithmetic StarStar lst[0] lst[1])
      Operator(Plus, Some Prefix), (fun lst -> performUnary Plus lst[0])
      Operator(Minus, Some Prefix), (fun lst -> performUnary Minus lst[0])
      Operator(Bang, Some Prefix), (fun lst -> performUnary Bang lst[0])
      Operator(EqualEqual, Some Infix), (fun lst -> performEquality lst[0] lst[1])
      Operator(BangEqual, Some Infix),
      (fun lst expr ->
          match performEquality lst[0] lst[1] expr with
          | ELiteral(LBool b, _) -> ELiteral(LBool(not b), TBool)
          | _ -> expr)
      Operator(AmpersandAmpersand, Some Infix),
      (fun lst expr ->
          match lst[0], lst[1] with
          | ELiteral(LBool a, _), ELiteral(LBool b, _) -> ELiteral(LBool(a && b), TBool)
          | _, _ -> expr)
      Operator(PipePipe, Some Infix),
      (fun lst expr ->
          match lst[0], lst[1] with
          | ELiteral(LBool a, _), ELiteral(LBool b, _) -> ELiteral(LBool(a || b), TBool)
          | _, _ -> expr)
      Operator(Percent, Some Infix),
      (fun lst expr ->
          match lst[0], lst[1] with
          | ELiteral(LNumber(LInteger a), _), ELiteral(LNumber(LInteger b), _) ->
              ELiteral(LNumber(LInteger(a % b)), TInteger)
          | _, _ -> expr) ]
    |> Map.ofList

/// <summary>
/// Find the greatest common divisor of two numbers.
/// </summary>
/// <param name="a">The first number.</param>
/// <param name="b">The second number.</param>
/// <returns>The greatest common divisor.</returns>
let rec GCD a b =
    let r = a / b
    if r = 0 then b else GCD b r

/// <summary>
/// Simplify a rational number.
/// </summary>
/// <param name="rat">The rational number to simplify.</param>
/// <returns>The simplified rational number.</returns>
let simplifyRational (rat: Number) : Number =
    match rat with
    | LRational(a, b) ->
        let gcd = GCD b a
        let a = a / gcd
        let b = b / gcd
        LRational(a, b)
    | _ -> failwith "bad"

/// <summary>
/// Helper function to perform indexing on lists.
/// </summary>
/// <param name="expr">The expression to index.</param>
/// <param name="start">An optional start</param>
/// <param name="end_">An optional end</param>
/// <param name="isRange">Whether the index is a range.</param>
/// <param name="typ">The type of the expression.</param>
/// <returns>The indexed expression.</returns>
let performIndexRange expr start end_ typ =
    match expr, start, end_ with
    | EList(lst, t), ELiteral(LNumber(LInteger s), _), ELiteral(LNumber(LInteger e), _) ->
        if e = 0 then
            EList(List.skip s lst, t)
        else
            EList(List.skip s lst |> List.take (e - s + 1), t)
    | _ -> EIndexRange(expr, start, end_, typ)

/// <summary>
/// Fold constants in the program.
/// </summary>
/// <param name="program">The program to fold.</param>
/// <returns>The folded program.</returns>
let foldConstants (program: Program) : Program =
    /// <summary>
    /// Fold a list of statements.
    /// </summary>
    /// <param name="stmts">The statements to fold.</param>
    /// <returns>The folded statements.</returns>
    let rec foldStatements (stmts: Stmt list) : Stmt list = List.map foldStmt stmts

    /// <summary>
    /// Fold a statement.
    /// </summary>
    /// <param name="stmt">The statement to fold.</param>
    /// <returns>The folded statement.</returns>
    and foldStmt (stmt: Stmt) : Stmt =
        match stmt with
        | SVariableDeclaration(tok, expr, typ) -> SVariableDeclaration(tok, foldExpr expr, typ)
        | SExpression(expr, typ) -> SExpression(foldExpr expr, typ)
        | SAssertStatement(expr, msg, typ) -> SAssertStatement(foldExpr expr, Option.map foldExpr msg, typ)
        | STypeDeclaration(tok, typ, typ2) -> STypeDeclaration(tok, typ, typ2)
        | SRecFunc(token, tuples, expr, typeOption) -> SRecFunc(token, tuples, foldExpr expr, typeOption)
        | SAsync(token, tuples, expr, typeOption) -> SAsync(token, tuples, foldExpr expr, typeOption)
        | SImport(token, path, typ) -> SImport(token, path, typ) // maybe fold in imports?

    /// <summary>
    /// Fold an expression.
    /// </summary>
    /// <param name="expr">The expression to fold.</param>
    /// <returns>The folded expression.</returns>
    and foldExpr (expr: Expr) : Expr =
        match expr with
        | ETail(expr, typ) -> ETail(foldExpr expr, typ)
        | ELiteral(lit, typ) ->
            match lit with
            // | LNumber(LRational(a, b)) -> ELiteral(LNumber(simplifyRational(LRational(a, b))), typ)
            | _ -> ELiteral(lit, typ)
        | EBlock(stmts, typ) -> EBlock(foldStatements stmts, typ)
        | EIdentifier(token, typ) -> EIdentifier(token, typ)
        | EGrouping(expr, typ) -> EGrouping(foldExpr expr, typ)
        | ECall(callee, args, typ) ->
            let callee = foldExpr callee
            let args = List.map foldExpr args
            let expr = ECall(callee, args, typ)

            match callee with
            | EIdentifier(tok, _) when builtins.ContainsKey(tok.Lexeme) ->
                let f = builtins[tok.Lexeme]
                f args expr
            | _ -> expr
        | EList(elems, typ) -> EList(List.map foldExpr elems, typ)
        | EIndex(expr, start, typ) ->
            let expr = foldExpr expr
            let start = foldExpr start
            EIndex(expr, start, typ)
        | EIndexRange(expr, start, end_, typ) ->
            let expr = foldExpr expr
            let start = foldExpr start
            let end_ = foldExpr end_
            EIndexRange(expr, start, end_, typ)

        | ELambda(args, body, rt, pr, typ, isAsync) -> ELambda(args, foldExpr body, rt, pr, typ, isAsync)
        | EIf(condEx, thenEx, elseEx, typ) ->
            let cond = foldExpr condEx

            match cond with
            | ELiteral(LBool true, _) -> foldExpr thenEx
            | ELiteral(LBool false, _) -> foldExpr elseEx
            | _ -> EIf(cond, foldExpr thenEx, foldExpr elseEx, typ)
        | ETernary(condEx, thenEx, elseEx, typ) -> foldExpr (EIf(condEx, thenEx, elseEx, typ))
        | ETuple(elems, typ) -> ETuple(List.map foldExpr elems, typ)
        | ERecordSelect(record, field, typ) -> ERecordSelect(foldExpr record, field, typ)
        | ERecordExtend((field, value, typ), record, _) ->
            ERecordExtend((field, foldExpr value, typ), foldExpr record, typ)
        | ERecordRestrict(record, field, typ) -> ERecordRestrict(foldExpr record, field, typ)
        | ERecordEmpty(typ) -> ERecordEmpty(typ)
        | ERange(start, stop, typ) ->
            let start = foldExpr start
            let stop = foldExpr stop

            match start, stop with
            | ELiteral(LNumber(LInteger s), _), ELiteral(LNumber(LInteger e), _) ->
                let range = [ for i in s..e -> ELiteral(LNumber(LInteger i), TInteger) ]
                EList(range, typ)
            | _ -> ERange(start, stop, typ)
        | ECodeBlock e -> ECodeBlock e
        | EMatch(expr, cases, typ) -> EMatch(foldExpr expr, cases, typ)

    in

    foldStatements program
