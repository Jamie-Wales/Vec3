module Vec3.Interpreter.Parser2

open Token
open Grammar

let parser (tList: Token list) : Expr =
    let rec E (tList: Token list) : Expr * Token list=
        let lhs, tail = T tList
        Eopt lhs tail
    
    and Eopt (lhs: Expr) (tList: Token list) : Expr * Token list =
        match tList with
        | { Lexeme = Operator Plus } as token :: tail ->
            let rhs, tail = T tail
            Eopt (EBinary(lhs, token, rhs, TInfer)) tail
        | { Lexeme = Operator Minus } as token :: tail ->
            let rhs, tail = T tail
            Eopt (EBinary(lhs, token, rhs, TInfer)) tail
        | _ -> (lhs, tList)
    
    and T (tList: Token list) : Expr * Token list =
        let lhs, tail = P tList
        Topt lhs tail
    
    and Topt (lhs: Expr) (tList: Token list) : Expr * Token list =
        match tList with
        | { Lexeme = Operator Star } as token :: tail ->
            let rhs, tail = P tail
            Topt (EBinary(lhs, token, rhs, TInfer)) tail
        | { Lexeme = Operator Slash } as token :: tail ->
            let rhs, tail = P tail
            Topt (EBinary(lhs, token, rhs, TInfer)) tail
        | { Lexeme = Operator Percent } as token :: tail ->
            let rhs, tail = P tail
            Topt (EBinary(lhs, token, rhs, TInfer)) tail
        | _ -> (lhs, tList)
        
    and P (tList: Token list) : Expr * Token list =
        let lhs, tail = NR tList
        Popt lhs tail
        
    and Popt (lhs: Expr) (tList: Token list) : Expr * Token list =
        match tList with
        | { Lexeme = Operator Caret } as token::tail ->
            let rhs, tail = NR tail
            Popt (EBinary(lhs, token, rhs, TInfer)) tail
        | _ ->
            let lhs, tail = CallOpt lhs tList
            lhs, tail
        
    and U (tList: Token list) : Expr * Token list =
        match tList with
        | { Lexeme = Operator Minus } as token :: tail ->
            let rhs, tail = U tail
            EUnary(token, rhs, TInfer), tail
        | _ -> NR tList
        
    and NR (tList: Token list) : Expr * Token list =
        match tList with
        | { Lexeme = Identifier _ } as token :: tail ->
            EIdentifier(token, TInfer), tail
        | { Lexeme = Lexeme.Number lit } :: tail ->
            match lit with
            | Integer i -> (ELiteral(LNumber(LInteger i), TInfer), tail)
            | Float f -> (ELiteral(LNumber(LFloat f), TInfer), tail)
            | Rational (n, d) -> (ELiteral(LNumber(LRational(n, d)), TInfer), tail)
            | Complex (r, i) -> (ELiteral(LNumber(LComplex(r, i)), TInfer), tail)
        | { Lexeme = Lexeme.String lit } :: tail ->
            ELiteral(LString lit, TInfer), tail
        | { Lexeme = Operator LeftParen } :: tail ->
            let expr, tail = E tail
            match tail with
            | { Lexeme = Operator RightParen } :: tail -> (expr, tail)
            | _ -> failwith "Expected ')'"
        | _ -> failwith "Expected expression"
    
    and CallOpt (lhs: Expr) (tList: Token list) : Expr * Token list =
        match tList with
        | { Lexeme = Operator LeftParen } :: tail ->
            let args, tail = Args tail
            (ECall(lhs, args, TInfer), tail)
        | _ -> (lhs, tList)
    
    and Args (tList: Token list) : Expr list * Token list =
        match tList with
        | { Lexeme = Operator RightParen } :: tail -> ([], tail)
        | _ -> 
            let arg, tail = E tList
            match tail with
            | { Lexeme = Comma } :: tail ->
                let args, tail = Args tail
                (arg :: args, tail)
            | { Lexeme = Operator RightParen } :: tail -> ([arg], tail)
            | _ -> failwith "Expected ',' or ')'"
       
    fst <| E tList
            
        
            
            
        