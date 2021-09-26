open GT       
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let eval_instruction (stack, (state, io, out)) insn =
    match stack, io, insn with
    | [],         _, BINOP op -> failwith ("Empty stack for operation " ^ op)
    | x::[],      _, BINOP op -> failwith ("Only one of two values in stack for operation " ^ op)
    | y::x::tail, _, BINOP op -> ((Syntax.Expr.op_to_func op x y)::tail, (state, io, out))
    | _,          _, CONST c  -> (c::stack, (state, io, out))
    | _,         [], READ     -> failwith ("Read from empty input")
    | _,    z::tail, READ     -> (z::stack, (state, tail, out))
    | [],         _, WRITE    -> failwith ("Write from empty stack")
    | z::tail,    _, WRITE    -> (tail, (state, io, out @ [z]))
    | _,          _, LD var   -> ((state var)::stack, (state, io, out))
    | [],         _, ST var   -> failwith ("Store to variable" ^ var ^ " from empty stack")
    | z::tail,    _, ST var   -> (tail, ((Syntax.Expr.update var z state), io, out)) 

                   
let eval config program = List.fold_left eval_instruction config program
(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile_expr expr = 
  match expr with
  | Syntax.Expr.Const const             -> [CONST const]
  | Syntax.Expr.Var   var               -> [LD var]
  | Syntax.Expr.Binop (op, left, right) -> (compile_expr left) @ (compile_expr right) @ [BINOP op]

let rec compile stmt =
  match stmt with
  | Syntax.Stmt.Read   var_name         -> READ :: [ST var_name]
  | Syntax.Stmt.Write  expr             -> (compile_expr expr) @ [WRITE]
  | Syntax.Stmt.Assign (var_name, expr) -> (compile_expr expr) @ [ST var_name]
  | Syntax.Stmt.Seq    (stmt1, stmt2)   -> (compile stmt1) @ (compile stmt2)
