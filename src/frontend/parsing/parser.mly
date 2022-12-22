%{
  [@@@coverage exclude_file]
  open Ast.Ast_types
  open Parsed_ast
%}

/* Token definitions */

%token  <int> INT
%token  <string> ID
%token  LPAREN
%token  RPAREN 
%token  LBRACE 
%token  RBRACE 
%token  LANGLE
%token  RANGLE
%token  COMMA 
%token  DOT 
%token  COLON 
%token  SEMICOLON 
%token  EQUAL 
%token  PLUS
%token  MINUS
%token  MULT
%token  DIV
%token  MODULO
%token  AND
%token  OR
%token  EXCLAMATION_MARK
%token  COLONEQ
%token  LET 
%token  NEW 
%token  MUTABLE
%token  FUNCTION 
%token  CONSUME 
%token  FINISH 
%token  ASYNC 
%token  CLASS
%token  EXTENDS 
%token  GENERIC_TYPE
%token  TYPE_INT 
%token  TYPE_BOOL
%token  TYPE_VOID
%token  BORROWED
%token  REAL
%token  FAKE
%token  IF
%token  ELSE
%token  FOR
%token  WHILE
%token  MAIN
%token PRINTF
%token <string> STRING
%token EOF

/* 
 Menhir allows you to specify how to resolve shift-reduce conflicts when it sees a .
 There are three options:
  %left  we reduce
  %right we shift
  %nonassoc raise a syntax error 
 We list the operators in order of precedence - from low to high.
 e.g. * has higher precedence than +  so 1 + 2 * 3  = 1 + (2 * 3)
*/

%right  COLONEQ   EQUAL              
%left PLUS MINUS  LANGLE RANGLE
%left MULT DIV MODULO
%left AND OR  
%nonassoc EXCLAMATION_MARK


/* Specify starting production */
%start program 

/* Types for the result of productions */

%type <Parsed_ast.program> program

/* Class defn types */
%type <class_defn> class_defn
%type <generic_type> generic_type
%type <Class_name.t> inherits
%type <borrowed_ref> borrowed_ref
%type <modifier> modifier
%type <field_defn> field_defn
%type <param list> params
%type <param> param

%type <method_defn> method_defn
%type <function_defn> function_defn

%type <type_expr> parameterised_type
%type <type_expr> type_expr
%type <type_expr> let_type_annot

%type <block_expr> main_expr
%type <block_expr> block_expr
%type <expr list> args
%type <constructor_arg> constructor_arg
%type <identifier> identifier
%type <expr> expr
%type <async_expr> async_expr

%type <un_op> un_op
%type <bin_op> bin_op


%% /* Start grammar productions */


program: 
| class_defns=list(class_defn); function_defns=list(function_defn); main= main_expr;  EOF {Prog(class_defns, function_defns, main)}


/* Productions related to class definitions */

class_defn:
| CLASS ; name=ID; maybe_generic=option(generic_type); maybe_superclass=option(inherits); LBRACE; field_defns=nonempty_list(field_defn); method_defns=list(method_defn);  RBRACE 
{TClass(Class_name.of_string name, maybe_generic,maybe_superclass, field_defns, method_defns)}

generic_type:
| LANGLE GENERIC_TYPE RANGLE  { Generic }

inherits:
| EXTENDS; class_name=ID { Class_name.of_string class_name}

borrowed_ref:
| BORROWED {Borrowed}

/* Field definitions */

modifier:
| MUTABLE {Mutable}

field_defn:
| m=modifier; field_type=type_expr; field_name=ID; SEMICOLON {TField(m, field_type, Field_name.of_string field_name)}


/* Method and function definitions */

params:
| LPAREN; params=separated_list(COMMA,param); RPAREN {params}

param:
| maybeBorrowed=option(borrowed_ref); param_type=type_expr; param_name=ID;  {TParam(param_type, Var_name.of_string param_name, maybeBorrowed)}


method_defn: 
| maybeBorrowed=option(borrowed_ref); return_type=type_expr; method_name=ID; method_params=params; body=block_expr {TMethod( Method_name.of_string method_name, maybeBorrowed, return_type, method_params,body)}

function_defn: 
| FUNCTION; maybeBorrowed=option(borrowed_ref); return_type=type_expr; function_name=ID; function_params=params;  body=block_expr {TFunction(Function_name.of_string function_name, maybeBorrowed, return_type, function_params,body)}


/* Types */

parameterised_type:
| LANGLE type_param=type_expr RANGLE {type_param}

type_expr : 
| class_name=ID maybe_param_type=option(parameterised_type) {TEClass(Class_name.of_string class_name,maybe_param_type )}
| TYPE_INT  {TEInt} 
| TYPE_BOOL {TEBool}
| TYPE_VOID {TEVoid}
| GENERIC_TYPE {TEGeneric}

let_type_annot:
| COLON ; type_annot=type_expr {type_annot}

/* Expressions */

main_expr:
| TYPE_VOID; MAIN; LPAREN; RPAREN; exprs=block_expr {exprs}

block_expr:
| LBRACE; exprs=separated_list(SEMICOLON, expr); RBRACE {Block($startpos, exprs)}


/* Method / function arguments */
args:
| LPAREN; args=separated_list(COMMA, expr); RPAREN {args}

constructor_arg:
| field_name=ID; COLON; assigned_expr=expr {ConstructorArg(Field_name.of_string field_name, assigned_expr)}


identifier:
| variable=ID {Variable(Var_name.of_string variable)}
| obj=ID DOT field=ID {ObjField(Var_name.of_string obj, Field_name.of_string field)}


expr:
| LPAREN e=expr RPAREN {e}
| i=INT {Integer($startpos, i)}
| REAL { Boolean($startpos, true)}
| FAKE {  Boolean($startpos, false) }
| id=identifier { Identifier($startpos, id)}
| op=un_op e=expr {UnOp($startpos,op,e)}
| e1=expr op=bin_op e2=expr {BinOp($startpos, op, e1, e2)}
/*  Creating / reassigning \ deallocating references */
| NEW; class_name=ID; maybe_type_param=option(parameterised_type); LPAREN; constr_args=separated_list(COMMA, constructor_arg); RPAREN {Constructor($startpos, Class_name.of_string class_name, maybe_type_param, constr_args)}
| LET; var_name=ID; type_annot=option(let_type_annot);  EQUAL; bound_expr=expr  {Let($startpos, type_annot, Var_name.of_string var_name, bound_expr)} 
| id=identifier; COLONEQ; assigned_expr=expr {Assign($startpos, id, assigned_expr)}
| CONSUME; id=identifier {Consume($startpos, id)}
/* Function / Method Application */
| obj=ID; DOT; method_name=ID; method_args=args {MethodApp($startpos, Var_name.of_string obj, Method_name.of_string method_name, method_args)}
| fn=ID; fn_args=args { FunctionApp($startpos, Function_name.of_string fn, fn_args) } 
| PRINTF; LPAREN; format_str=STRING; option(COMMA); args=separated_list(COMMA, expr); RPAREN {Printf($startpos, format_str,args)}
/* Control flow */
| IF; cond_expr=expr; then_expr=block_expr; ELSE; else_expr=block_expr {If($startpos, cond_expr, then_expr, else_expr)}
| WHILE cond_expr=expr; loop_expr=block_expr {While($startpos, cond_expr, loop_expr)}
| FOR; LPAREN; init_expr=expr; SEMICOLON; cond_expr=expr; SEMICOLON; step_expr=expr; RPAREN; loop_expr=block_expr {For($startpos, init_expr, cond_expr, step_expr, loop_expr)}
/* Async expression */
| FINISH; LBRACE; forked_async_exprs=list(async_expr); curr_thread_expr=separated_list(SEMICOLON, expr) RBRACE {FinishAsync($startpos, forked_async_exprs, Block($startpos(curr_thread_expr), curr_thread_expr))}

async_expr:
| ASYNC exprs=block_expr {AsyncExpr exprs}


/* Operator expressions */

/* %inline expands occurrences of these 
    so rather than 
    unop e 
    we get two productions
     EXCLAMATION_MARK e 
     MINUS e 

*/
%inline un_op:
| EXCLAMATION_MARK {UnOpNot}
| MINUS {UnOpNeg}

%inline bin_op:
| PLUS { BinOpPlus }
| MINUS { BinOpMinus }
| MULT { BinOpMult }
| DIV { BinOpIntDiv } 
| MODULO { BinOpMod }
| LANGLE { BinOpLessThan }
| LANGLE EQUAL { BinOpLessThanEq }
| RANGLE { BinOpGreaterThan }
| RANGLE EQUAL{ BinOpGreaterThanEq }
| AND {BinOpAnd}
| OR {BinOpOr}
| EQUAL EQUAL {BinOpEq}
| EXCLAMATION_MARK EQUAL {BinOpNotEq}
