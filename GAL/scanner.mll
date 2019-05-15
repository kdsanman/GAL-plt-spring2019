(* Ocamllex scanner for GAL *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "#"      { comment lexbuf }           (* Comment *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['	   { LBRACK }
| ']'	   { RBRACK }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| "++"     { INCR }
| "--"     { DECR }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| '"'      { DQUOT }
| "String" { STR }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "list"   { LIST }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "def"    { DEF }
| "node"   { NODE }
| "float"  { FLOAT }
| "void"   { VOID }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }

| ".set_data" { NODE_SET_DATA }

| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| '\"' ([^'\"']* as lxm) '\"' { STR_LIT(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| "#"  { token lexbuf }
| _    { comment lexbuf }
