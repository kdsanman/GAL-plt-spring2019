type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | ASSIGN
  | INCR
  | DECR
  | STR_LIT of (string)
  | STR
  | DQUOT
  | LIST
  | LBRACK
  | RBRACK
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | VOID
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "microcparse.mly"
open Ast
# 51 "microcparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* MOD *);
  268 (* ASSIGN *);
  269 (* INCR *);
  270 (* DECR *);
  272 (* STR *);
  273 (* DQUOT *);
  274 (* LIST *);
  275 (* LBRACK *);
  276 (* RBRACK *);
  277 (* NOT *);
  278 (* EQ *);
  279 (* NEQ *);
  280 (* LT *);
  281 (* LEQ *);
  282 (* GT *);
  283 (* GEQ *);
  284 (* AND *);
  285 (* OR *);
  286 (* RETURN *);
  287 (* IF *);
  288 (* ELSE *);
  289 (* FOR *);
  290 (* WHILE *);
  291 (* INT *);
  292 (* BOOL *);
  293 (* FLOAT *);
  294 (* VOID *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  271 (* STR_LIT *);
  295 (* LITERAL *);
  296 (* BLIT *);
  297 (* ID *);
  298 (* FLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\005\000\007\000\
\007\000\003\000\008\000\008\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\012\000\012\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\013\000\
\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\004\000\000\000\
\002\000\003\000\000\000\002\000\002\000\003\000\003\000\005\000\
\007\000\009\000\005\000\000\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\002\000\002\000\003\000\004\000\003\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\060\000\000\000\013\000\000\000\010\000\011\000\
\012\000\014\000\001\000\003\000\004\000\000\000\000\000\000\000\
\000\000\018\000\000\000\015\000\000\000\000\000\000\000\008\000\
\000\000\000\000\016\000\000\000\000\000\009\000\017\000\000\000\
\000\000\000\000\000\000\019\000\005\000\000\000\034\000\000\000\
\000\000\000\000\000\000\000\000\000\000\030\000\032\000\000\000\
\031\000\020\000\000\000\000\000\000\000\049\000\000\000\000\000\
\000\000\050\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\051\000\052\000\021\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\055\000\023\000\035\000\000\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\038\000\039\000\040\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\054\000\000\000\000\000\027\000\
\000\000\000\000\025\000\000\000\000\000\026\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\014\000\022\000\029\000\033\000\
\023\000\050\000\051\000\060\000\056\000\057\000"

let yysindex = "\009\000\
\000\000\000\000\000\000\001\000\000\000\248\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\234\254\120\255\046\255\
\007\255\000\000\120\255\000\000\016\255\056\255\054\255\000\000\
\063\255\120\255\000\000\032\255\120\255\000\000\000\000\033\255\
\047\255\075\255\216\255\000\000\000\000\216\255\000\000\216\255\
\216\255\216\255\080\255\083\255\089\255\000\000\000\000\051\255\
\000\000\000\000\033\000\222\000\088\255\000\000\025\001\074\255\
\092\255\000\000\025\001\101\255\216\255\216\255\216\255\216\255\
\216\255\000\000\000\000\000\000\216\255\216\255\216\255\216\255\
\216\255\216\255\216\255\216\255\216\255\216\255\216\255\216\255\
\216\255\000\000\000\000\000\000\216\255\000\000\249\000\103\255\
\002\001\102\255\025\001\060\255\060\255\000\000\000\000\000\000\
\040\000\040\000\132\255\132\255\132\255\132\255\172\255\033\001\
\025\001\211\255\216\255\211\255\000\000\076\255\062\000\000\000\
\211\255\216\255\000\000\108\255\211\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\109\255\000\000\000\000\000\000\110\255\000\000\
\000\000\000\000\000\000\000\000\129\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\095\255\
\000\000\115\255\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\255\000\000\
\001\255\000\000\021\255\000\000\000\000\115\255\000\000\114\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\255\073\000\102\000\000\000\000\000\000\000\
\190\000\214\000\112\000\141\000\151\000\180\000\055\255\094\255\
\012\255\000\000\000\000\000\000\000\000\170\255\000\000\000\000\
\000\000\117\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\095\000\000\000\250\255\000\000\000\000\089\000\
\000\000\249\255\221\255\196\255\068\000\000\000"

let yytablesize = 573
let yytable = "\052\000\
\011\000\088\000\054\000\057\000\055\000\058\000\059\000\058\000\
\017\000\001\000\058\000\053\000\021\000\053\000\059\000\015\000\
\053\000\059\000\016\000\028\000\057\000\029\000\032\000\029\000\
\058\000\087\000\059\000\089\000\055\000\091\000\053\000\059\000\
\020\000\092\000\093\000\094\000\095\000\096\000\097\000\098\000\
\099\000\100\000\101\000\102\000\103\000\104\000\018\000\019\000\
\035\000\105\000\036\000\037\000\064\000\116\000\038\000\047\000\
\024\000\047\000\025\000\026\000\047\000\039\000\065\000\066\000\
\067\000\040\000\027\000\041\000\071\000\072\000\073\000\111\000\
\030\000\034\000\047\000\018\000\042\000\043\000\059\000\044\000\
\045\000\061\000\047\000\047\000\062\000\046\000\047\000\048\000\
\049\000\035\000\063\000\036\000\083\000\084\000\048\000\038\000\
\048\000\085\000\110\000\048\000\112\000\086\000\039\000\107\000\
\109\000\115\000\040\000\113\000\041\000\118\000\117\000\006\000\
\007\000\048\000\056\000\028\000\056\000\042\000\043\000\028\000\
\044\000\045\000\048\000\031\000\053\000\000\000\046\000\047\000\
\048\000\049\000\019\000\090\000\019\000\019\000\000\000\005\000\
\019\000\006\000\069\000\070\000\071\000\072\000\073\000\019\000\
\000\000\000\000\000\000\019\000\000\000\019\000\000\000\000\000\
\000\000\000\000\007\000\008\000\009\000\010\000\019\000\019\000\
\000\000\019\000\019\000\000\000\000\000\000\000\000\000\019\000\
\019\000\019\000\019\000\024\000\000\000\024\000\024\000\000\000\
\000\000\024\000\069\000\070\000\071\000\072\000\073\000\000\000\
\024\000\000\000\000\000\000\000\024\000\000\000\024\000\000\000\
\000\000\074\000\075\000\076\000\077\000\078\000\079\000\024\000\
\024\000\000\000\024\000\024\000\000\000\000\000\000\000\000\000\
\024\000\024\000\024\000\024\000\035\000\000\000\036\000\000\000\
\000\000\035\000\038\000\000\000\000\000\000\000\000\000\038\000\
\000\000\039\000\000\000\000\000\000\000\040\000\039\000\041\000\
\000\000\000\000\040\000\000\000\041\000\000\000\000\000\000\000\
\042\000\043\000\000\000\044\000\045\000\000\000\000\000\000\000\
\000\000\046\000\047\000\048\000\049\000\000\000\046\000\047\000\
\048\000\049\000\033\000\000\000\033\000\000\000\000\000\033\000\
\033\000\033\000\033\000\033\000\033\000\000\000\000\000\000\000\
\005\000\000\000\006\000\000\000\000\000\033\000\000\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
\000\000\068\000\000\000\007\000\008\000\009\000\010\000\069\000\
\070\000\071\000\072\000\073\000\000\000\000\000\069\000\070\000\
\071\000\072\000\073\000\000\000\000\000\000\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\114\000\076\000\
\077\000\078\000\079\000\000\000\069\000\070\000\071\000\072\000\
\073\000\036\000\000\000\036\000\000\000\000\000\036\000\036\000\
\036\000\000\000\000\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\000\000\036\000\000\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\037\000\000\000\
\037\000\000\000\000\000\037\000\037\000\037\000\000\000\000\000\
\043\000\000\000\043\000\000\000\000\000\043\000\000\000\000\000\
\000\000\037\000\000\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\043\000\000\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\044\000\000\000\044\000\
\000\000\000\000\044\000\000\000\000\000\000\000\000\000\045\000\
\000\000\045\000\000\000\000\000\045\000\000\000\000\000\000\000\
\044\000\000\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\045\000\000\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\046\000\000\000\046\000\000\000\
\000\000\046\000\000\000\000\000\000\000\000\000\041\000\000\000\
\041\000\000\000\000\000\041\000\000\000\000\000\000\000\046\000\
\000\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\041\000\000\000\041\000\041\000\000\000\042\000\000\000\
\042\000\041\000\041\000\042\000\000\000\000\000\000\000\000\000\
\082\000\000\000\000\000\000\000\069\000\070\000\071\000\072\000\
\073\000\042\000\000\000\042\000\042\000\000\000\000\000\000\000\
\000\000\042\000\042\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\106\000\000\000\000\000\000\000\069\000\
\070\000\071\000\072\000\073\000\108\000\000\000\000\000\000\000\
\069\000\070\000\071\000\072\000\073\000\000\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\000\000\074\000\
\075\000\076\000\077\000\078\000\079\000\080\000\081\000\069\000\
\070\000\071\000\072\000\073\000\000\000\000\000\000\000\069\000\
\070\000\071\000\072\000\073\000\000\000\000\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000"

let yycheck = "\035\000\
\000\000\062\000\038\000\003\001\040\000\041\000\042\000\003\001\
\015\000\001\000\006\001\001\001\019\000\003\001\003\001\024\001\
\006\001\006\001\041\001\026\000\020\001\001\001\029\000\003\001\
\020\001\061\000\062\000\063\000\064\000\065\000\020\001\020\001\
\026\001\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\001\001\002\001\
\002\001\085\000\004\001\005\001\002\001\114\000\008\001\001\001\
\041\001\003\001\003\001\006\001\006\001\015\001\012\001\013\001\
\014\001\019\001\004\001\021\001\009\001\010\001\011\001\107\000\
\041\001\041\001\020\001\001\001\030\001\031\001\114\000\033\001\
\034\001\002\001\028\001\029\001\002\001\039\001\040\001\041\001\
\042\001\002\001\002\001\004\001\005\001\020\001\001\001\008\001\
\003\001\006\001\106\000\006\001\108\000\001\001\015\001\001\001\
\003\001\113\000\019\001\032\001\021\001\117\000\003\001\003\001\
\003\001\020\001\020\001\001\001\003\001\030\001\031\001\003\001\
\033\001\034\001\029\001\029\000\036\000\255\255\039\001\040\001\
\041\001\042\001\002\001\064\000\004\001\005\001\255\255\016\001\
\008\001\018\001\007\001\008\001\009\001\010\001\011\001\015\001\
\255\255\255\255\255\255\019\001\255\255\021\001\255\255\255\255\
\255\255\255\255\035\001\036\001\037\001\038\001\030\001\031\001\
\255\255\033\001\034\001\255\255\255\255\255\255\255\255\039\001\
\040\001\041\001\042\001\002\001\255\255\004\001\005\001\255\255\
\255\255\008\001\007\001\008\001\009\001\010\001\011\001\255\255\
\015\001\255\255\255\255\255\255\019\001\255\255\021\001\255\255\
\255\255\022\001\023\001\024\001\025\001\026\001\027\001\030\001\
\031\001\255\255\033\001\034\001\255\255\255\255\255\255\255\255\
\039\001\040\001\041\001\042\001\002\001\255\255\004\001\255\255\
\255\255\002\001\008\001\255\255\255\255\255\255\255\255\008\001\
\255\255\015\001\255\255\255\255\255\255\019\001\015\001\021\001\
\255\255\255\255\019\001\255\255\021\001\255\255\255\255\255\255\
\030\001\031\001\255\255\033\001\034\001\255\255\255\255\255\255\
\255\255\039\001\040\001\041\001\042\001\255\255\039\001\040\001\
\041\001\042\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\255\255\
\016\001\255\255\018\001\255\255\255\255\020\001\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\255\255\
\255\255\001\001\255\255\035\001\036\001\037\001\038\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\001\001\024\001\
\025\001\026\001\027\001\255\255\007\001\008\001\009\001\010\001\
\011\001\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\008\001\255\255\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\255\255\020\001\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\001\001\255\255\
\003\001\255\255\255\255\006\001\007\001\008\001\255\255\255\255\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\255\255\
\255\255\020\001\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\020\001\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\001\001\
\255\255\003\001\255\255\255\255\006\001\255\255\255\255\255\255\
\020\001\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\020\001\255\255\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\020\001\
\255\255\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\020\001\255\255\022\001\023\001\255\255\001\001\255\255\
\003\001\028\001\029\001\006\001\255\255\255\255\255\255\255\255\
\003\001\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\011\001\020\001\255\255\022\001\023\001\255\255\255\255\255\255\
\255\255\028\001\029\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\003\001\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\003\001\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\011\001\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  ASSIGN\000\
  INCR\000\
  DECR\000\
  STR\000\
  DQUOT\000\
  LIST\000\
  LBRACK\000\
  RBRACK\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  VOID\000\
  EOF\000\
  "

let yynames_block = "\
  STR_LIT\000\
  LITERAL\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 37 "microcparse.mly"
            ( _1 )
# 385 "microcparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "microcparse.mly"
                 ( ([], [])               )
# 391 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 41 "microcparse.mly"
               ( ((_2 :: fst _1), snd _1) )
# 399 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 42 "microcparse.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 407 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 46 "microcparse.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 422 "microcparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "microcparse.mly"
                  ( [] )
# 428 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 54 "microcparse.mly"
                  ( _1 )
# 435 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "microcparse.mly"
                             ( [(_1,_2)]     )
# 443 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "microcparse.mly"
                             ( (_3,_4) :: _1 )
# 452 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "microcparse.mly"
          ( Int   )
# 458 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "microcparse.mly"
          ( Bool  )
# 464 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "microcparse.mly"
          ( Float )
# 470 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "microcparse.mly"
          ( Str   )
# 476 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "microcparse.mly"
          ( Void  )
# 482 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 66 "microcparse.mly"
                    ( List(_3) )
# 489 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "microcparse.mly"
                     ( [(Int, "__i");
                        (List(Int), "__intlist");
                        (List(Str),"__strlist");
                        (Int, "__l")] )
# 498 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 73 "microcparse.mly"
                     ( _2 :: _1 )
# 506 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 76 "microcparse.mly"
               ( (_1, _2) )
# 514 "microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "microcparse.mly"
                   ( [] )
# 520 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "microcparse.mly"
                   ( _2 :: _1 )
# 528 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 83 "microcparse.mly"
                                            ( Expr _1               )
# 535 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 84 "microcparse.mly"
                                            ( Return _2             )
# 542 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 85 "microcparse.mly"
                                            ( Block(List.rev _2)    )
# 549 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 86 "microcparse.mly"
                                            ( If(_3, _5, Block([])) )
# 557 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 87 "microcparse.mly"
                                            ( If(_3, _5, _7)        )
# 566 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 89 "microcparse.mly"
                                            ( For(_3, _5, _7, _9)   )
# 576 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 90 "microcparse.mly"
                                            ( While(_3, _5)         )
# 584 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "microcparse.mly"
                  ( Noexpr )
# 590 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "microcparse.mly"
                  ( _1 )
# 597 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 97 "microcparse.mly"
                                ( Literal(_1)            )
# 604 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "microcparse.mly"
                               ( Fliteral(_1)           )
# 611 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 99 "microcparse.mly"
                                ( BoolLit(_1)            )
# 618 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "microcparse.mly"
                                ( Id(_1)                 )
# 625 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "microcparse.mly"
                                ( StrLit(_1)             )
# 632 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 102 "microcparse.mly"
                                ( ListLit(_2)        )
# 639 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "microcparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 647 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "microcparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 655 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "microcparse.mly"
                     ( Binop(_1, Mult,  _3)   )
# 663 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "microcparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 671 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "microcparse.mly"
                     ( Binop(_1, Mod,   _3)   )
# 679 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "microcparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 687 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "microcparse.mly"
                     ( Binop(_1, Neq,   _3)   )
# 695 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "microcparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 703 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "microcparse.mly"
                     ( Binop(_1, Leq,   _3)   )
# 711 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "microcparse.mly"
                     ( Binop(_1, Greater, _3) )
# 719 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "microcparse.mly"
                     ( Binop(_1, Geq,   _3)   )
# 727 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "microcparse.mly"
                     ( Binop(_1, And,   _3)   )
# 735 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "microcparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 743 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "microcparse.mly"
                         ( Unop(Neg, _2)      )
# 750 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "microcparse.mly"
                     ( Unop(Not, _2)          )
# 757 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 119 "microcparse.mly"
                     (Assign("K", (Binop(Id(_1), Add, Literal(1)))) )
# 764 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 120 "microcparse.mly"
                     (Assign("K", (Binop(Id(_1), Sub, Literal(1)))) )
# 771 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "microcparse.mly"
                      ( Assign(_1, _3)         )
# 779 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 122 "microcparse.mly"
                              ( Call(_1, _3)  )
# 787 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 123 "microcparse.mly"
                       ( _2                   )
# 794 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "microcparse.mly"
                  ( [] )
# 800 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 128 "microcparse.mly"
               ( List.rev _1 )
# 807 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "microcparse.mly"
                            ( [_1] )
# 814 "microcparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "microcparse.mly"
                         ( _3 :: _1 )
# 822 "microcparse.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
