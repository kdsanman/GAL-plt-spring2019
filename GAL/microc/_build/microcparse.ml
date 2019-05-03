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
# 43 "microcparse.ml"
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
  269 (* NOT *);
  270 (* EQ *);
  271 (* NEQ *);
  272 (* LT *);
  273 (* LEQ *);
  274 (* GT *);
  275 (* GEQ *);
  276 (* AND *);
  277 (* OR *);
  278 (* RETURN *);
  279 (* IF *);
  280 (* ELSE *);
  281 (* FOR *);
  282 (* WHILE *);
  283 (* INT *);
  284 (* BOOL *);
  285 (* FLOAT *);
  286 (* VOID *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  287 (* LITERAL *);
  288 (* BLIT *);
  289 (* ID *);
  290 (* FLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\007\000\007\000\003\000\
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\000\000\002\000\003\000\
\000\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\005\000\000\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\003\000\004\000\
\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\010\000\011\000\012\000\013\000\
\001\000\003\000\004\000\000\000\000\000\016\000\000\000\000\000\
\000\000\000\000\008\000\000\000\000\000\014\000\000\000\000\000\
\009\000\015\000\000\000\000\000\000\000\000\000\017\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\030\000\
\000\000\029\000\018\000\000\000\000\000\000\000\045\000\046\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\021\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\034\000\035\000\036\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\048\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\023\000\
\000\000\000\000\024\000"

let yydgoto = "\002\000\
\003\000\004\000\010\000\011\000\012\000\017\000\024\000\028\000\
\018\000\043\000\044\000\050\000\077\000\078\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\255\017\255\000\000\063\255\030\255\
\067\255\065\255\000\000\068\255\063\255\000\000\049\255\063\255\
\000\000\000\000\050\255\043\255\085\255\163\255\000\000\000\000\
\163\255\163\255\163\255\093\255\101\255\102\255\000\000\000\000\
\038\255\000\000\000\000\218\255\151\000\076\255\000\000\000\000\
\204\000\086\255\163\255\163\255\163\255\163\255\163\255\000\000\
\163\255\163\255\163\255\163\255\163\255\163\255\163\255\163\255\
\163\255\163\255\163\255\163\255\163\255\000\000\000\000\000\000\
\170\000\105\255\189\000\204\000\108\255\106\255\204\000\048\255\
\048\255\000\000\000\000\000\000\246\000\246\000\182\255\182\255\
\182\255\182\255\233\000\219\000\141\255\163\255\141\255\000\000\
\163\255\089\255\239\255\000\000\204\000\141\255\163\255\000\000\
\113\255\141\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\116\255\000\000\
\000\000\117\255\000\000\000\000\000\000\000\000\000\000\092\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\120\255\000\000\000\000\000\000\000\000\000\000\
\197\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\255\000\000\000\000\120\255\000\000\119\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\011\255\000\000\128\255\009\255\004\000\
\031\000\000\000\000\000\000\000\040\255\136\000\052\000\073\000\
\094\000\115\000\181\255\005\255\000\000\000\000\000\000\000\000\
\000\000\125\255\000\000\000\000\046\255\000\000\129\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\104\000\000\000\064\000\000\000\000\000\103\000\
\000\000\170\255\226\255\206\255\000\000\000\000"

let yytablesize = 521
let yytable = "\045\000\
\009\000\074\000\047\000\048\000\049\000\044\000\098\000\044\000\
\100\000\047\000\044\000\047\000\001\000\052\000\047\000\104\000\
\052\000\014\000\015\000\107\000\073\000\049\000\075\000\076\000\
\079\000\044\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\091\000\092\000\054\000\
\037\000\027\000\037\000\027\000\030\000\037\000\031\000\032\000\
\053\000\055\000\033\000\053\000\105\000\037\000\037\000\034\000\
\059\000\060\000\061\000\037\000\037\000\013\000\019\000\099\000\
\035\000\036\000\101\000\037\000\038\000\020\000\021\000\022\000\
\049\000\039\000\040\000\041\000\042\000\030\000\016\000\031\000\
\071\000\025\000\029\000\033\000\023\000\014\000\072\000\027\000\
\034\000\005\000\006\000\007\000\008\000\017\000\051\000\017\000\
\017\000\035\000\036\000\017\000\037\000\038\000\052\000\053\000\
\017\000\094\000\039\000\040\000\041\000\042\000\096\000\097\000\
\102\000\017\000\017\000\106\000\017\000\017\000\006\000\007\000\
\026\000\050\000\017\000\017\000\017\000\017\000\022\000\026\000\
\022\000\022\000\051\000\026\000\022\000\046\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\030\000\000\000\
\031\000\000\000\022\000\022\000\033\000\022\000\022\000\000\000\
\000\000\034\000\000\000\022\000\022\000\022\000\022\000\000\000\
\000\000\000\000\035\000\036\000\030\000\037\000\038\000\000\000\
\000\000\000\000\033\000\039\000\040\000\041\000\042\000\034\000\
\000\000\000\000\000\000\000\000\000\000\043\000\000\000\043\000\
\000\000\000\000\043\000\000\000\057\000\058\000\059\000\060\000\
\061\000\039\000\040\000\041\000\042\000\031\000\000\000\031\000\
\043\000\043\000\031\000\031\000\031\000\031\000\031\000\031\000\
\000\000\000\000\031\000\031\000\031\000\031\000\031\000\031\000\
\031\000\031\000\056\000\000\000\000\000\000\000\000\000\000\000\
\057\000\058\000\059\000\060\000\061\000\000\000\000\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\103\000\
\000\000\000\000\000\000\000\000\000\000\057\000\058\000\059\000\
\060\000\061\000\000\000\000\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\032\000\000\000\032\000\000\000\
\000\000\032\000\032\000\032\000\000\000\000\000\000\000\000\000\
\000\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\000\000\000\000\005\000\006\000\007\000\008\000\033\000\
\000\000\033\000\000\000\000\000\033\000\033\000\033\000\000\000\
\000\000\000\000\000\000\000\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\039\000\000\000\039\000\000\000\
\000\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\040\000\000\000\040\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\041\000\000\000\
\041\000\000\000\000\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\042\000\000\000\042\000\000\000\000\000\
\042\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\038\000\000\000\038\000\000\000\000\000\038\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\038\000\038\000\000\000\
\000\000\070\000\000\000\038\000\038\000\057\000\058\000\059\000\
\060\000\061\000\000\000\000\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\093\000\000\000\000\000\000\000\
\057\000\058\000\059\000\060\000\061\000\000\000\000\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\095\000\
\000\000\000\000\000\000\057\000\058\000\059\000\060\000\061\000\
\000\000\000\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\057\000\058\000\059\000\060\000\061\000\000\000\
\000\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\057\000\058\000\059\000\060\000\061\000\000\000\000\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\057\000\
\058\000\059\000\060\000\061\000\000\000\000\000\062\000\063\000\
\064\000\065\000\066\000\067\000\057\000\058\000\059\000\060\000\
\061\000\000\000\000\000\000\000\000\000\064\000\065\000\066\000\
\067\000"

let yycheck = "\030\000\
\000\000\052\000\033\000\034\000\035\000\001\001\093\000\003\001\
\095\000\001\001\006\001\003\001\001\000\003\001\006\001\102\000\
\006\001\001\001\002\001\106\000\051\000\052\000\053\000\054\000\
\055\000\021\001\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\002\001\
\001\001\001\001\003\001\003\001\002\001\006\001\004\001\005\001\
\003\001\012\001\008\001\006\001\103\000\014\001\015\001\013\001\
\009\001\010\001\011\001\020\001\021\001\033\001\033\001\094\000\
\022\001\023\001\097\000\025\001\026\001\003\001\006\001\004\001\
\103\000\031\001\032\001\033\001\034\001\002\001\015\000\004\001\
\005\001\033\001\033\001\008\001\021\000\001\001\001\001\024\000\
\013\001\027\001\028\001\029\001\030\001\002\001\002\001\004\001\
\005\001\022\001\023\001\008\001\025\001\026\001\002\001\002\001\
\013\001\001\001\031\001\032\001\033\001\034\001\003\001\006\001\
\024\001\022\001\023\001\003\001\025\001\026\001\003\001\003\001\
\001\001\003\001\031\001\032\001\033\001\034\001\002\001\024\000\
\004\001\005\001\003\001\003\001\008\001\031\000\255\255\255\255\
\255\255\013\001\255\255\255\255\255\255\255\255\002\001\255\255\
\004\001\255\255\022\001\023\001\008\001\025\001\026\001\255\255\
\255\255\013\001\255\255\031\001\032\001\033\001\034\001\255\255\
\255\255\255\255\022\001\023\001\002\001\025\001\026\001\255\255\
\255\255\255\255\008\001\031\001\032\001\033\001\034\001\013\001\
\255\255\255\255\255\255\255\255\255\255\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\007\001\008\001\009\001\010\001\
\011\001\031\001\032\001\033\001\034\001\001\001\255\255\003\001\
\020\001\021\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\001\001\255\255\255\255\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\001\001\
\255\255\255\255\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\001\001\255\255\003\001\255\255\
\255\255\006\001\007\001\008\001\255\255\255\255\255\255\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\255\255\255\255\027\001\028\001\029\001\030\001\001\001\
\255\255\003\001\255\255\255\255\006\001\007\001\008\001\255\255\
\255\255\255\255\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\001\001\255\255\003\001\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\014\001\015\001\255\255\
\255\255\003\001\255\255\020\001\021\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\003\001\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\255\255\255\255\016\001\017\001\018\001\
\019\001"

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
# 32 "microcparse.mly"
            ( _1 )
# 344 "microcparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "microcparse.mly"
                 ( ([], [])               )
# 350 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "microcparse.mly"
               ( ((_2 :: fst _1), snd _1) )
# 358 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "microcparse.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 366 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 41 "microcparse.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 381 "microcparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "microcparse.mly"
                  ( [] )
# 387 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 49 "microcparse.mly"
                  ( _1 )
# 394 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "microcparse.mly"
                             ( [(_1,_2)]     )
# 402 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "microcparse.mly"
                             ( (_3,_4) :: _1 )
# 411 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "microcparse.mly"
          ( Int   )
# 417 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "microcparse.mly"
          ( Bool  )
# 423 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "microcparse.mly"
          ( Float )
# 429 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "microcparse.mly"
          ( Void  )
# 435 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "microcparse.mly"
                     ( [] )
# 441 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 63 "microcparse.mly"
                     ( _2 :: _1 )
# 449 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 66 "microcparse.mly"
               ( (_1, _2) )
# 457 "microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "microcparse.mly"
                   ( [] )
# 463 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "microcparse.mly"
                   ( _2 :: _1 )
# 471 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "microcparse.mly"
                                            ( Expr _1               )
# 478 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 74 "microcparse.mly"
                                            ( Return _2             )
# 485 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 75 "microcparse.mly"
                                            ( Block(List.rev _2)    )
# 492 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 76 "microcparse.mly"
                                            ( If(_3, _5, Block([])) )
# 500 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "microcparse.mly"
                                            ( If(_3, _5, _7)        )
# 509 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "microcparse.mly"
                                            ( For(_3, _5, _7, _9)   )
# 519 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "microcparse.mly"
                                            ( While(_3, _5)         )
# 527 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "microcparse.mly"
                  ( Noexpr )
# 533 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "microcparse.mly"
                  ( _1 )
# 540 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 87 "microcparse.mly"
                     ( Literal(_1)            )
# 547 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "microcparse.mly"
              ( Fliteral(_1)           )
# 554 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 89 "microcparse.mly"
                     ( BoolLit(_1)            )
# 561 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "microcparse.mly"
                     ( Id(_1)                 )
# 568 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "microcparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 576 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "microcparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 584 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "microcparse.mly"
                     ( Binop(_1, Mult,  _3)   )
# 592 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "microcparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 600 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "microcparse.mly"
                     ( Binop(_1, Mod,   _3)   )
# 608 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "microcparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 616 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "microcparse.mly"
                     ( Binop(_1, Neq,   _3)   )
# 624 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "microcparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 632 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "microcparse.mly"
                     ( Binop(_1, Leq,   _3)   )
# 640 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "microcparse.mly"
                     ( Binop(_1, Greater, _3) )
# 648 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "microcparse.mly"
                     ( Binop(_1, Geq,   _3)   )
# 656 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "microcparse.mly"
                     ( Binop(_1, And,   _3)   )
# 664 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "microcparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 672 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "microcparse.mly"
                         ( Unop(Neg, _2)      )
# 679 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "microcparse.mly"
                     ( Unop(Not, _2)          )
# 686 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "microcparse.mly"
                     ( Assign(_1, _3)         )
# 694 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 107 "microcparse.mly"
                              ( Call(_1, _3)  )
# 702 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "microcparse.mly"
                       ( _2                   )
# 709 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "microcparse.mly"
                  ( [] )
# 715 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 112 "microcparse.mly"
               ( List.rev _1 )
# 722 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "microcparse.mly"
                            ( [_1] )
# 729 "microcparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "microcparse.mly"
                         ( _3 :: _1 )
# 737 "microcparse.ml"
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
