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
# 45 "microcparse.ml"
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
  271 (* NOT *);
  272 (* EQ *);
  273 (* NEQ *);
  274 (* LT *);
  275 (* LEQ *);
  276 (* GT *);
  277 (* GEQ *);
  278 (* AND *);
  279 (* OR *);
  280 (* RETURN *);
  281 (* IF *);
  282 (* ELSE *);
  283 (* FOR *);
  284 (* WHILE *);
  285 (* INT *);
  286 (* BOOL *);
  287 (* FLOAT *);
  288 (* VOID *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  289 (* LITERAL *);
  290 (* BLIT *);
  291 (* ID *);
  292 (* FLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\007\000\007\000\003\000\
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\000\000\002\000\003\000\
\000\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\005\000\000\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\002\000\002\000\
\003\000\004\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\056\000\000\000\010\000\011\000\012\000\013\000\
\001\000\003\000\004\000\000\000\000\000\016\000\000\000\000\000\
\000\000\000\000\008\000\000\000\000\000\014\000\000\000\000\000\
\009\000\015\000\000\000\000\000\000\000\000\000\017\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\030\000\
\000\000\029\000\018\000\000\000\000\000\000\000\045\000\046\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\000\
\048\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\051\000\
\021\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\034\000\035\000\036\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\050\000\000\000\000\000\000\000\025\000\000\000\000\000\
\000\000\023\000\000\000\000\000\024\000"

let yydgoto = "\002\000\
\003\000\004\000\010\000\011\000\012\000\017\000\024\000\028\000\
\018\000\043\000\044\000\050\000\079\000\080\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\247\254\017\255\000\000\132\255\007\255\
\046\255\050\255\000\000\056\255\132\255\000\000\028\255\132\255\
\000\000\000\000\029\255\043\255\072\255\145\255\000\000\000\000\
\145\255\145\255\145\255\078\255\081\255\087\255\000\000\000\000\
\110\255\000\000\000\000\204\255\124\000\057\255\000\000\000\000\
\183\000\097\255\145\255\145\255\145\255\145\255\145\255\000\000\
\000\000\000\000\145\255\145\255\145\255\145\255\145\255\145\255\
\145\255\145\255\145\255\145\255\145\255\145\255\145\255\000\000\
\000\000\000\000\145\000\098\255\166\000\183\000\103\255\089\255\
\183\000\077\255\077\255\000\000\000\000\000\000\231\000\231\000\
\094\255\094\255\094\255\094\255\216\000\200\000\141\255\145\255\
\141\255\000\000\145\255\083\255\227\255\000\000\183\000\141\255\
\145\255\000\000\115\255\141\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\126\255\000\000\
\000\000\129\255\000\000\000\000\000\000\000\000\000\000\092\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\134\255\000\000\000\000\000\000\000\000\000\000\
\181\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\112\255\000\000\000\000\134\255\000\000\133\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\255\000\000\135\255\
\009\255\250\255\018\000\000\000\000\000\000\000\103\000\107\000\
\041\000\049\000\072\000\080\000\005\255\051\255\000\000\000\000\
\000\000\000\000\000\000\106\255\000\000\000\000\040\255\000\000\
\143\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\113\000\000\000\029\000\000\000\000\000\117\000\
\000\000\168\255\226\255\206\255\000\000\000\000"

let yytablesize = 508
let yytable = "\045\000\
\009\000\076\000\047\000\048\000\049\000\043\000\100\000\043\000\
\102\000\049\000\043\000\049\000\001\000\054\000\049\000\106\000\
\054\000\014\000\015\000\109\000\075\000\049\000\077\000\078\000\
\081\000\013\000\043\000\043\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\019\000\055\000\016\000\030\000\055\000\031\000\032\000\
\020\000\023\000\033\000\044\000\027\000\044\000\107\000\021\000\
\044\000\034\000\030\000\022\000\031\000\073\000\025\000\029\000\
\033\000\101\000\035\000\036\000\103\000\037\000\038\000\034\000\
\014\000\044\000\049\000\039\000\040\000\041\000\042\000\051\000\
\035\000\036\000\052\000\037\000\038\000\061\000\062\000\063\000\
\053\000\039\000\040\000\041\000\042\000\017\000\099\000\017\000\
\017\000\074\000\096\000\017\000\059\000\060\000\061\000\062\000\
\063\000\098\000\017\000\022\000\104\000\022\000\022\000\054\000\
\027\000\022\000\027\000\017\000\017\000\108\000\017\000\017\000\
\022\000\055\000\056\000\057\000\017\000\017\000\017\000\017\000\
\006\000\022\000\022\000\007\000\022\000\022\000\026\000\052\000\
\026\000\053\000\022\000\022\000\022\000\022\000\030\000\000\000\
\031\000\026\000\030\000\046\000\033\000\000\000\000\000\000\000\
\033\000\000\000\000\000\034\000\000\000\000\000\000\000\034\000\
\005\000\006\000\007\000\008\000\035\000\036\000\000\000\037\000\
\038\000\000\000\000\000\000\000\000\000\039\000\040\000\041\000\
\042\000\039\000\040\000\041\000\042\000\031\000\000\000\031\000\
\000\000\000\000\031\000\031\000\031\000\031\000\031\000\031\000\
\000\000\000\000\000\000\000\000\031\000\031\000\031\000\031\000\
\031\000\031\000\031\000\031\000\058\000\000\000\000\000\000\000\
\000\000\000\000\059\000\060\000\061\000\062\000\063\000\000\000\
\000\000\000\000\000\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\105\000\000\000\000\000\000\000\000\000\
\000\000\059\000\060\000\061\000\062\000\063\000\000\000\000\000\
\000\000\000\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\071\000\032\000\000\000\032\000\000\000\000\000\032\000\
\032\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\000\000\033\000\000\000\033\000\000\000\000\000\033\000\
\033\000\033\000\000\000\000\000\000\000\005\000\006\000\007\000\
\008\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\039\000\000\000\039\000\000\000\000\000\039\000\000\000\
\000\000\040\000\000\000\040\000\000\000\000\000\040\000\000\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\041\000\000\000\041\000\000\000\000\000\041\000\000\000\000\000\
\042\000\000\000\042\000\000\000\000\000\042\000\000\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\037\000\
\000\000\037\000\000\000\038\000\037\000\038\000\000\000\000\000\
\038\000\000\000\000\000\000\000\000\000\000\000\037\000\037\000\
\000\000\000\000\038\000\038\000\037\000\037\000\072\000\000\000\
\038\000\038\000\059\000\060\000\061\000\062\000\063\000\000\000\
\000\000\000\000\000\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\095\000\000\000\000\000\000\000\059\000\
\060\000\061\000\062\000\063\000\000\000\000\000\000\000\000\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\097\000\000\000\000\000\000\000\059\000\060\000\061\000\062\000\
\063\000\000\000\000\000\000\000\000\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\059\000\060\000\061\000\
\062\000\063\000\000\000\000\000\000\000\000\000\064\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\059\000\060\000\
\061\000\062\000\063\000\000\000\000\000\000\000\000\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\059\000\060\000\
\061\000\062\000\063\000\000\000\000\000\000\000\000\000\064\000\
\065\000\066\000\067\000\068\000\069\000\059\000\060\000\061\000\
\062\000\063\000\000\000\000\000\000\000\000\000\000\000\000\000\
\066\000\067\000\068\000\069\000"

let yycheck = "\030\000\
\000\000\052\000\033\000\034\000\035\000\001\001\095\000\003\001\
\097\000\001\001\006\001\003\001\001\000\003\001\006\001\104\000\
\006\001\001\001\002\001\108\000\051\000\052\000\053\000\054\000\
\055\000\035\001\022\001\023\001\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\035\001\003\001\015\000\002\001\006\001\004\001\005\001\
\003\001\021\000\008\001\001\001\024\000\003\001\105\000\006\001\
\006\001\015\001\002\001\004\001\004\001\005\001\035\001\035\001\
\008\001\096\000\024\001\025\001\099\000\027\001\028\001\015\001\
\001\001\023\001\105\000\033\001\034\001\035\001\036\001\002\001\
\024\001\025\001\002\001\027\001\028\001\009\001\010\001\011\001\
\002\001\033\001\034\001\035\001\036\001\002\001\006\001\004\001\
\005\001\001\001\001\001\008\001\007\001\008\001\009\001\010\001\
\011\001\003\001\015\001\002\001\026\001\004\001\005\001\002\001\
\001\001\008\001\003\001\024\001\025\001\003\001\027\001\028\001\
\015\001\012\001\013\001\014\001\033\001\034\001\035\001\036\001\
\003\001\024\001\025\001\003\001\027\001\028\001\001\001\003\001\
\024\000\003\001\033\001\034\001\035\001\036\001\002\001\255\255\
\004\001\003\001\002\001\031\000\008\001\255\255\255\255\255\255\
\008\001\255\255\255\255\015\001\255\255\255\255\255\255\015\001\
\029\001\030\001\031\001\032\001\024\001\025\001\255\255\027\001\
\028\001\255\255\255\255\255\255\255\255\033\001\034\001\035\001\
\036\001\033\001\034\001\035\001\036\001\001\001\255\255\003\001\
\255\255\255\255\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\001\001\255\255\255\255\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\001\001\255\255\255\255\255\255\255\255\
\255\255\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\255\255\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\029\001\030\001\031\001\
\032\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\001\001\255\255\003\001\255\255\255\255\006\001\255\255\
\255\255\001\001\255\255\003\001\255\255\255\255\006\001\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\255\255\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\001\001\
\255\255\003\001\255\255\001\001\006\001\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\255\255\016\001\017\001\
\255\255\255\255\016\001\017\001\022\001\023\001\003\001\255\255\
\022\001\023\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\003\001\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\003\001\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\255\255\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\255\255\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\255\255\255\255\255\255\
\018\001\019\001\020\001\021\001"

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
# 33 "microcparse.mly"
            ( _1 )
# 346 "microcparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "microcparse.mly"
                 ( ([], [])               )
# 352 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 37 "microcparse.mly"
               ( ((_2 :: fst _1), snd _1) )
# 360 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 38 "microcparse.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 368 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 42 "microcparse.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 383 "microcparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "microcparse.mly"
                  ( [] )
# 389 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 50 "microcparse.mly"
                  ( _1 )
# 396 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "microcparse.mly"
                             ( [(_1,_2)]     )
# 404 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "microcparse.mly"
                             ( (_3,_4) :: _1 )
# 413 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "microcparse.mly"
          ( Int   )
# 419 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "microcparse.mly"
          ( Bool  )
# 425 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "microcparse.mly"
          ( Float )
# 431 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "microcparse.mly"
          ( Void  )
# 437 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "microcparse.mly"
                     ( [] )
# 443 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 64 "microcparse.mly"
                     ( _2 :: _1 )
# 451 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 67 "microcparse.mly"
               ( (_1, _2) )
# 459 "microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "microcparse.mly"
                   ( [] )
# 465 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 71 "microcparse.mly"
                   ( _2 :: _1 )
# 473 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "microcparse.mly"
                                            ( Expr _1               )
# 480 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 75 "microcparse.mly"
                                            ( Return _2             )
# 487 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 76 "microcparse.mly"
                                            ( Block(List.rev _2)    )
# 494 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "microcparse.mly"
                                            ( If(_3, _5, Block([])) )
# 502 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "microcparse.mly"
                                            ( If(_3, _5, _7)        )
# 511 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "microcparse.mly"
                                            ( For(_3, _5, _7, _9)   )
# 521 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "microcparse.mly"
                                            ( While(_3, _5)         )
# 529 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "microcparse.mly"
                  ( Noexpr )
# 535 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "microcparse.mly"
                  ( _1 )
# 542 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "microcparse.mly"
                     ( Literal(_1)            )
# 549 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "microcparse.mly"
              ( Fliteral(_1)           )
# 556 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 90 "microcparse.mly"
                     ( BoolLit(_1)            )
# 563 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "microcparse.mly"
                     ( Id(_1)                 )
# 570 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "microcparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 578 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "microcparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 586 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "microcparse.mly"
                     ( Binop(_1, Mult,  _3)   )
# 594 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "microcparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 602 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "microcparse.mly"
                     ( Binop(_1, Mod,   _3)   )
# 610 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "microcparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 618 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "microcparse.mly"
                     ( Binop(_1, Neq,   _3)   )
# 626 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "microcparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 634 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "microcparse.mly"
                     ( Binop(_1, Leq,   _3)   )
# 642 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "microcparse.mly"
                     ( Binop(_1, Greater, _3) )
# 650 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "microcparse.mly"
                     ( Binop(_1, Geq,   _3)   )
# 658 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "microcparse.mly"
                     ( Binop(_1, And,   _3)   )
# 666 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "microcparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 674 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "microcparse.mly"
                         ( Unop(Neg, _2)      )
# 681 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "microcparse.mly"
                     ( Unop(Not, _2)          )
# 688 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 107 "microcparse.mly"
                     (Assign("MARK", (Binop(Id(_1), Add, Literal(1)))) )
# 695 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 108 "microcparse.mly"
                     (Assign("MARK", (Binop(Id(_1), Sub, Literal(1)))) )
# 702 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "microcparse.mly"
                    ( Assign(_1, _3)         )
# 710 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 110 "microcparse.mly"
                              ( Call(_1, _3)  )
# 718 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 111 "microcparse.mly"
                       ( _2                   )
# 725 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "microcparse.mly"
                  ( [] )
# 731 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 116 "microcparse.mly"
               ( List.rev _1 )
# 738 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "microcparse.mly"
                            ( [_1] )
# 745 "microcparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "microcparse.mly"
                         ( _3 :: _1 )
# 753 "microcparse.ml"
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
