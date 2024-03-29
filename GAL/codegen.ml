(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Ast
open Sast 

module StringMap = Map.Make(String)

let get_type(t, _) = t
let first_element (myList) = match myList with
 [] -> Void
| first_e1 :: _ -> get_type(first_e1)

let check_list_type m =
  let (t, _) = m in 
  match t with
   List(ty) -> ty
  |_ -> raise (Failure ("List must be of type list in ListLit (CODEGEN): " ^ string_of_typ t))

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  let llmem_graph = L.MemoryBuffer.of_file "GAL.bc" in
  let llm_graph = Llvm_bitreader.parse_bitcode context llmem_graph in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "GAL" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and str_t      = L.pointer_type (L.i8_type context) 
  and void_t     = L.void_type   context 
  and void_ptr_t = L.pointer_type (L.i8_type context)
  and lst_t      = L.pointer_type (match L.type_by_name llm_graph "struct.list" with
      None -> raise (Failure "Missing implementation for struct list")
    | Some t -> t)
  and node_t      = L.pointer_type (match L.type_by_name llm_graph "struct.node" with
      None -> raise (Failure "Missing implementation for struct node")
    | Some t -> t)
  in

  (* Return the LLVM type for a GAL type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Str   -> str_t
    | A.Void  -> void_t
    | A.List _ -> lst_t
    | A.Node -> node_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

   let printil_t : L.lltype = 
      L.function_type lst_t [| lst_t |] in
  let printil_func : L.llvalue = 
      L.declare_function "printil" printil_t the_module in

  let printl_t : L.lltype = 
      L.function_type lst_t [| lst_t |] in
  let printl_func : L.llvalue = 
      L.declare_function "printl" printl_t the_module in

  let string_concat_t : L.lltype =
    L.function_type str_t [| str_t; str_t |] in
  let string_concat_f : L.llvalue =
    L.declare_function "string_concat" string_concat_t the_module in

  let string_length_t = 
          L.function_type i32_t [| str_t |] in
  let string_length_f = 
          L.declare_function "str_size" string_length_t the_module in

  (* Functions for Lists. It is generic *)
  let make_list_t = L.function_type lst_t [||] in
  let make_list_func = L.declare_function "make_list" make_list_t the_module in

  let list_add_tail_t = L.function_type i32_t [| lst_t; void_ptr_t |] in
  let list_add_tail_func = L.declare_function "add_tail" list_add_tail_t the_module in

  let list_get_t = L.function_type void_ptr_t [| lst_t; i32_t |] in
  let list_get_func = L.declare_function "list_get" list_get_t the_module in

  let list_set_t = L.function_type i32_t [| lst_t; i32_t; i32_t |] in
  let list_set_func = L.declare_function "list_set_int" list_set_t the_module in 


  let node_set_t = L.function_type i32_t [| node_t; i32_t |] in
  let node_set_func = L.declare_function "node_set_int" node_set_t the_module in 
 
  let list_length_t = 
          L.function_type i32_t [| lst_t |] in
  let list_length_f =
          L.declare_function "list_len" list_length_t the_module in

  let listSort_t = L.function_type void_ptr_t [| lst_t |] in
  let listSort_func = L.declare_function "listSort" listSort_t the_module in

  let list_concat_t = L.function_type lst_t [| lst_t; lst_t |] in
  let list_concat_func = L.declare_function "list_concat" list_concat_t the_module in

  (* functions for Nodes. Generic *)
  let make_node_t = L.function_type node_t [||] in
  let make_node_func = L.declare_function "make_node" make_node_t the_module in

  let node_get_data_t = L.function_type void_ptr_t [| node_t; i32_t |] in
  let node_get_data_func = L.declare_function "node_get" node_get_data_t the_module in

  let node_add_tail_t = L.function_type i32_t [| node_t; void_ptr_t |] in
  let node_add_tail_func = L.declare_function "node_add_tail" node_add_tail_t the_module in

  (* Casting functions *)

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((styp, e) : sexpr) = match e with
	     SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SStrLit s   -> L.build_global_stringptr s "string" builder
      | SFliteral l -> L.const_float_of_string float_t l
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SNodeLit l ->  let rec list_fill lst = (function
          [] -> lst
          | sx :: rest ->
          let (t, _) = sx in 
          let data = (match t with
              A.List _ | A.Node -> expr builder sx 
            | _ -> let data = L.build_malloc (ltype_of_typ t) "data" builder in
              let llvm =  expr builder sx 
              in ignore(L.build_store llvm data builder); data)
          in let data = L.build_bitcast data void_ptr_t "data" builder in
            ignore(L.build_call node_add_tail_func [| lst; data |] "node_add_tail" builder); list_fill lst rest) in
          let m = L.build_call make_node_func [||] "make_node" builder in
          list_fill m l

      | SListLit l -> let rec list_fill lst = (function
          [] -> lst
          | sx :: rest ->
          let (t, _) = sx in 
          let data = (match t with
              A.List _  -> expr builder sx 
            | _ -> let data = L.build_malloc (ltype_of_typ t) "data" builder in
              let llvm =  expr builder sx 
              in ignore(L.build_store llvm data builder); data)
          in let data = L.build_bitcast data void_ptr_t "data" builder in
            ignore(L.build_call list_add_tail_func [| lst; data |] "list_add_tail" builder); list_fill lst rest) in
          let m = L.build_call make_list_func [||] "make_list" builder in
          list_fill m l

      | SNodeGet(l, idx) ->
        let ltype = ltype_of_typ styp in
        let node = expr builder l in
        let index = expr builder idx in
        let data = L.build_call node_get_data_func [| node; index |] "index" builder in
          (match styp with 
            A.List _ | A.Node | A.Str -> L.build_bitcast data ltype "data" builder
          | _ -> let data = L.build_bitcast data (L.pointer_type ltype) "data" builder in
            L.build_load data "data" builder)

      
      | SListGet(l, idx) ->
        let ltype = ltype_of_typ styp in
        let lst = expr builder l in
        let index = expr builder idx in
        let data = L.build_call list_get_func [| lst; index |] "index" builder in
          (match styp with 
            A.List _ | A.Node | A.Str -> L.build_bitcast data ltype "data" builder
          | _ -> let data = L.build_bitcast data (L.pointer_type ltype) "data" builder in
            L.build_load data "data" builder)

      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with 
	    A.Add     -> L.build_fadd
	  | A.Sub     -> L.build_fsub
	  | A.Mult    -> L.build_fmul
	  | A.Div     -> L.build_fdiv 
          | A.Mod     -> L.build_frem
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
	  | A.Neq     -> L.build_fcmp L.Fcmp.One
	  | A.Less    -> L.build_fcmp L.Fcmp.Olt
	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
	  | A.And | A.Or ->
	      raise (Failure "internal error: semant should have rejected and/or on float")
	  ) e1' e2' "tmp" builder
      
      |  SBinop ((A.Str,_ ) as e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with
           A.Add     -> L.build_call string_concat_f [| e1'; e2' |] "string_concat" builder
 | _ -> raise (Failure ("operation " ^ (A.string_of_op op) ^ " not implemented")))

       | SBinop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_srem
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | SUnop(op, ((t, _) as e)) ->
          let e' = expr builder e in
	  (match op with
	    A.Neg when t = A.Float -> L.build_fneg 
	  | A.Neg                  -> L.build_neg
          | A.Incr -> raise (Failure "Incr should never be called in codegen")
          | A.Decr -> raise (Failure "Decr should never be called in codegen")  
          | A.Not                  -> L.build_not) e' "tmp" builder
           
      |  SCall ("print", [e]) | SCall ("printb", [e]) ->
          L.build_call printf_func [| int_format_str ; (expr builder e) |]
            "printf" builder
      | SCall ("prints", [e]) ->
          L.build_call printf_func [| str_format_str ; (expr builder e) |] "print f" builder
      | SCall ("printbig", [e]) ->
          L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | SCall ("printf", [e]) ->
          L.build_call printf_func [| float_format_str ; (expr builder e) |]
            "printf" builder
     | SCall ("lens", [s]) -> 
                     L.build_call string_length_f [| expr builder s |] "lens" builder
     | SCall ("list_len", [l]) ->
                     L.build_call list_length_f [| expr builder l |] "list_len" builder
     | SCall ("printl", [e]) ->
                L.build_call printl_func [| (expr builder e) |] "printl" builder
     | SCall ("printil", [e]) ->
                L.build_call printil_func [| (expr builder e) |] "printil" builder
     | SCall ("listSort", [l]) -> 
                     L.build_call listSort_func [| expr builder l |] "listSort" builder 
     | SCall ("list_set", [l; i; d]) ->
                     L.build_call list_set_func [| expr builder l; expr builder i; expr builder d|] "list_set" builder
     | SCall ("string_concat", [s1; s2]) -> 
                     L.build_call string_concat_f 
                     [| expr builder s1; expr builder s2 |] 
                     "string_concat" builder

    | SCall ("node_set", [n; d]) -> 
                     L.build_call node_set_func [| expr builder n; expr builder d|] "node_set" builder

      | SCall ("list_concat", [l; m]) ->
                     L.build_call list_concat_func [| expr builder l; expr builder m |] "list_concat" builder



      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
      	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
      	 let result = (match fdecl.styp with 
                              A.Void -> ""
                            | _ -> f ^ "_result") in
               L.build_call fdef (Array.of_list llargs) result builder
          in
    
          (* LLVM insists each basic block end with exactly one "terminator" 
             instruction that transfers control.  This function runs "instr builder"
             if the current block does not already have a terminator.  Used,
             e.g., to handle the "fall off the end of the function" case. *)
          let add_terminal builder instr =
            match L.block_terminator (L.insertion_block builder) with
      	     Some _ -> ()
            | None -> ignore (instr builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

      let rec stmt builder = function
    	 SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder 
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                   builder
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   build_br_merge;

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
