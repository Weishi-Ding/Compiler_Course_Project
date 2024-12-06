(* open Llvm *)
open Sast
open Semant
module L = Llvm
module A = Ast
module StringMap = Map.Make (String)
module FunSigMap = Map.Make (FunSig)

let translate ((program : sprogramComp list), classMap_from_sement) =
  (* print_string "In codegen" *)
  let context = L.global_context () in
  let funMap = ref FunSigMap.empty
  and globalvarMap = ref StringMap.empty
  and classMap = ref StringMap.empty
  and classTypeMap = ref StringMap.empty in
  
  (* Add types to the context *)
  let string_t = L.i8_type context (*string_ptr*)
  and bool_t = L.i1_type context (*bool*)
  and i32_t = L.i32_type context
  and int_t = L.i64_type context (*int*)
  and void_t = L.void_type context
  and double_t = L.double_type context
  and the_module = L.create_module context "MicroJ" in

  (* Covert MicroJ types to LLVM types *)
  let ltype_of_typ = function
    | A.String -> L.pointer_type string_t
    | A.Int -> int_t
    | A.Void -> void_t
    | A.Bool -> bool_t
    | A.Double -> double_t
    | IntList -> L.pointer_type int_t
    | DoubleList -> L.pointer_type double_t
    | BoolList -> L.pointer_type bool_t
    | StringList -> L.pointer_type (L.pointer_type string_t)
    | Object s -> let struct_type = try StringMap.find s !classTypeMap 
                                    with Not_found -> raise (Exception ("Type " ^ s ^ " is undefined")) 
                  in 
                    L.pointer_type struct_type
    | _ -> L.pointer_type int_t (* object list is unimplemented *)
  in
   (*** Add all class type into the classTypeMap ****)
  let add_class_struct_type className (fieldM,_,_,_)=
    let type_list =
      List.rev
        (StringMap.fold
           (fun _key (_, _, ty) acc ->  ty :: acc)
           fieldM [])
    in
    let struct_type = L.struct_type context (Array.of_list (List.map ltype_of_typ type_list)) in
      classTypeMap := StringMap.add className struct_type !classTypeMap 
        
  in
  let _ = StringMap.iter add_class_struct_type classMap_from_sement in
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type string_t |]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in
  (**************** Build global variables ***************)
  let global_vars vardef =
    match vardef with
    | SStmt (SExpr (_, SDefAsn (_, name, (t, value)))) ->
        let init =
          match t with
          | A.Double -> (
              match value with
              | SNull -> L.const_float (ltype_of_typ t) 2.0
              | SDliteral s ->
                  L.const_float (ltype_of_typ t) (float_of_string s)
              | _ -> raise (Failure "Expected Dliteral or SNull"))
          | A.Int -> (
              match value with
              | SNull -> L.const_int (ltype_of_typ t) 0
              | SLiteral i -> L.const_int (ltype_of_typ t) i
              | _ -> raise (Failure "Expected Sliteral or SNull"))
          | A.Bool -> (
              match value with
              | SNull -> L.const_int (ltype_of_typ t) 0
              | SLiteral i -> L.const_int (ltype_of_typ t) i
              | _ -> raise (Failure "Expected Sliteral or SNull"))
          | A.String -> (
              match value with
              (*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!constant string is not generated properly!!!!!!!!!!!!!!!!!!!!!!!!!!*)
              | SNull -> L.const_null (L.pointer_type string_t)
              | SStringLiteral s -> L.const_stringz context s
              | _ -> raise (Failure "Expected SStringliteral or SNull"))
          | _ -> raise (Failure "Not implemented yet")
        in

        (globalvarMap : L.llvalue StringMap.t ref)
        := StringMap.add name
             (L.define_global name init the_module)
             !globalvarMap
    | _ -> raise (Failure "Only global variable allowed outside main")
  in
  let function_decl sx =
    match sx with
    | SFun sfundef ->
        let name = sfundef.id
        and args_types = List.map (fun (t, _) -> t) sfundef.args
        and args_types' =
          Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) sfundef.args)
        in
        let key : FunSig.t = (name, args_types) in
        let ftype = L.function_type (ltype_of_typ sfundef.ty) args_types' in
        funMap :=
          FunSigMap.add key
            (L.define_function name ftype the_module, sfundef)
            !funMap
    | _ -> raise (Exception "Expected SFun") 
  in

  let rec build_function_body (the_function, (sfundef : sfundef)) (if_build_class_method)=
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and double_format_str = L.build_global_stringptr "%f\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    let local_vars =
      let add_formal m (t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        let _ = L.build_store p local builder in
        StringMap.add n local m
      in
      ref
        (List.fold_left2 add_formal StringMap.empty sfundef.args
           (Array.to_list (L.params the_function)))
    in
    let lookup n =
      try StringMap.find n !local_vars
      with Not_found -> (
        try StringMap.find n !globalvarMap
        with Not_found -> raise (Failure ("undeclared identifier " ^ n)))
    in
    let _ = (match if_build_class_method with
      None -> () 
    | Some (obname, fieldM') -> 
      (* let loaded_struct_ptr = L.build_load struct_ptr "struct_ptr_val" builder in *)
      ignore (StringMap.fold (fun field_name _value acc ->   (** add fields into globalvarMap**)
        (* let _ = raise (Exception "") in *)
        let struct_ptr = L.build_load (lookup obname) obname builder in
        let field_ptr = L.build_struct_gep struct_ptr acc "field_ptr" builder in   
        let _ = globalvarMap := StringMap.add field_name field_ptr !globalvarMap in
        acc + 1) fieldM' 0 ) )
    in
    (**********find_method function will recursivly find the method. It will lookup in its father until raise Not found************)
    let rec find_method key cname =
      let _, methodM', _, (cdf : sclassdef) = StringMap.find cname !classMap in
      try FunSigMap.find key methodM'
      with Not_found -> (
        match cdf.father with
        | Some fname -> find_method key fname
        | None -> raise (Failure ("Function " ^ fst key ^ " is undefined")))
    in

    (**********find_field function will recursivly find the method. It will lookup in its father until raise Not found************)
    let rec find_field (key : string) cname =
      let fieldM', _, _, (cdf : sclassdef) = StringMap.find cname !classMap in
      try StringMap.find key fieldM'
      with Not_found -> (
        match cdf.father with
        | Some fname -> find_field key fname
        | None -> raise (Failure ("Field " ^ key ^ " is undefined")))
    in

    let rec expr builder ((_, e) : sexpr) =
      (***** helper function for SAccess *****)
      (******** field_or_method returns a pointers to the field of a struct *******)
      let field_or_method e funM fieldM struct_ptr' obname= 
        match e with
        | _, SCall (fname, args) ->
            let fdef, (fdecl : sfundef) =
              try FunSigMap.find (fname, get_formals_type args) funM
              with Not_found ->
                raise (Exception ("Constructor " ^ fname ^ " undefined"))
            in

            (* let _ = StringMap.fold (fun key _value k ->  (**extend the environment**)
              let field_ptr = L.build_struct_gep struct_ptr' k ("field_ptr"^ string_of_int k) builder in           
              let _ = globalvarMap := StringMap.add key field_ptr !globalvarMap in k + 1) fieldM 0
            in *)
            let _ = build_function_body (fdef,fdecl) (Some (obname, fieldM)) in
            let llargs = List.rev (List.map (expr builder) (List.rev args)) in
            let result =
              match fdecl.ty with A.Void -> "" | _ -> fname ^ "_result"
            in
              L.build_call fdef (Array.of_list llargs) result builder
        | _, SId name ->
            let k = snd  (*get index*)
                (StringMap.fold
                   (fun key _value (index, answer) ->
                     if key = name then (index + 1, index)
                     else (index + 1, answer))
                   fieldM (0, 0))
            in
            let field_ptr = L.build_struct_gep struct_ptr' k "field_ptr" builder in
              field_ptr
        | _ -> raise (Exception "Cannot access things other than field or method")
      in
      let get_index se builder =
        let llv = expr builder se in
        match Llvm.int64_of_const llv with
        | None -> raise (Failure "index must be an integer")
        | Some v ->
            if Int64.compare v Int64.zero >= 0 then v
            else raise (Failure "index cannot be negative")
      in
      match e with
      | SLiteral i -> L.const_int int_t i
      | SStringLiteral s ->
          let str_ptr = L.build_global_stringptr s "str" builder in
          let str_ptr_cast =
            L.build_bitcast str_ptr (L.pointer_type string_t) "strcast" builder
          in
          str_ptr_cast
      | SBoolLit b -> L.const_int bool_t (if b then 1 else 0)
      | SDliteral l -> L.const_float_of_string double_t l
      | SNoexpr -> L.const_int i32_t 0
      | SId s -> L.build_load (lookup s) s builder
      | SDefAsn (t, name, sexpr) ->
          let t', e = sexpr in
          let local = L.build_alloca (ltype_of_typ t) name builder in
          (* let () = prerr_endline(L.string_of_llvalue local)  in *)
          let _ = local_vars := StringMap.add name local !local_vars in
          let value =
            match t' with
            | Int | Bool -> (
                match e with
                | SNull -> L.const_int (ltype_of_typ t) 0
                | sexpr -> expr builder (t, sexpr))
            | String -> (
                match e with
                | SNull -> L.const_null (L.pointer_type string_t)
                | sexpr -> expr builder (t, sexpr))
            | Double -> (
                match e with
                | SNull -> L.const_float (ltype_of_typ t) 0.0
                | sexpr -> expr builder (t, sexpr))
            | IntList -> (
                match e with
                | SNull -> L.const_null (L.pointer_type int_t)
                | sexpr -> expr builder (t, sexpr))
            | DoubleList -> (
                match e with
                | SNull -> L.const_null (L.pointer_type double_t)
                | sexpr -> expr builder (t, sexpr))
            | BoolList -> (
                match e with
                | SNull -> L.const_null (L.pointer_type bool_t)
                | sexpr -> expr builder (t, sexpr))
            | StringList -> (
                match e with
                | SNull -> L.const_null (L.pointer_type string_t)
                | sexpr -> expr builder (t, sexpr))
            | Object _o -> (
                match e with
                | SNull -> L.const_null (L.pointer_type int_t)
                | sexpr -> expr builder (t, sexpr))
            | _ -> raise (Exception "SDefAsn not implemented")
          in
          L.build_store value local builder
      | SCall ("charAt", [ (_, name); e ]) -> (
          let index = get_index e builder in
          let llvalueIndex = [| L.const_int int_t (Int64.to_int index) |] in
          match name with
          | SId name ->
              let get_array = L.build_load (lookup name) name builder in
              let get_element =
                L.build_gep get_array llvalueIndex "array" builder
              in
              let asic = L.build_load get_element name builder in
              (* let asic_64 = L.build_zext asic int_t "sb" builder in *)
              let () = raise (Failure (string_of_bool (L.is_constant asic))) in
              let ocaml_int64 =
                match L.int64_of_const asic with
                | None -> raise (Failure "index must be integer")
                | Some v -> v
              in
              let ocaml_char = Char.chr (Int64.to_int ocaml_int64) in
              let ocaml_string = String.make 1 ocaml_char in
              let sl = SStringLiteral ocaml_string in
              expr builder (A.String, sl)
          | _ -> raise (Exception "Expected a ID of string"))
      | SCall ("print", [ (t, e') ]) -> (
          match t with
          | A.Int | A.Bool | A.IntList ->
              L.build_call printf_func
                [| int_format_str; expr builder (t, e') |]
                "printf" builder
          | A.String ->
              L.build_call printf_func
                [| string_format_str; expr builder (t, e') |]
                "printf" builder
          | A.Double | A.DoubleList ->
              L.build_call printf_func
                [| double_format_str; expr builder (t, e') |]
                "printf" builder
          | A.Object "Animal" ->
              L.build_call printf_func
                [| int_format_str; expr builder (t, e') |]
                "printf" builder
          | _ -> raise (Exception "Unmatched print function"))
      | SCall (f, args) ->
          let get_formals_type args =
            List.rev
              (List.fold_left (fun accu (typ, _id) -> typ :: accu) [] args)
          in
          let fdef, fdecl =
            try FunSigMap.find (f, get_formals_type args) !funMap
            with Not_found ->
              raise (Exception ("Function " ^ f ^ " undefined"))
          in
          let llargs = List.rev (List.map (expr builder) (List.rev args)) in
          let result =
            match fdecl.ty with A.Void -> "" | _ -> f ^ "_result"
          in
            L.build_call fdef (Array.of_list llargs) result builder
      | SNewExpr (_ty, e) -> (
          match e with
          | SCall (fname, args) ->
              let _, constM, _, _ =
                try StringMap.find fname !classMap
                with Not_found ->
                  raise (Exception ("Class " ^ fname ^ " undefined"))
              in
              let fdef, (fdecl : sfundef) =
                try FunSigMap.find (fname, get_formals_type args) constM
                with Not_found ->
                  raise (Exception ("Constructor " ^ fname ^ " undefined"))
              in
              let llargs = List.rev (List.map (expr builder) (List.rev args)) in
              let result =
                match fdecl.ty with A.Void -> "" | _ -> fname ^ "_result"
              in
                L.build_call fdef (Array.of_list llargs) result builder 
              (* let format_str = L.build_global_stringptr "Size of struct: %ld\n" "fmt" builder in
      L.build_call printf_func [|format_str; L.size_of (L.type_of (L.build_load struct_ptr "" builder))|] "printf" builder  *)
          | _ -> raise (Exception "Expect a constructor after 'new' keyword\n"))
      | SAccess (e1, e2) -> (
          match e1 with
          | ty, SId name ->
              if StringMap.mem name !classMap then
                (*if it's a class name rather than a var name, then it can access static field or method*)
                let fieldM, _, methodM, _ = StringMap.find name !classMap in
                L.build_load (field_or_method e2 methodM fieldM (L.const_null int_t) name) "" builder
                (* The last argument is useless here, only to make the compiler happy*)
              else
                let struct_ptr = L.build_load (lookup name) name builder in
                let _ = globalvarMap := StringMap.add name (lookup name) !globalvarMap in
                let fieldM, _, methodM, _ =
                  try StringMap.find (A.string_of_type ty) !classMap
                  with Not_found ->
                    raise
                      (Exception
                         ("Class " ^ A.string_of_type ty ^ " is undefined"))
                in
                (match e2 with (_ ,SCall _) -> field_or_method e2 methodM fieldM struct_ptr name
                  | (_, SId _) -> L.build_load (field_or_method e2 methodM fieldM struct_ptr name) "" builder
                  | _ -> raise (Exception "Cannot access things other than field or method"))
                
          | _ ->
              raise
                (Failure "Expect an object type on left-hand side of Access"))
      | SAsn (e1, e2) ->
        (match e1 with (_, SId s) -> 
          let e2' = expr builder e2 in
          let _ = L.build_store e2' (lookup s) builder in
            e2'
        | (ty, SAccess((cty, SId name), e''))->   (***Now only allows to modify a field. Modify method is not allowed ***)
          let struct_ptr = L.build_load (lookup name) name builder in
          let fieldM, _, methodM, _ =
            try StringMap.find (A.string_of_type cty) !classMap
            with Not_found ->
              raise
                (Exception
                   ("Class " ^ A.string_of_type ty ^ " is undefined"))
          in
          let target_ptr = field_or_method e'' methodM fieldM struct_ptr "" in
          let e2' = expr builder e2 in
          let _ = L.build_store e2' target_ptr builder in
            e2'
        | _ -> raise (Exception "Bad left-hand side of assign"))
      | SBinop (e1, op, e2) ->
          let t, _ = e1 and e1' = expr builder e1 and e2' = expr builder e2 in
          if t = A.Double then
            (match op with
            | A.Add -> L.build_fadd
            | A.Sub -> L.build_fsub
            | A.Mult -> L.build_fmul
            | A.Div -> L.build_fdiv
            | A.Equal -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq -> L.build_fcmp L.Fcmp.One
            | A.Less -> L.build_fcmp L.Fcmp.Olt
            | A.Leq -> L.build_fcmp L.Fcmp.Ole
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt
            | A.Geq -> L.build_fcmp L.Fcmp.Oge
            | A.And | A.Or ->
                raise
                  (Failure
                     "internal error: semant should have rejected and/or on \
                      float"))
              e1' e2' "tmp" builder
          else
            (match op with
            | A.Add -> L.build_add
            | A.Sub -> L.build_sub
            | A.Mult -> L.build_mul
            | A.Div -> L.build_sdiv
            | A.And -> L.build_and
            | A.Or -> L.build_or
            | A.Equal -> L.build_icmp L.Icmp.Eq
            | A.Neq -> L.build_icmp L.Icmp.Ne
            | A.Less -> L.build_icmp L.Icmp.Slt
            | A.Leq -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.Geq -> L.build_icmp L.Icmp.Sge)
              e1' e2' "tmp" builder
      | SUnop (op, e) ->
          let t, _ = e in
          let e' = expr builder e in
          (match op with
          | A.Neg when t = A.Double -> L.build_fneg
          | A.Neg -> L.build_neg
          | A.Not -> L.build_not)
            e' "tmp" builder
      | SNewArray (ty, sexpr) -> (
          match ty with
          | Int ->
              let index = get_index sexpr builder in
              let llvalueIndex = L.const_int int_t (Int64.to_int index) in
              L.build_array_malloc int_t llvalueIndex "array" builder
          | Double ->
              let index = get_index sexpr builder in
              let llvalueIndex = L.const_int int_t (Int64.to_int index) in
              L.build_array_malloc int_t llvalueIndex "array" builder
          | String ->
              let index = get_index sexpr builder in
              let llvalueIndex = L.const_int int_t (Int64.to_int index) in
              L.build_array_malloc int_t llvalueIndex "array" builder
          | Bool ->
              let index = get_index sexpr builder in
              let llvalueIndex = L.const_int int_t (Int64.to_int index) in
              L.build_array_malloc int_t llvalueIndex "array" builder
          | _ -> raise (Exception "Wait for implement"))
      | SIndexing (id, sexpr, sexpr_o) -> (
          let index = get_index sexpr builder in
          let llvalueIndex = [| L.const_int int_t (Int64.to_int index) |] in
          let get_array = L.build_load (lookup id) id builder in
          let get_element =
            L.build_gep get_array llvalueIndex "array" builder
          in
          match sexpr_o with
          | Some sexp ->
              let e' = expr builder sexp in
              L.build_store e' get_element builder
          | None -> L.build_load get_element id builder)
      | _ -> raise (Exception "New Need Implementation")
    in
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> () (***if the current block already has a terminator***)
      | None -> ignore (instr builder)
      (***if the current block doesn't have a terminator***)
    in
    let rec stmt builder block = function
      | SBlock sl ->
          List.fold_left (fun accb st -> stmt accb block st) builder sl
      | SReturn e ->
          let _ =
            match sfundef.ty with
            (* Special "return nothing" instr *)
            | A.Void -> L.build_ret_void builder (* Build return statement *)
            | _ -> L.build_ret (expr builder e) builder
          in
          builder
      | SControlFlow Break -> (
          match block with
          | None -> raise (Exception "No destination for break")
          | Some bb_list ->
              let _ = L.build_br (fst bb_list) builder in
              builder)
      | SControlFlow Continue -> (
          match block with
          | None -> raise (Exception "No destination for continue")
          | Some bb_list ->
              let _ = L.build_br (snd bb_list) builder in
              builder)
      | SExpr e ->
          let _ = expr builder e in
          builder
      | SIf (predicate, then_stmt, else_stmt) ->
          let bool_val = expr builder predicate in
          (* Add "merge" basic block to our function's list of blocks *)
          let merge_bb = L.append_block context "merge" the_function in
          (* Partial function used to generate branch to merge block *)
          let branch_instr = L.build_br merge_bb in

          (* Same for "then" basic block *)
          let then_bb = L.append_block context "then" the_function in
          (* Position builder in "then" block and build the statement *)
          let then_builder =
            stmt (L.builder_at_end context then_bb) block then_stmt
          in
          (* Add a branch to the "then" block (to the merge block)
             if a terminator doesn't already exist for the "then" block *)
          let () = add_terminal then_builder branch_instr in

          (* Identical to stuff we did for "then" *)
          let else_bb = L.append_block context "else" the_function in
          let else_builder =
            stmt (L.builder_at_end context else_bb) block else_stmt
          in
          let () = add_terminal else_builder branch_instr in

          (* Generate initial branch instruction perform the selection of "then"
             or "else". Note we're using the builder we had access to at the start
             of this alternative. *)
          let _ = L.build_cond_br bool_val then_bb else_bb builder in
          (* Move to the merge block for further instruction building *)
          L.builder_at_end context merge_bb
      | SWhile (predicate, body) ->
          (* First create basic block for condition instructions -- this will
             serve as destination in the case of a loop *)
          let pred_bb = L.append_block context "while" the_function in
          (* In current block, branch to predicate to execute the condition *)
          let _ = L.build_br pred_bb builder in

          (* Create the body's block, generate the code for it, and add a branch
             back to the predicate block (we always jump back at the end of a while
             loop's body, unless we returned or something) *)
          let body_bb = L.append_block context "while_body" the_function in
          let merge_bb = L.append_block context "merge" the_function in
          let while_builder =
            stmt
              (L.builder_at_end context body_bb)
              (Some (merge_bb, pred_bb))
              body
          in
          let () = add_terminal while_builder (L.build_br pred_bb) in

          (* Generate the predicate code in the predicate block *)
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder predicate in

          (* Hook everything up *)
          let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
          L.builder_at_end context merge_bb
      | SFor (e1, e2, e3, body) ->
          (*semant can not regconized var in e1 e2 e3*)
          stmt builder block
            (SBlock [ SExpr e1; SWhile (e2, SBlock [ body; SExpr e3 ]) ])
      | _ -> raise (Exception "stmt function needs Implementation")
    in
    let builder = stmt builder None (SBlock sfundef.body) in
    add_terminal builder
      (match sfundef.ty with
      | A.Void -> L.build_ret_void
      | A.Double -> L.build_ret (L.const_float double_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  (* build a class definition*)
  let class_decl (c : sclassdef) =
    (* Step 1: scan through the sclassdef and creat three maps*)
    let scanning (fieldM, constM, methodM) (s : sclassStmt) =
      match s with
      | SConstructorDef sfundef ->
          (* add to local methodMap*)
          let name = sfundef.id
          and args_types = List.map (fun (t, _) -> t) sfundef.args
          and args_types' =
            Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) sfundef.args)
          in
          let key : FunSig.t = (name, args_types) in
          let ftype = L.function_type (ltype_of_typ sfundef.ty) args_types' in
          let constM =
            FunSigMap.add key
              (L.define_function name ftype the_module, sfundef)
              constM
          in
          (fieldM, constM, methodM)
      | SFieldDef (accflag, fflag, ty, id, e) ->
          (* add a field to local fieldmap*)
          let accflag' = A.string_of_ac accflag in
          let fflag' =
            match fflag with None -> "nonstatic" | Some _ -> "static"
          in
          let fieldM = StringMap.add id (accflag', fflag', ty, e) fieldM in
          (fieldM, constM, methodM)
      | SMethodDef (_accflag, _fflag, sfundef) ->
          (* let accflag' =  A.string_of_ac accflag in
             let fflag'   = match fflag with None -> "nonstatic" | Some _ -> "static" in *)
          let name = sfundef.id
          and args_types = List.map (fun (t, _) -> t) sfundef.args
          and args_types' =
            Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) sfundef.args)
          in
          let key : FunSig.t = (name, args_types) in
          let ftype = L.function_type (ltype_of_typ sfundef.ty) args_types' in
          let methodM =
            FunSigMap.add key
              (L.define_function name ftype the_module, sfundef)
              methodM
          in
          (fieldM, constM, methodM)
    in
    let fieldM, constM, methodM =
      (* perform scanning here *)
      List.fold_left scanning
        (StringMap.empty, FunSigMap.empty, FunSigMap.empty)
        c.body
    in
    let _ = classMap := StringMap.add c.id (fieldM, constM, methodM, c) !classMap in
    (*add the new class into classMap*)
    (* now use the field map to build the struct*)
    (***get the current struct type***)
    let (fieldM',_,_,_) = StringMap.find c.id classMap_from_sement in   (**search in classMap_from_sement **)
    let type_list =
      List.rev
        (StringMap.fold
            (fun _key (_, _, ty) acc ->  ty :: acc)
            fieldM' [])
    in
    let struct_type = L.struct_type context (Array.of_list (List.map ltype_of_typ type_list)) in
    let current_struct_ptr = L.define_global "my_struct_ptr" (L.const_null (L.pointer_type struct_type)) the_module in (**store the struct_ptr of current class, it will be used in build method**)
    let field_address_Map : L.llvalue list ref = ref [] in
    let build_constructor_body (the_function, (sfundef : sfundef)) =
      let builder = L.builder_at_end context (L.entry_block the_function) in
      (* let _ = classTypeMap := StringMap.add c.id struct_type !classTypeMap in *)
      let struct_ptr = L.build_malloc struct_type "struct_ptr" builder in
      let _ = L.build_store struct_ptr current_struct_ptr builder in
      let initialize_field k ty =
        let field_ptr = L.build_struct_gep struct_ptr k "field_ptr" builder in
        let _ = field_address_Map := field_ptr :: !field_address_Map in
        let init =
          match ty with
            A.Int -> L.const_int int_t 2
          | A.Bool -> L.const_int int_t 0
          | A.Double -> L.const_float double_t 1.11
          | A.String -> L.const_stringz context "" (**** could be wrong here ***)
          | _ -> raise (Exception "invalid type")
        in
        let _ = L.build_store init field_ptr builder in
          k + 1
      in
      let _ = List.fold_left initialize_field 0 type_list in
      (* use initialze_field here*)
      let _ = L.build_struct_gep struct_ptr 0 "" builder in             
        L.build_ret struct_ptr builder (* return the pointer to the struct*)
    in
      FunSigMap.iter (fun _key value -> ignore (build_constructor_body value)) constM (* build all constructors here*)
    (***build all method**)
    (** The trick here is to add the field of a class into the globalvarMap, and remove them after building all method body**)
    (** Probably need to do the same thing for funMap **)
    (* let _ = field_address_Map := List.rev !field_address_Map in *)
    (* let build_method_body (the_function, (sfundef : sfundef)) = *)
      (* build_function_body (the_function, sfundef)    * build the method body* *)
      (* let _ = raise (Exception( "Lengrth of globalvarMap: " ^ string_of_int (StringMap.cardinal !globalvarMap))) in *)
        (* StringMap.iter (fun key _value -> ignore (globalvarMap := StringMap.remove key !globalvarMap)) fieldM' *remove fields from globalvarMap* *)
    (* in *)
      (* FunSigMap.iter (fun _key value -> build_method_body value) methodM  iteratively build methods *)
  in
  (*body of classdecl*)
  let generate_all = function
    | SStmt _e as s -> global_vars s
    | SFun _sfundef as s -> function_decl s
    | SClass sclassdef -> class_decl sclassdef
    | _ -> raise (Exception "Not yet implemented")
  in
  let _ = List.iter generate_all program in
  let _ = FunSigMap.iter (fun _key value -> build_function_body value None) !funMap in
  (* let _ = StringMap.iter (fun _key (_,_,methodM,_) -> FunSigMap.iter (fun _key value -> build_function_body value None) methodM) !classMap in *)
  the_module
