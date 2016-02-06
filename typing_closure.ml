open Closure

let extenv = ref M.empty

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t
exception Not_found_term of t

let convert_label str =
  if (String.length str >= 10) && (String.sub str 0 9 = "min_caml_") then
    String.sub str 9 ((String.length str) - 9)
  else
    str

let rec deref_typ = function (* 型変数を中身でおきかえる関数 *)
  | Type.Fun(t1s, t2) -> Type.Fun(List.map deref_typ t1s, deref_typ t2)
  | Type.Tuple(ts) -> Type.Tuple(List.map deref_typ ts)
  | Type.Array(t) -> Type.Array(deref_typ t)
  | Type.Var({ contents = None } as r) ->
      Format.eprintf "uninstantiated type variable detected; assuming int@.";
      r := Some(Type.Int);
      Type.Int
  | Type.Var({ contents = Some(t) } as r) ->
      let t' = deref_typ t in
      r := Some(t');
      t'
  | t -> t
let rec deref_id_typ (x, t) = (x, deref_typ t)
let rec deref_id_typ_lst = function
  | [] -> []
  | (x, t) :: lst -> (x, deref_typ t) :: (deref_id_typ_lst lst)
let rec deref_term = function
  | IfEq(x1, x2, e1, e2) -> IfEq(x1, x2, deref_term e1, deref_term e2)
  | IfLE(x1, x2, e1, e2) -> IfLE(x1, x2, deref_term e1, deref_term e2)
  | Let(xt, e1, e2) -> Let(deref_id_typ xt, deref_term e1, deref_term e2)
  | MakeCls(xt, clos, e) -> MakeCls(deref_id_typ xt, clos, deref_term e)
  | LetTuple(xts, x, e) -> LetTuple(deref_id_typ_lst xts, x, deref_term e)
  | e -> e
let deref_fundef { name = name;
		   args = xts1;
		   formal_fv = xts2;
		   body = e } =
  { name = name;
    args = deref_id_typ_lst xts1;
    formal_fv = deref_id_typ_lst xts2;
    body = deref_term e }
let rec deref_fundefs = function
  | [] -> []
  | fundef :: fundefs -> (deref_fundef fundef) :: (deref_fundefs fundefs)
let deref_prog (Prog(fundefs, e)) = Prog(deref_fundefs fundefs, deref_term e)

let rec xs_to_xts env = function
  | [] -> []
  | x :: lst -> (x, M.find x env) :: (xs_to_xts env lst)

let rec occur r1 = function (* occur check *)
  | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
  | Type.Tuple(t2s) -> List.exists (occur r1) t2s
  | Type.Array(t2) -> occur r1 t2
  | Type.Var(r2) when r1 == r2 -> true
  | Type.Var({ contents = None }) -> false
  | Type.Var({ contents = Some(t2) }) -> occur r1 t2
  | _ -> false

let rec unify t1 t2 = (* 型が合うように、型変数への代入をする *)
  match t1, t2 with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
      (try List.iter2 unify t1s t2s
      with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)));
      unify t1' t2'
  | Type.Tuple(t1s), Type.Tuple(t2s) ->
      (try List.iter2 unify t1s t2s
      with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)))
  | Type.Array(t1), Type.Array(t2) -> unify t1 t2
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
  | _, Type.Var({ contents = Some(t2') }) -> unify t1 t2'
  | Type.Var({ contents = None } as r1), _ -> (* 一方が未定義の型変数の場合 *)
      if occur r1 t2 then raise (Unify(t1, t2));
      r1 := Some(t2)
  | _, Type.Var({ contents = None } as r2) ->
      if occur r2 t1 then raise (Unify(t1, t2));
      r2 := Some(t1)
  | _, _ -> raise (Unify(t1, t2))

let rec g env e =
  try
    match e with
    | Unit -> Type.Unit
    | Int(_) -> Type.Int
    | Float(_) -> Type.Float
    | Neg(x) ->
       unify Type.Int (M.find x env);
       Type.Int
    | Add(x1, x2) | Sub(x1, x2) ->
       unify Type.Int (M.find x1 env);
       unify Type.Int (M.find x2 env);
       Type.Int
    | FNeg(x) ->
       unify Type.Float (M.find x env);
       Type.Float
    | FAdd(x1, x2) | FSub(x1, x2) | FMul(x1, x2) | FDiv(x1, x2) ->
       unify Type.Float (M.find x1 env);
       unify Type.Float (M.find x2 env);
       Type.Float
    | IfEq(x1, x2, e1, e2) | IfLE(x1, x2, e1, e2) ->
       unify (M.find x1 env) (M.find x2 env);
       let t1 = g env e1 in
       let t2 = g env e2 in
       unify t1 t2;
       t1
    | Let((x, t), e1, e2) ->
       unify t (g env e1);
       g (M.add x t env) e2
    | Var(x) when M.mem x env ->
       M.find x env
    | Var(x) when M.mem x !extenv ->
       M.find x !extenv
    | Var(x) -> (* 型推論しなきゃいけない外部変数 *)
       let t = ref Type.Unit in
       (match x with
	| "fabs" | "abs_float" | "sqrt" | "floor" | "cos" | "sin" | "atan"
          -> t := Type.Fun([Type.Float], Type.Float)
	| "int_of_float" | "truncate"
          -> t := Type.Fun([Type.Float], Type.Int)
	| "float_of_int"
	  -> t := Type.Fun([Type.Int], Type.Float)
	| "print_newline"
	  -> t := Type.Fun([Type.Unit], Type.Unit)
	| "print_int"
	  -> t := Type.Fun([Type.Int], Type.Unit)
	| "print_float"
	  -> t := Type.Fun([Type.Float], Type.Unit)
	| "read_int"
	  -> t := Type.Fun([Type.Unit], Type.Int)
	| "read_float"
	  -> t := Type.Fun([Type.Unit], Type.Float)
	| _ ->
	   (Format.eprintf "free variable %s assumed as external@." x;
	    t := Type.gentyp ()));
       extenv := M.add x !t !extenv;
       !t
    | MakeCls((x, t1), { entry = Id.L str; actual_fv = fv }, e) when M.mem x env ->
       let xx = convert_label str in
       let t2 = M.find x env in
       let t3 = if M.mem xx env then M.find xx env else Type.gentyp () in
       unify t1 t2;
       unify t1 t3;
       unify t2 t3;
       g env e
    | MakeCls((x, t1), { entry = Id.L str; actual_fv = fv }, e) when M.mem (convert_label str) env ->
       let xx = convert_label str in
       let t3 = M.find xx env in
       unify t1 t3;
       g env e
    | MakeCls((x, t1), { entry = Id.L str; actual_fv = fv }, e) ->
       let env = M.add x t1 env in
       let env = M.add (convert_label str) t1 env in
       g env e
    | AppCls(x, xs) ->
       let t = Type.gentyp () in
       unify (M.find x env) (Type.Fun(List.map (fun x -> M.find x env) xs, t));
       t
    | AppDir(Id.L str, xs) ->
       let x = convert_label str in
       if x = "create_array" || x = "create_float_array" then
	 (* arrayのための "polymorphic" typing *)
	 (assert(List.length xs = 2);
	  let x1 = List.hd xs in
	  let x2 = List.hd (List.tl xs) in
	  unify Type.Int (M.find x1 env);
	  Type.Array(M.find x2 env))
       else
	 (* 内部実装されている関数は特別扱い *)
	 let rt = ref Type.Unit in
	 (match x with
	  | "fabs" | "abs_float" | "sqrt" | "floor" | "cos" | "sin" | "atan"
	    -> rt := Type.Fun([Type.Float], Type.Float)
	  | "int_of_float" | "truncate"
	    -> rt := Type.Fun([Type.Float], Type.Int)
	  | "float_of_int"
	    -> rt := Type.Fun([Type.Int], Type.Float)
	  | "print_newline"
	    -> rt := Type.Fun([Type.Unit], Type.Unit)
	  | "print_int"
	    -> rt := Type.Fun([Type.Int], Type.Unit)
	  | "print_float"
	    -> rt := Type.Fun([Type.Float], Type.Unit)
	  | "read_int"
	    -> rt := Type.Fun([Type.Unit], Type.Int)
	  | "read_float"
	    -> rt := Type.Fun([Type.Unit], Type.Float)
	  | _ -> rt := M.find str env);
	 let t = Type.gentyp () in
	 unify !rt (Type.Fun(List.map (fun x -> M.find x env) xs, t));
	 t
    | Tuple(xs) -> Type.Tuple(List.map (fun x -> M.find x env) xs)
    | LetTuple(xts, x, e) ->
       unify (Type.Tuple(List.map snd xts)) (M.find x env);
       g (M.add_list xts env) e
    | Get(x1, x2) ->
       let t = Type.gentyp () in
       unify (Type.Array(t)) (M.find x1 env);
       unify Type.Int (M.find x2 env);
       t
    | Put(x1, x2, x3) ->
       let t = M.find x3 env in
       unify (Type.Array(t)) (M.find x1 env);
       unify Type.Int (M.find x2 env);
       Type.Unit
    | ExtArray(Id.L str) ->
       M.find (convert_label str) env
  with Unify(t1, t2) -> raise (Error(deref_term e, deref_typ t1, deref_typ t2))
     | Not_found -> raise (Not_found_term(deref_term e))
let g_fundef env { name = (Id.L str, t1);
		   args = xts1;
		   formal_fv = xts2;
		   body = e } =
  let env = M.add (convert_label str) t1 env in
  let (xts3, xts4) = List.partition (fun (x, t) -> M.mem x env) xts2 in
  List.iter (fun (x, t) -> unify t (M.find x env)) xts3;
  let env = M.add_list xts4 env in
  let env = M.add_list xts1 env in
  let t2 = g env e in
  (try unify (M.find (convert_label str) env) (Type.Fun(List.map snd xts1, t2))
  with Unify(t1, t2) -> raise (Error(deref_term e, deref_typ t1, deref_typ t2)));
  env
let rec g_fundefs env = function
  | [] -> env
  | fundef :: fundefs ->
     let env = g_fundef env fundef in
     g_fundefs env fundefs

let rec clear_typ_xts = function
  | [] -> []
  | (x, t) :: xts -> (x, Type.gentyp ()) :: (clear_typ_xts xts)
let rec clear_typ_e = function
  | IfEq(x1, x2, e1, e2) -> IfEq(x1, x2, clear_typ_e e1, clear_typ_e e2)
  | IfLE(x1, x2, e1, e2) -> IfLE(x1, x2, clear_typ_e e1, clear_typ_e e2)
  | Let((x, t), e1, e2) -> Let((x, Type.gentyp ()), clear_typ_e e1, clear_typ_e e2)
  | MakeCls((x, t), clos, e) -> MakeCls((x, Type.gentyp ()), clos, clear_typ_e e)
  | LetTuple(xts, x, e) -> LetTuple(clear_typ_xts xts, x, clear_typ_e e)
  | e -> e
let clear_typ_fundef { name = name;
		       args = args;
		       formal_fv = fv;
		       body = e } =
  { name = name;
    args = clear_typ_xts args;
    formal_fv = clear_typ_xts fv;
    body = clear_typ_e e }
let rec clear_typ_fundefs = function
  | [] -> []
  | fundef :: fundefs -> (clear_typ_fundef fundef) :: (clear_typ_fundefs fundefs)
let clear_typ (Prog(fundefs, e)) = Prog(clear_typ_fundefs fundefs, clear_typ_e e)

let f prog =
  let (Prog(fundefs, e)) = clear_typ prog in
  extenv := M.empty;
  (try
      let env = g_fundefs M.empty fundefs in
      unify Type.Unit (g env e)
    with Unify _ -> failwith "top level does not have type unit");
  extenv := M.map deref_typ !extenv;
  let prog' = deref_prog (Prog(fundefs, e)) in
  prog'

let compare prog =
  prog = f prog
