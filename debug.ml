open Syntax

(* id_emit : Id.t -> unit *)
(* 識別子情報を表示する *)
let id_emit id = Format.print_string id

(* type_emit : Type.t -> unit *)
(* 型情報を1文字で表示する *)
let rec type_emit = function
    Type.Unit    -> Format.print_string "u"
  | Type.Bool    -> Format.print_string "b"
  | Type.Int     -> Format.print_string "i"
  | Type.Float   -> Format.print_string "d"
  | Type.Fun _   -> Format.print_string "f"
  | Type.Tuple _ -> Format.print_string "t"
  | Type.Array _ -> Format.print_string "a"
  | Type.Var rt  -> (Format.print_string "v{";
		     (match !rt with
		      | None   -> Format.print_string "None"
		      | Some t -> type_emit t);
		     Format.print_string "}")

(** parser_emit用の関数 *)
(* id_ty_list_iter : (Id.t * Type.t) list -> unit *)
(* for parser_emit LetTuple and syntax_fundef_emit *)
let rec id_ty_list_iter = function
    [] -> ()
  | [(id, ty)] -> (Format.open_hbox ();
		   Format.print_string "(";
		   id_emit id;
		   Format.print_space ();
		   Format.print_string ":";
		   Format.print_space ();
		   type_emit ty;
		   Format.print_string ")";
		   Format.close_box ())
  | (id, ty) :: l -> (Format.open_hbox ();
		      Format.print_string "(";
		      id_emit id;
		      Format.print_space ();
		      Format.print_string ":";
		      Format.print_space ();
		      type_emit ty;
		      Format.print_string ");";
		      Format.close_box ();
		      Format.print_space ();
		      id_ty_list_iter l)

(* parser_emit : out_channel -> Syntax.t -> unit *)
(* pretty printer of Syntax.t *)
(* NB. Only this function changes out_channel to emitting Parser.t *)
let rec parser_emit oc s =
  (Format.set_formatter_out_channel oc;
   Format.set_margin 128;
   Format.open_vbox 0;
   parser_iter s;
   Format.print_space ();
   Format.close_box ();
   Format.print_flush ())
(* parsr_iter : Syntax.t -> unit *)
and parser_iter s =
  begin
    Format.open_box 2;
    begin
      match s with
      | Syntax.Unit ->
	 Format.print_string "Unit"
      | Syntax.Bool b ->
	 (Format.open_hbox ();
	  Format.print_string "(Bool";
	  Format.print_space ();
	  Format.print_bool b;
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.Int i ->
	 (Format.open_hbox ();
	  Format.print_string "(Int";
	  Format.print_space ();
	  Format.print_int i;
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.Float f ->
	 (Format.open_hbox ();
	  Format.print_string "(Float";
	  Format.print_space ();
	  Format.print_float f;
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.Not t ->
	 (Format.open_hbox ();
	  Format.print_string "(Not";
	  Format.print_space ();
	  parser_iter t;
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.Neg t ->
	 (Format.open_hbox ();
	  Format.print_string "(Neg";
	  Format.print_space ();
	  parser_iter t;
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.Add (t0, t1) ->
	 (Format.open_hbox ();
	  Format.print_string "(Add";
	  Format.print_space ();
	  Format.open_box 0;
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.Sub (t0, t1) ->
	 (Format.open_hbox ();
	  Format.print_string "(Sub";
	  Format.print_space ();
	  Format.open_box 0;
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.FNeg t ->
	 (Format.open_hbox ();
	  Format.print_string "(FNeg";
	  Format.print_space ();
	  parser_iter t;
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.FAdd (t0, t1) ->
	 (Format.open_hbox ();
	  Format.print_string "(FAdd";
	  Format.print_space ();
	  Format.open_box 0;
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.FSub (t0, t1) ->
	 (Format.open_hbox ();
	  Format.print_string "(FSub";
	  Format.print_space ();
	  Format.open_box 0;
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.FMul (t0, t1) ->
	 (Format.open_hbox ();
	  Format.print_string "(FMul";
	  Format.print_space ();
	  Format.open_box 0;
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.FDiv (t0, t1) ->
	 (Format.open_hbox ();
	  Format.print_string "(FDiv";
	  Format.print_space ();
	  Format.open_box 0;
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.Eq (t0, t1) ->
	 (Format.open_hbox ();
	  Format.print_string "(Eq";
	  Format.print_space ();
	  Format.open_box 0;
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.LE (t0, t1) ->
	 (Format.open_hbox ();
	  Format.print_string "(LE";
	  Format.print_space ();
	  Format.open_box 0;
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.If (t0, t1, t2) ->
	 (Format.open_hbox ();
	  Format.print_string "(If";
	  Format.print_space ();
	  Format.open_vbox 0;
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.print_space ();
	  parser_iter t2;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.Let ((id, ty), t0, t1) ->
	 (Format.open_vbox 1;
	  Format.print_string "(";
	  (* definition *)
	  Format.open_box 2;
	  Format.print_string "Let";
	  Format.print_space ();
	  id_emit id;
	  Format.print_space ();
	  Format.print_string ":";
	  Format.print_space ();
	  type_emit ty;
	  Format.print_space ();
	  Format.print_string "=";
	  Format.print_space ();
	  parser_iter t0;
	  Format.close_box ();
	  Format.print_space ();
	  (* body *)
	  Format.open_hbox ();
	  Format.print_string "in";
	  Format.print_space ();
	  parser_iter t1;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ());
      | Syntax.Var id ->
	 (Format.print_string "(Var";
	  Format.print_space ();
	  id_emit id;
	  Format.print_string ")")
      | Syntax.LetRec (fd, t) ->
	 (Format.open_vbox 2;
	  Format.print_string "(LetRec";
	  Format.print_space ();
	  syntax_fundef_emit fd;
	  Format.print_space ();
	  parser_iter t;
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.App (t0, t1_list) ->
	 (Format.print_string "(App";
	  Format.print_space ();
	  parser_iter t0;
	  Format.print_space ();
	  parser_list_emit t1_list;
	  Format.print_string ")")
      | Syntax.Tuple t_list ->
	 (Format.print_string "(Tuple";
	  Format.print_space ();
	  parser_list_emit t_list;
	  Format.print_string ")")
      | Syntax.LetTuple (tuple, t0, t1) ->
	 (Format.open_vbox 2;
	  Format.print_string "(";
	  Format.open_box 2;
	  Format.print_string "LetTuple";
	  Format.print_space ();
	  (* 定義リスト *)
	  Format.open_box 1;
	  Format.print_string "(";
	  id_ty_list_iter tuple;
	  Format.print_string ")";
	  Format.close_box ();
	  Format.print_space ();
	  Format.print_string "=";
	  Format.print_space ();
	  parser_iter t0;
	  Format.close_box ();
	  Format.print_space ();
	  (* body *)
	  Format.open_hbox ();
	  Format.print_string "in";
	  Format.print_space ();
	  parser_iter t1;
	  Format.close_box ();
	  Format.print_string ")";
	  Format.close_box ())
      | Syntax.Array (t0, t1) ->
	 (Format.print_string "[Array";
	  Format.print_space ();
	  parser_iter t0;
	  Format.print_string ",";
	  Format.print_space ();
	  parser_iter t1;
	  Format.print_string "]")
      | Syntax.Get (t0, t1) ->
	 (Format.print_string "(Get";
	  Format.print_space ();
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.print_string ")")
      | Syntax.Put (t0, t1, t2) ->
	 (Format.print_string "(Put";
	  Format.print_space ();
	  parser_iter t0;
	  Format.print_space ();
	  parser_iter t1;
	  Format.print_space ();
	  parser_iter t2;
	  Format.print_string ")")
    end;
    Format.close_box ()
  end
(* syntax_fundef_emit : Syntax.fundef -> unit *)
(* for parser_emit parser_iter LetRec *)
and syntax_fundef_emit { name = (id, ty); args = lst; body = t } =
  (Format.open_vbox 1;
   Format.print_string "{";
   (* name *)
   Format.open_hbox ();
   Format.print_string "name";
   Format.print_space ();
   Format.print_string "=";
   Format.print_space ();
   id_emit id;
   Format.print_space ();
   Format.print_string ":";
   Format.print_space ();
   type_emit ty;
   Format.print_string ";";
   Format.close_box ();
   Format.print_space ();
   (* args *)
   Format.open_hbox ();
   Format.print_string "args";
   Format.print_space ();
   Format.print_string "=";
   Format.print_space ();
   Format.open_box 1;
   Format.print_string "[";
   id_ty_list_iter lst;
   Format.print_string "]";
   Format.close_box ();
   Format.print_string ";";
   Format.close_box ();
   Format.print_space ();
   (* body *)
   Format.open_box 2;
   Format.print_string "body";
   Format.print_space ();
   Format.print_string "=";
   Format.print_space ();
   Format.open_box 0;
   parser_iter t;
   Format.close_box ();
   Format.close_box ();
   Format.print_string "}";
   Format.close_box ())
(* parser_list_emit : Type.t list -> unit *)
(* for parser_emit parser_iter App and Tuple *)
and parser_list_emit = function
    [] -> ()
  | [x]   -> parser_iter x;
  | x::xs -> (parser_iter x;
	      Format.print_space ();
	      parser_list_emit xs)

(** main.ml用の情報 *)
type level = (* どの段階でデバッグ出力するかを管理する型 *)
  | Parser
  | Typing
  | KNormal
  | Alpha
  | Iter
  | Closure
  | Virtual
  | Simm
  | RegAlloc
  | Emit

(* level_of_string : string -> Debug.level *)
(* 規定外なら(例外を出すのではなく)Emit扱いにする *)
let level_of_string = function
    "Parser"   -> Parser
  | "Typing"   -> Typing
  | "KNormal"  -> KNormal
  | "Alpha"    -> Alpha
  | "Parser_Iter"     -> Iter
  | "Closure"  -> Closure
  | "Virtual"  -> Virtual
  | "Simm"     -> Simm
  | "RegAlloc" -> RegAlloc
  | _          -> Emit