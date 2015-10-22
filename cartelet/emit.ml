open Asm

external getfl : float -> int32 = "getfl"

let stackset = ref S.empty (* ���Ǥ�Save���줿�ѿ��ν��� (caml2html: emit_stackset) *)
let stackmap = ref [] (* Save���줿�ѿ��Ρ������å��ˤ�������� (caml2html: emit_stackmap) *)
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad =
       if List.length !stackmap mod 2 = 0 then [] else [Id.gentmp Type.Int] in
     stackmap := !stackmap @ pad @ [x; x])
let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
  loc !stackmap
let offset x = 4 * List.hd (locate x)
let stacksize () = align (List.length !stackmap * 4)

let pp_id_or_imm = function
  | V(x) -> x
  | C(i) -> "$" ^ string_of_int i

(* �ؿ��ƤӽФ��Τ���˰������¤��ؤ���(register shuffling) (caml2html: emit_shuffle) *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] -> (* no acyclic moves; resolve a cyclic move *)
     (y, sw) :: (x, y) :: shuffle sw (List.map
					(function
					  | (y', z) when y = y' -> (sw, z)
					  | yz -> yz)
					xys)
  | xys, acyc -> acyc @ shuffle sw xys

(* ������.ml�ե�����ι��ֹ������յ� *)
let line oc p = Printf.fprintf oc "\t# %d\n" p.Lexing.pos_lnum

type dest = Tail | NonTail of Id.t (* �������ɤ�����ɽ���ǡ����� (caml2html: emit_dest) *)
let rec g oc = function (* ̿����Υ�����֥����� (caml2html: emit_g) *)
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
     g' oc (NonTail(x), exp);
     g oc (dest, e)
and g' oc = function (* ��̿��Υ�����֥����� (caml2html: emit_gprime) *)
  (* �����Ǥʤ��ä���׻���̤�dest�˥��å� (caml2html: emit_nontail) *)
  | NonTail(_), Nop _ -> ()
  | NonTail(x), Set(i, p) ->
     Printf.fprintf oc "#test\n";
     Printf.fprintf oc "\taddi\t%s %s $%d" x reg_zero i;
     line oc p
  | NonTail(x), SetL(Id.L(y), p) ->
     Printf.fprintf oc "\taddi\t%s %s %s" x reg_zero y;
     line oc p
  | NonTail(x), Mov(y, p) ->
     if x <> y then (Printf.fprintf oc "\tadd\t%s %s %s" x reg_zero y;
		     line oc p)
  | NonTail(x), Neg(y, p) ->
     Printf.fprintf oc "\tsub\t%s %s %s" x reg_zero y;
     line oc p
  | NonTail(x), Add(y, V(z), p) ->
     Printf.fprintf oc "\tadd\t%s %s %s" x y z;
     line oc p
  | NonTail(x), Add(y, C(z), p) ->
     Printf.fprintf oc "\taddi\t%s %s $%d" x y z; (* addi 0�ä��ʤ����ɤ�? *)
     line oc p
  | NonTail(x), Sub(y, V(z), p) ->
     Printf.fprintf oc "\tsub\t%s %s %s" x y z;
     line oc p
  | NonTail(x), Sub(y, C(z), p) ->
     Printf.fprintf oc "\taddi\t%s %s $%d" x y (-z); (* subi 0�ä��ʤ����ɤ�? *)
     line oc p
  | NonTail(x), Ld(y, V(z), i, p) ->
     assert(i = 4);
     Printf.fprintf oc "\tadd\t%s %s %s" reg_tmp y z;
     line oc p;
     Printf.fprintf oc "\tld\t0(%s) %s" reg_tmp x;
     line oc p
  | NonTail(x), Ld(y, C(j), i, p) ->
     assert(i = 4);
     Printf.fprintf oc "\tld\t%d(%s) %s" j y x;
     line oc p
  | NonTail(_), St(x, y, V(z), i, p) ->
     assert(i = 4);
     Printf.fprintf oc "\tadd\t%s %s %s" reg_tmp y z;
     line oc p;
     Printf.fprintf oc "\tst\t0(%s) %s" reg_tmp x;
     line oc p
  | NonTail(_), St(x, y, C(j), i, p) ->
     assert(i = 4);
     Printf.fprintf oc "\tst\t%d(%s) %s" j reg_zero x;
     line oc p
  | NonTail(x), FMov(y, p) ->
     if x <> y then (Printf.fprintf oc "\tfmov\t%s %s" y x;
		      line oc p)
  | NonTail(x), FNeg(y, p) ->
     Printf.fprintf oc "\tfneg\t%s %s" x y;
     line oc p
  | NonTail(x), FAdd(y, z, p) ->
     Printf.fprintf oc "\tfadd\t%s %s %s" x y z;
     line oc p
  | NonTail(x), FSub(y, z, p) ->
     Printf.fprintf oc "\tfneg\t%s %s" z z;
     line oc p;
     Printf.fprintf oc "\tfadd\t%s %s %s" x y z;
     line oc p
  | NonTail(x), FMul(y, z, p) ->
     Printf.fprintf oc "\tfmul\t%s %s %s" x y z;
     line oc p
  | NonTail(x), FDiv(y, z, p) ->
     Printf.fprintf oc "\tfinv\t%s %s" z z;
     line oc p;
     Printf.fprintf oc "\tfmul\t%s %s %s" x y z;
     line oc p
  | NonTail(x), LdF(y, V(z), i, p) ->
     assert(i = 4);
     Printf.fprintf oc "\tadd\t%s %s %s" reg_tmp y z;
     line oc p;
     Printf.fprintf oc "\tfld\t0(%s) %s" reg_tmp x;
     line oc p
  | NonTail(x), LdF(y, C(j), i, p) ->
     assert(i = 4);
     Printf.fprintf oc "\tfld\t%d(%s) %s" j y x;
     line oc p
  | NonTail(_), StF(x, y, V(z), i, p) ->
     assert(i = 4);
     Printf.fprintf oc "\tadd\t%s %s %s" reg_tmp y z;
     line oc p;
     Printf.fprintf oc "\tfst\t0(%s) %s" reg_tmp x;
     line oc p
  | NonTail(_), StF(x, y, C(j), i, p) ->
     assert(i = 4);
     Printf.fprintf oc "\tfst\t%d(%s) %s" j y x;
     line oc p
  | NonTail(_), Comment(s, p) ->
     Printf.fprintf oc "\t# %s\t" s;
     line oc p
  (* ����β���̿��μ��� (caml2html: emit_save) *)
  | NonTail(_), Save(x, y, p) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      Printf.fprintf oc "\tst\t%d(%s) %s" (offset y) reg_sp x;
      line oc p
  | NonTail(_), Save(x, y, p) when List.mem x allfregs && not (S.mem y !stackset) ->
     savef y;
     Printf.fprintf oc "\tfst\t%d(%s) %s" (offset y) reg_sp x;
     line oc p
  | NonTail(_), Save(x, y, p) -> assert (S.mem y !stackset); ()
  (* �����β���̿��μ��� (caml2html: emit_restore) *)
  | NonTail(x), Restore(y, p) when List.mem x allregs ->
      Printf.fprintf oc "\tld\t%d(%s) %s" (offset y) reg_sp x;
      line oc p
  | NonTail(x), Restore(y, p) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tfld\t%d(%s) %s" (offset y) reg_sp x;
      line oc p
  (* �������ä���׻���̤����쥸�����˥��åȤ���ret (caml2html: emit_tailret) *)
  | Tail, (Nop _ | St _ | StF _ | Comment _ | Save _ as exp) ->
      let p = Asm.pos_of_exp exp in
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tjr\t%s" reg_ra;
      line oc p
  | Tail, (Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
     let p = Asm.pos_of_exp exp in
     g' oc (NonTail(regs.(0)), exp);
     Printf.fprintf oc "\tjr\t%s" reg_ra;
     line oc p
  | Tail, (FMov _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ | LdF _  as exp) ->
      let p = Asm.pos_of_exp exp in
      g' oc (NonTail(fregs.(0)), exp);
      Printf.fprintf oc "\tjr\t%s" reg_ra;
      line oc p
  | Tail, (Restore(x, p) as exp) ->
     (match locate x with
      | [i] -> g' oc (NonTail(regs.(0)), exp)
      | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp)
      | _ -> assert false);
     Printf.fprintf oc "\tjr\t%s" reg_ra;
     line oc p
(* ���ʬ��ͽ¬�Τ��ȹͤ��� *)
  | Tail, IfEq(x, V(y), e1, e2, p) ->
     g'_tail_if oc e1 e2 "beq" x y p
  | Tail, IfEq(x, C(y), e1, e2, p) ->
     Printf.fprintf oc "\taddi\t%s %s $%d" reg_tmp x (-y);
     line oc p;
     g'_tail_if oc e1 e2 "beq" reg_tmp reg_zero p
  | Tail, IfLE(x, V(y), e1, e2, p) ->
     Printf.fprintf oc "\tslt\t%s %s %s" reg_tmp y x;
     line oc p;
     g'_tail_if oc e1 e2 "beq" reg_tmp reg_zero p
  | Tail, IfLE(x, C(y), e1, e2, p) ->
     Printf.fprintf oc "\tsub\t%s %s %s" reg_tmp reg_zero x;
     line oc p;
     Printf.fprintf oc "\taddi\t%s %s $%d" reg_tmp reg_tmp y;
     line oc p;
     Printf.fprintf oc "\tslt\t%s %s %s" reg_tmp reg_tmp reg_zero;
     line oc p;
     g'_tail_if oc e1 e2 "beq" reg_tmp reg_zero p
  | Tail, IfGE(x, V(y), e1, e2, p) ->
     Printf.fprintf oc "\tslt\t%s %s %s" reg_tmp x y;
     line oc p;
     g'_tail_if oc e1 e2 "beq" reg_tmp reg_zero p
  | Tail, IfGE(x, C(y), e1, e2, p) ->
     Printf.fprintf oc "\tsub\t%s %s %s" reg_tmp reg_zero x;
     line oc p;
     Printf.fprintf oc "\taddi\t%s %s $%d" reg_tmp reg_tmp y;
     line oc p;
     Printf.fprintf oc "\tslt\t%s %s %s" reg_tmp reg_zero reg_tmp;
     line oc p;
     g'_tail_if oc e1 e2 "beq" reg_tmp reg_zero p
  | Tail, IfFEq(x, y, e1, e2, p) ->
     Printf.fprintf oc "\tfseq\t%s %s" x y;
     line oc p;
     g'_tail_if_float oc e1 e2 "bclt" p;
  | Tail, IfFLE(x, y, e1, e2, p) ->
     Printf.fprintf oc "\tfslt\t%s %s" y x;
     line oc p;
     g'_tail_if_float oc e2 e1 "bclt" p;
  | NonTail(z), IfEq(x, V(y), e1, e2, p) ->
     g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" x y p
  | NonTail(z), IfEq(x, C(y), e1, e2, p) ->
     Printf.fprintf oc "\taddi\t%s %s $%d" reg_tmp x (-y);
     line oc p;
     g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" reg_tmp reg_zero p
  | NonTail(z), IfLE(x, V(y), e1, e2, p) ->
     Printf.fprintf oc "\tslt\t%s %s %s" reg_tmp y x;
     line oc p;
     g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" reg_tmp reg_zero p
  | NonTail(z), IfLE(x, C(y), e1, e2, p) ->
     Printf.fprintf oc "\tsub\t%s %s %s" reg_tmp reg_zero x;
     line oc p;
     Printf.fprintf oc "\taddi\t%s %s $%d" reg_tmp reg_tmp y;
     line oc p;
     Printf.fprintf oc "\tslt\t%s %s %s" reg_tmp reg_tmp reg_zero;
     line oc p;
     g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" reg_tmp reg_zero p
  | NonTail(z), IfGE(x, V(y), e1, e2, p) ->
     Printf.fprintf oc "\tslt\t%s %s %s" reg_tmp x y;
     line oc p;
     g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" reg_tmp reg_zero p
  | NonTail(z), IfGE(x, C(y), e1, e2, p) ->
     Printf.fprintf oc "\tsub\t%s %s %s" reg_tmp reg_zero x;
     line oc p;
     Printf.fprintf oc "\taddi\t%s %s $%d" reg_tmp reg_tmp y;
     line oc p;
     Printf.fprintf oc "\tslt\t%s %s %s" reg_tmp reg_zero reg_tmp;
     line oc p;
     g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" reg_tmp reg_zero p
  | NonTail(z), IfFEq(x, y, e1, e2, p) ->
     Printf.fprintf oc "\tfseq\t%s %s" x y;
     line oc p;
     g'_non_tail_if_float oc (NonTail(z)) e1 e2 "bclt" p;
  | NonTail(z), IfFLE(x, y, e1, e2, p) ->
     Printf.fprintf oc "\tfslt\t%s %s" y x;
     line oc p;
     g'_non_tail_if_float oc (NonTail(z)) e2 e1 "bclt" p;
  (* �ؿ��ƤӽФ��β���̿��μ��� (caml2html: emit_call) *)
  | Tail, CallCls(x, ys, zs, p) -> (* �����ƤӽФ� (caml2html: emit_tailcall) *)
     g'_args oc [(x, reg_cl)] ys zs p;
     Printf.fprintf oc "\tjr\t%s" reg_cl;
     line oc p
  | Tail, CallDir(Id.L(x), ys, zs, p) -> (* �����ƤӽФ� *)
     g'_args oc [] ys zs p;
     Printf.fprintf oc "\taddi\t%s %s %s" reg_tmp reg_zero x;
     line oc p;
     Printf.fprintf oc "\tjr\t%s" reg_tmp;
     line oc p
  | NonTail(a), CallCls(x, ys, zs, p) ->
     g'_args oc [(x, reg_cl)] ys zs;
     let ss = stacksize () in
     Printf.fprintf oc "\taddi\t%s %s $%d" reg_sp reg_sp (-(ss+1));
     line oc p;
     Printf.fprintf oc "\tst\t0(%s) %s" reg_ra reg_sp;
     line oc p;
     Printf.fprintf oc "\tjalr\t%s" reg_cl;
     line oc p;
     Printf.fprintf oc "\tld\t0(%s) %s" reg_sp reg_ra;
     line oc p;
     Printf.fprintf oc "\taddi\t%s %s $%d" reg_sp reg_sp (ss+1);
     line oc p;
     if List.mem a allregs && a <> reg_rv then
       (Printf.fprintf oc "\tadd\t%s %s %s" reg_rv a reg_zero;
	line oc p)
     else if List.mem a allfregs && a <> fregs.(0) then
       (Printf.fprintf oc "\tmov.s\t%s %s" a fregs.(0);
	line oc p)
  | NonTail(a), CallDir(Id.L(x), ys, zs, p) ->
     g'_args oc [] ys zs;
     let ss = stacksize () in
     Printf.fprintf oc "\taddi\t%s %s $%d" reg_sp reg_sp (-(ss+1));
     line oc p;
     Printf.fprintf oc "\tst\t0(%s) %s" reg_ra reg_sp;
     line oc p;
     Printf.fprintf oc "\tjal\t%s" x;
     line oc p;
     Printf.fprintf oc "\tld\t0(%s) %s" reg_sp reg_ra;
     line oc p;
     Printf.fprintf oc "\taddi\t%s %s $%d" reg_sp reg_sp (ss+1);
     line oc p;
     if List.mem a allregs && a <> reg_rv then
       (Printf.fprintf oc "\tadd\t%s %s %s" reg_rv a reg_zero;
	line oc p)
     else if List.mem a allfregs && a <> fregs.(0) then
       (Printf.fprintf oc "\tmov.s\t%s %s" a fregs.(0);
	line oc p)
and g'_tail_if oc e1 e2 b reg1 reg2 p =
  let b_true = Id.genid b in
  Printf.fprintf oc "\t%s\t%s %s %s" b reg1 reg2 b_true;
  line oc p;
  let stackset_back = !stackset in
  g oc (Tail, e2);
  Printf.fprintf oc "%s:\n" b_true;
  stackset := stackset_back;
  g oc (Tail, e1);
and g'_tail_if_float oc e1 e2 b p =
  let b_true = Id.genid b in
  Printf.fprintf oc "\t%s\t%s %s %s" b b_true;
  line oc p;
  let stackset_back = !stackset in
  g oc (Tail, e2);
  Printf.fprintf oc "%s:\n" b_true;
  stackset := stackset_back;
  g oc (Tail, e1);
and g'_non_tail_if oc dest e1 e2 b reg1 reg2 p =
  let b_true = Id.genid (b ^ "_true") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "\t%s\t%s %s %s" b reg1 reg2 b_true;
  line oc p;
  let stackset_back = !stackset in
  g oc (dest, e2);
  let stackset1 = !stackset in
  Printf.fprintf oc "\taddi\t%s %s %s" reg_tmp reg_zero b_cont; (* �����С��ե��������? *)
  line oc p;
  Printf.fprintf oc "\tjr\t%s" reg_tmp;
  line oc p;
  Printf.fprintf oc "%s:\n" b_true;
  stackset := stackset_back;
  g oc (dest, e1);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_non_tail_if_float oc dest e1 e2 b p =
  let b_true = Id.genid (b ^ "_true") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "\t%s\t%s %s %s" b b_true;
  line oc p;
  let stackset_back = !stackset in
  g oc (dest, e2);
  let stackset1 = !stackset in
  Printf.fprintf oc "\taddi\t%s %s %s" reg_tmp reg_zero b_cont; (* �����С��ե��������? *)
  line oc p;
  Printf.fprintf oc "\tjr\t%s" reg_tmp;
  line oc p;
  Printf.fprintf oc "%s:\n" b_true;
  stackset := stackset_back;
  g oc (dest, e1);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_args oc x_reg_cl ys zs p =
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let sw = Printf.sprintf "%d(%s)" (stacksize ()) reg_sp in
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> (Printf.fprintf oc "\tadd\t%s %s %s" r y reg_zero; line oc p))
    (shuffle sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> (Printf.fprintf oc "\tfmov\t%s %s" fr z; line oc p))
    (shuffle sw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc ".data\n";
  List.iter
    (fun (Id.L(x), f) ->
      Printf.fprintf oc "%s:\t# %f\n" x f;
      Printf.fprintf oc "\t.long\t0x%lx\n" (getfl f))
    data;
  Printf.fprintf oc ".text\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc ".globl\tmin_caml_start\n";
  Printf.fprintf oc "min_caml_start:\n";
  Printf.fprintf oc "\taddi\t%s %s $-2\n" reg_sp reg_sp;
  Printf.fprintf oc "\tst\t1(%s) %s\n" reg_sp reg_ra;
  Printf.fprintf oc "\tst\t0(%s) %s\n" reg_sp reg_fp;
  Printf.fprintf oc "\tadd\t%s %s %s\n" reg_sp reg_fp reg_zero;
  stackset := S.empty;
  stackmap := [];
  Printf.fprintf oc "\t# Main Program Begin\n";
  g oc (NonTail(regs.(0)), e);
  Printf.fprintf oc "\t# Main Program End\n";
  Printf.fprintf oc "\tadd\t%s %s %s\n" reg_fp reg_sp reg_zero;
  Printf.fprintf oc "\tld\t0(%s) %s\n" reg_sp reg_fp;
  Printf.fprintf oc "\tld\t-1(%s) %s\n" reg_sp reg_ra;
  Printf.fprintf oc "\taddi\t%s %s $2\n" reg_sp reg_sp;
  Printf.fprintf oc "\thalt\n";