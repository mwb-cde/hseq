
  open Basic
  open Gtypes

  type binder = {quant: quant_ty; qvar: string; qtyp: stype}

  type dbterm =
      Var of Basic.fnident * stype
    | Qnt of binder * dbterm
    | Bound of int
    | App of dbterm * dbterm
    | Const of Basic.const_ty

  let mk_binder q v t = {quant=q; qvar=v; qtyp=t}

  let rec of_term_aux env qnts t= 
    match t with
      Term.Var(n, ty) -> Var(n, (Gtypes.to_save_env env ty))
    | Term.Const(c) -> Const(c)
    | Term.App(f, a) -> App(of_term_aux env qnts f, of_term_aux env qnts a)
    | Term.Typed (trm, ty) -> of_term_aux env qnts trm
    | Term.Bound(q) ->
	Bound(Lib.index (fun x -> (x==q)) qnts)
    | Term.Qnt(q, b) -> 
	let (tqnt, tqvar, tqtyp) = Term.dest_binding q
	and nb = of_term_aux env (q::qnts) b
	in 
	Qnt(mk_binder tqnt tqvar (Gtypes.to_save_env env tqtyp), nb)
	  
  let of_term t = of_term_aux (ref []) [] t

  let rec to_term_aux env qnts t =
    match t with
      Var(n, ty) -> Term.Var(n, Gtypes.from_save_env env ty)
    | Const(c) -> Term.Const(c)
    | App(f, a) -> Term.App(to_term_aux env qnts f, to_term_aux env qnts a)
    | Bound(q) -> Term.Bound(List.nth qnts q)
    | Qnt(q, b) ->
	let nq = 
	  Term.mk_binding q.quant q.qvar (Gtypes.from_save_env env q.qtyp)
	in 
	Term.Qnt(nq, to_term_aux env (nq::qnts) b)

  let to_term t = to_term_aux (ref[]) [] t

  let output oc d = 
    output_value oc d 

  let input ic =  ((input_value ic):dbterm)


