  open Gtypes

  type binder = {quant: Basic.quant_ty; qvar: string; qtyp: stype}

  type dbterm =
      Id of Basic.ident * stype
    | Qnt of Basic.quant_ty * binder * dbterm
    | Bound of int
    | App of dbterm * dbterm
    | Const of Basic.const_ty

  let mk_binder q v t = {quant=q; qvar=v; qtyp=t}

  let rec of_term_aux env qnts t= 
    match t with
      Basic.Id(n, ty) -> Id(n, (Gtypes.to_save_env env ty))
    | Basic.Const(c) -> Const(c)
    | Basic.App(f, a) -> App(of_term_aux env qnts f, of_term_aux env qnts a)
    | Basic.Typed (trm, ty) -> of_term_aux env qnts trm
    | Basic.Bound(q) ->
	Bound(Lib.index (fun x -> (x==q)) qnts)
    | Basic.Qnt(tqnt, q, b) -> 
	let (_, tqvar, tqtyp) = Basic.dest_binding q
	and nb = of_term_aux env (q::qnts) b
	in 
	Qnt(tqnt, mk_binder tqnt tqvar (Gtypes.to_save_env env tqtyp), nb)
	  
  let of_term t = of_term_aux (ref []) [] t

  let rec to_term_aux env qnts t =
    match t with
      Id(n, ty) -> Basic.Id(n, Gtypes.from_save_env env ty)
    | Const(c) -> Basic.Const(c)
    | App(f, a) -> Basic.App(to_term_aux env qnts f, to_term_aux env qnts a)
    | Bound(q) -> Basic.Bound(List.nth qnts q)
    | Qnt(qnt, q, b) ->
	let nq = 
	  Basic.mk_binding qnt 
	    (q.qvar) (Gtypes.from_save_env env q.qtyp)
	in 
	Basic.Qnt(qnt, nq, to_term_aux env (nq::qnts) b)

  let to_term t = to_term_aux (ref[]) [] t

  let output oc d = 
    output_value oc d 

  let input ic =  ((input_value ic):dbterm)


