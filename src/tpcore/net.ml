

  type term_label = 
      Var | Bound | App | Exist | All | Lam 
    | Const of Basic.const_ty 
    | Cname of Basic.fnident

  type 'a net = 
      Tip of 'a list
    |  Node of (term_label * 'a net) list

  let rec var_label_of varp trm =
    if varp trm then Var 
    else 
    match trm with
      Term.Bound _ -> Bound
    | Term.Qnt _ -> 
	if Logicterm.is_all trm then All
	else if Logicterm.is_exists trm then Exist
	else Lam
    | Term.App _ -> App
    | Term.Typed(t, _) -> var_label_of varp t
    | Term.Const(c) -> Const c
    | Term.Var(v, _) -> Cname(v)

  let label_of trm = var_label_of (fun x-> false) trm

  let empty() = (Node [])
  let is_empty n = 
    match n with Node [] -> true | _ -> false

  let get_edge label n = 
    match n with
      (Node edges) ->
	(try let  net = (List.assoc label edges) in net
	with Not_found -> empty())
    | (Tip _) -> Result.raiseError ("get_edge: tips have no edges")

(* itlist = List.fold_right *)

  let rec follow tm net = 
    let label= label_of tm
    in 
    let nets = 
      match label with 
      	Var -> []
      |	Lam -> follow (Term.get_qnt_body tm) (get_edge Lam net)
      |	All -> follow (Term.get_qnt_body tm) (get_edge All net)
      |	Exist -> follow (Term.get_qnt_body tm) (get_edge Exist net)
      |	App -> let (l, r)=Term.dest_app tm
	       in (List.fold_right 
		     (fun i a ->  List.append (follow l i) a)
		     (follow r (get_edge App net)) [])
      |	_ -> [get_edge label net]
    in Lib.filter (is_empty) ((get_edge Var net)::nets)

  let lookup tm net =
    List.fold_right 
      (fun x a ->
      match x with Tip l -> List.append l a | Node _ -> a)
      (follow tm net) []

  let rec overwrite (a, b) xs = 
      match xs with 
	[] -> [(a, b)]
      |	(x, y)::rst -> 
	  if (x=a) 
	  then ((a, b)::rst) 
	  else ((x, y)::(overwrite (a, b) rst))

  let get_tip_list x =
    match x with 
      Tip l -> l
    | Node _ -> []

  let rec net_update varp elem defd tm net =
      match net with
	Tip _ -> Result.raiseError "net_update: cannot update a tip"
      |	Node(edges) ->
	  (let exec_defd l n =
	    (match l with 
	      [] -> Tip (elem :: (get_tip_list n))
	    | (h::rst) -> net_update varp elem rst h n)
	  and label() = var_label_of varp tm
	  in let child() = get_edge (label()) net
	  in 
	  let new_child ()=
	    (match label() with 
	      App -> 
		(let (l, r)=Term.dest_app tm
		in net_update varp elem (l::defd) r (child()))
	    | Lam -> net_update varp elem defd (Term.get_qnt_body tm) (child())
	    | All -> net_update varp elem defd (Term.get_qnt_body tm) (child())
	    | Exist -> 
		net_update varp elem defd (Term.get_qnt_body tm) (child())
	    | _ -> exec_defd defd (child()))
	  in Node(overwrite (label(), new_child()) edges))

let enter varp (tm, elem) net = net_update varp elem [] tm net


(* ordered insert into a list *)

let cons_ord lt t ls =
  let rec cons_aux ys =
    match ys with
      [] -> [t]
    | (y::yys) -> 
	if (lt t y)
	then (t::ys) 
	else (y::(cons_aux yys))
  in cons_aux ls

let rec net_update_ord lt varp elem defd tm net =
  match net with
    Tip _ -> Result.raiseError "net_update_ord: cannot update a tip"
  | Node(edges) ->
      (let exec_defd l n =
	(match l with 
	  [] -> Tip (cons_ord lt elem (get_tip_list n))
	| (h::rst) -> net_update_ord lt varp elem rst h n)
      and label() = var_label_of varp tm
      in let child() = get_edge (label()) net
      in 
      let new_child ()=
	(match label() with 
	  App -> 
	    (let (l, r)=Term.dest_app tm
	    in net_update_ord lt varp elem (l::defd) r (child()))
	| Lam -> 
	    net_update_ord lt varp elem defd 
	      (Term.get_qnt_body tm) (child())
	| All -> 
	    net_update_ord lt varp elem defd 
	      (Term.get_qnt_body tm) (child())
	| Exist -> 
	    net_update_ord lt varp elem defd 
	      (Term.get_qnt_body tm) (child())
	| _ -> exec_defd defd (child()))
      in Node(overwrite (label(), new_child()) edges))
	
let insert lt varp (tm, elem) net = 
  net_update_ord lt varp elem [] tm net
