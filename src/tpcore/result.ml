open Format

type kind = int
let kind_ctr = ref 0

let mk_new_kind () = 
  let tmp = !kind_ctr
  in 
  kind_ctr:=(!kind_ctr)+1;
  tmp

let errorkind = mk_new_kind()

class message s=
  object (self)
    method msg () = s
    method print (x: Printer.ppinfo) = 
      open_box 0; print_string (self#msg()); close_box ()
  end

class error s =
  object 
    inherit message s
    val kind = errorkind
  end

class errormsg =
  object
    inherit error "Error"
  end

(*
exception Error of error list

let mkError e = Error[(e:>error)]

let addError e x = 
  match x with 
    (Error es) ->  (Error ((e:>error)::es))
  | _ -> raise x
let raiseError s = raise (mkError (new error s))

let catchError e x = 
  match x with 
    (Error es) ->  raise (Error ((e:>error)::es))
  | _ -> raise x

let raiseError s = raise (mkError (new error s))

let print_error st dpth e = 
  let rec print_aux es d=
    if d = 0 then ()
    else 
      (match es with 
	[] -> ()
      |	(s::ds) -> 
	  open_box 0;
	  ((s:>error)#print st); 
	  print_newline(); print_aux ds (d-1));
    close_box()
  in 
  match e with 
    (Error(ers)) -> (print_aux ers dpth; ()) 
  | _ -> ()

*)


(* Alternative error messages *)

exception Error of error
exception Errors of exn list

let mk_error e = Error e
let error s = Error (new error s)

let add_error e x=
  match x with
    Errors es -> Errors((Error e)::es)
  | _ -> Errors[(Error e); x]

(*
let catchError e x =  raise (addError e x)
*)

let print_error info depth errs=
  let ctr=ref (depth+1)
  in 
  let rec print_aux x=
    if (!ctr)=1 then ()
    else 
      (ctr:=(!ctr)-1;
       match x with
      (Error e) -> 
	Format.open_box 0;
	e#print info;
	Format.close_box ()
    | (Errors l) -> 
	List.iter print_aux l;
    | _ -> 
	Format.open_box 0;
	Format.print_string (Printexc.to_string x);
	Format.close_box())
  in 
  List.iter print_aux [errs]


let catch_error info depth f a = 
  try (f a)
  with x -> print_error info depth x

let warning s =
  (Format.open_box 0;
   Format.print_string s;
   Format.print_string "\n";
   Format.close_box())
