open Format

  class message s=
  object (self)
  method msg () = s
  method print (x: Printer.info) = 
    open_box 0; print_string (self#msg()); close_box ()
  end

class error s =
  object 
  inherit message s
end

class errormsg =
  object
  inherit error "Error"
end

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

let warning s =
  (Format.open_box 0;
  Format.print_string s;
  Format.print_string "\n";
  Format.close_box())

