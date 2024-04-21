type 'a iterator = < get : 'a ; has_value : bool ; next : unit >

class ['a] list_iterator init =
  object
    val mutable current : 'a list = init
    method has_value = not (List.is_empty current)

    method get =
      match current with
      | hd :: _tl -> hd
      | [] -> raise (Invalid_argument "no value")

    method next =
      match current with
      | _hd :: tl -> current <- tl
      | [] -> raise (Invalid_argument "no value")
  end

class ['a] istack init =
  object
    val mutable v : 'a list = init

    method pop =
      match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None

    method v = v
    method push hd = v <- hd :: v
    method iterator : 'a iterator = new list_iterator v
  end

let () =
  Printf.printf "\n";

  let s = new istack [ 4; 2; 0 ] in

  s#push 69;
  let _ = s#pop in
  let _ = s#pop in

  let it = s#iterator in
  let _ = it#get in

  it#next;
  let value = it#get in
  print_int value;

  it#next;

  let string_list = List.map (fun x -> Int.to_string x) s#v in
  let formated = String.concat "; " string_list in
  Printf.printf "[%s]\n" formated
