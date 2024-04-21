open Base

class ['a] stack init =
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
    method iter f = List.iter ~f v
  end

class ['a] sstack init =
  object
    inherit ['a] stack init

    method print =
      List.iter ~f:(Stdio.printf "%d, ") v;
      Stdlib.print_string "\n"
  end

class double_stack init =
  object
    inherit [int] sstack init as super
    method! push hd = super#push (hd * 2)
  end
