(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====             Stream type & functions             ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

type 'a stream =
  | Empty
  | More of 'a * (unit -> 'a stream)

(* take_n s n builds a list from the n next elements in the stream s, and
   returns this list with the remaining stream.
*)
let take_n s n =
  let rec f k acc = function
    | Empty -> (List.rev acc, Empty)
    | More (a, g) as s ->
      if k = n then (List.rev acc, s) else f (k+1) (a :: acc) (g())
  in
  f 0 [] s

(* take_line_char s takes all the characters of the char stream s until
   it finds a newline character ('\n'). It returns the list of these
   characters with the remaining stream.
*)
let take_line_char s =
  let rec f acc = function
    | Empty -> (List.rev acc, Empty)
    | More (a, g) as s ->
      if a = '\n' then (List.rev acc, s) else f (a :: acc) (g())
  in
  f [] s

(* take_while_rgx r s takes all the characters in the char stream s while the
   whole taken string matches the regexp r. It returns the string and the
   remaining stream.
*)
let take_while_rgx r s =
  let rec f acc = function
    | Empty -> (acc, Empty)
    | More (a, g) as s ->
      try
        let nacc = acc ^ (String.make 1 a) in
        if Str.string_match r nacc 0 && Str.matched_string nacc = nacc then
          f nacc (g())
        else (acc, s)
      with Not_found -> (acc, s)
  in
  f "" s

let rec stream_of_string s =
  let f () = stream_of_string (String.sub s 1 (String.length s - 1)) in
  if s = "" then Empty else More (s.[0], f)

let rec stream_of_list = function
  | [] -> Empty
  | e :: r -> More (e, fun () -> stream_of_list r)

(* Builds a one-element stream. *)
let one e = More (e, fun () -> Empty)

(* make_stream e s makes a stream with e as the first element, followed by
   the stream s.
*)
let make_stream e s = More (e, fun () -> s)

(* dump s takes all the remaining elements in the stream s and returns a
   list of these elements.
*)
let dump s =
  let rec f acc = function
    | Empty -> List.rev acc
    | More (a, g) -> f (a :: acc) (g())
  in
  f [] s

(* append s1 s2 builds a stream by concatenation of streams s1 and s2. *)
let rec append s1 s2 =
  match s1 with
  | Empty -> s2
  | More (a, g) -> More (a, fun () -> append (g()) s2)

(* Works like List.filter for streams. *)
let rec filter_stream p = function
  | Empty -> Empty
  | More (a, g) ->
    if p a then
      More(a, fun () -> filter_stream p (g()))
    else filter_stream p (g()) 