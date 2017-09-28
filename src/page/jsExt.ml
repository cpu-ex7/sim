include Js

external set : 'a -> string -> 'b -> unit = "" [@@bs.set_index]
external get : 'a -> string -> 'b = "" [@@bs.get_index]

external document : Dom.document = "" [@@bs.val]
external document_body : Dom.element = "document.body" [@@bs.val]
external current_script : Dom.element = "document.currentScript" [@@bs.val]

external appendChild : Dom.element -> Dom.element -> unit = "" [@@bs.send]
external insertBefore : Dom.element -> Dom.element -> Dom.element -> unit  = "" [@@bs.send]

let insert_before place elem =
  insertBefore (get place "parentNode") elem place

external createElement : Dom.document -> string -> Dom.element = "" [@@bs.send]
external getElementById : Dom.document -> string -> Dom.element option =  "" [@@bs.return null_to_opt] [@@bs.send]

type canvas_context
external getContext : Dom.element -> string -> canvas_context = "" [@@bs.send]
external clearRect : canvas_context -> int -> int -> int -> int -> unit = "" [@@bs.send]
external beginPath : canvas_context -> unit = "" [@@bs.send]
external stroke : canvas_context -> unit = "" [@@bs.send]
external moveTo : canvas_context -> int -> int -> unit = "" [@@bs.send]
external lineTo : canvas_context -> int -> int -> unit = "" [@@bs.send]
external fillRect : canvas_context -> int -> int -> int -> int -> unit = "" [@@bs.send]
external arc : canvas_context -> int -> int -> int -> float -> float -> Js.boolean -> unit = "" [@@bs.send]
