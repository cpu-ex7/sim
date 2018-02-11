let label : Label.t ref = ref []

let find_label str =
  try
    List.assoc str !label
  with Not_found ->
    failwith ("parser: label definition not found: " ^ str)
