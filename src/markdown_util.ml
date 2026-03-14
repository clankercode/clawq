let is_table_row line =
  let s = String.trim line in
  String.length s >= 3 && s.[0] = '|'

let is_separator_row line =
  let s = String.trim line in
  if String.length s < 5 || s.[0] <> '|' then false
  else
    let inner =
      String.sub s 1
        (String.length s - if s.[String.length s - 1] = '|' then 2 else 1)
    in
    let cells = String.split_on_char '|' inner in
    cells <> []
    && List.for_all
         (fun cell ->
           let c = String.trim cell in
           c <> ""
           && String.length c >= 1
           &&
           let has_dash = ref false in
           let ok = ref true in
           String.iter
             (fun ch ->
               match ch with
               | '-' -> has_dash := true
               | ':' | ' ' -> ()
               | _ -> ok := false)
             c;
           !has_dash && !ok)
         cells

let col_count line =
  let s = String.trim line in
  let inner =
    if String.length s >= 2 && s.[0] = '|' then
      let end_pos =
        if s.[String.length s - 1] = '|' then String.length s - 1
        else String.length s
      in
      String.sub s 1 (end_pos - 1)
    else s
  in
  List.length (String.split_on_char '|' inner)

let ensure_trailing_pipe line =
  let s = String.trim line in
  if String.length s = 0 then line
  else if s.[String.length s - 1] = '|' then line
  else line ^ " |"

let make_separator n =
  if n <= 0 then "| --- |"
  else "| " ^ String.concat " | " (List.init n (fun _ -> "---")) ^ " |"

let is_fence_line line =
  let s = String.trim line in
  String.length s >= 3
  && ((s.[0] = '`' && s.[1] = '`' && s.[2] = '`')
     || (s.[0] = '~' && s.[1] = '~' && s.[2] = '~'))

let normalize_tables text =
  if not (String.contains text '|') then text
  else
    let lines = String.split_on_char '\n' text in
    let out = Buffer.create (String.length text + 64) in
    let in_fence = ref false in
    let in_table = ref false in
    let prev_was_blank = ref true in
    let header_pending = ref false in
    let header_cols = ref 0 in
    let first_line = ref true in
    let add_line s =
      if not !first_line then Buffer.add_char out '\n';
      Buffer.add_string out s;
      first_line := false;
      prev_was_blank := String.trim s = ""
    in
    let add_blank_if_needed () = if not !prev_was_blank then add_line "" in
    List.iter
      (fun line ->
        if !in_fence then begin
          if is_fence_line line then in_fence := false;
          add_line line
        end
        else if is_fence_line line then begin
          if !in_table then begin
            in_table := false;
            header_pending := false;
            add_blank_if_needed ()
          end;
          in_fence := true;
          add_line line
        end
        else if is_table_row line then begin
          let line = ensure_trailing_pipe line in
          if not !in_table then begin
            in_table := true;
            add_blank_if_needed ();
            header_pending := true;
            header_cols := col_count line;
            add_line line
          end
          else if !header_pending then begin
            if is_separator_row line then begin
              header_pending := false;
              add_line line
            end
            else begin
              add_line (make_separator !header_cols);
              header_pending := false;
              add_line line
            end
          end
          else add_line line
        end
        else begin
          if !in_table then begin
            in_table := false;
            header_pending := false;
            if String.trim line <> "" then add_blank_if_needed ()
          end;
          add_line line
        end)
      lines;
    Buffer.contents out
