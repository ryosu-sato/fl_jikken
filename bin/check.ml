open Util
open Assignment

let check_filename ?ext s =
  match
    match ext with
    | None -> Some s
    | Some e when String.ends_with s ("."^e) -> Some String.(sub s 0 (length s - length e - 1))
    | _ -> None
  with
  | None -> None
  | Some s ->
      let s = Filename.basename s in
      if String.length s = 9 && s.[2] = '-' then
        match int_of_string (String.sub s 0 2) with
        | n ->
            Config.no := n;
            let s' = String.sub s 3 (String.length s - 3) in
            if Seq.fold_left (fun acc c -> acc && Char.is_int_char c) true (String.to_seq s') then
              Some s'
            else
              None
        | exception Invalid_argument _ -> None
      else
        None

let check_file_organization () =
  let for_dir = Sys.is_directory !Config.file in
  let ext = if for_dir then None else Some "zip" in
  match check_filename ?ext !Config.file with
  | None -> Error (File_name_invalid !Config.file)
  | Some id ->
      Config.id := id;
      let cmd =
        if for_dir then
          Printf.sprintf "cp -r %s %s" !Config.file Config.dir
        else
          Printf.sprintf "unzip -q -d %s %s" Config.dir !Config.file
      in
      if Sys.command cmd <> 0 then
        Error Cannot_extract
      else
        let dir = Config.dir ^ "/" ^ Filename.remove_extension @@ Filename.basename !Config.file in
        Config.file_dir := dir;
        if not (Sys.file_exists dir && Sys.is_directory dir) then
          Error (Directory_not_found (Filename.remove_extension !Config.file))
        else if not @@ List.exists (fun ext -> Sys.file_exists (Printf.sprintf "%s/%s.%s" dir Config.report_name ext)) Config.report_exts then
          Error (File_not_found (Config.report_name ^ ".*"))
        else
          Ok ()

let count_leading_spaces s =
  let rec loop i c s =
    if String.length s <= i || s.[i] <> ' ' then
      c
    else
      loop (i+1) (c+1) s
  in
  loop 0 0 s

let rec normalize_output acc_rev ss =
  match ss with
  | [] -> List.rev acc_rev
  | ""::ss' -> normalize_output acc_rev ss'
  | s::ss' when String.starts_with s "Hint: " -> normalize_output acc_rev ss'
  | s::ss' when String.starts_with s "  " && acc_rev <> [] ->
      let len = count_leading_spaces s in
      let s' = List.hd acc_rev ^ String.sub s (len-1) (String.length s - len + 1) in
      let acc_rev' = s' :: List.tl acc_rev in
      normalize_output acc_rev' ss'
  | s::ss' when s.[0] = '=' && acc_rev <> [] ->
      let s' = List.hd acc_rev ^ " " ^ s in
      let acc_rev' = s' :: List.tl acc_rev in
      normalize_output acc_rev' ss'
  | s::ss' -> normalize_output (s::acc_rev) ss'

let parse_output s =
  let map =
    ["Error: Unbound value ", (fun x -> Error (Value_not_found x));
     "Error: Unbound type constructor ", (fun x -> Error (Type_not_found x));
     "Error: Unbound constructor ", (fun x -> Error (Constructor_not_found x));
     "Error: Unbound module ", (fun x -> Error (Module_not_found x));
     "Error: Signature mismatch:", (fun x -> Error (Type_mismatch x));
     "- : ", Result.ok;
     "type ", Result.ok;
     "exception ", Result.ok;
     "module ", Result.ok]
  in
  match List.find_opt (fun (p,_) -> String.starts_with s p) map with
  | None -> Error (Unknown_error s)
  | Some(prefix, f) ->
      let len = String.length prefix in
      f @@ String.sub s len (String.length s - len)

let eval_file filename x =
  let cmd = Printf.sprintf "ocaml -noprompt -color never -init %s" filename in
  let cin,cout = Unix.open_process cmd in
  output_string cout (x ^ ";;\n");
  close_out cout;
  let s,ss_rev =
    match
      Util.input_lines cin
      |> normalize_output []
      |> List.rev
    with
    | [] -> assert false
    | s::ss -> s, ss
  in
  close_in cin;
  let result = parse_output s in
  let errors =
    ss_rev
    |> List.filter (fun s -> String.starts_with s "Error: ")
    |> List.map parse_output
    |> List.map Result.get_error
  in
  match result with
  | Ok s -> Ok(s, errors)
  | Error e -> Error (e::errors)

let check_item filename ?(is_dir=Sys.is_directory filename) item =
  match item with
  | ValDef v ->
      begin
        match eval_file filename v with
        | Ok _ -> [OK None]
        | Error es -> es
      end
  | Value(x,v) ->
      begin
        match eval_file filename x with
        | Ok(v', _) when String.ends_with v' (" = " ^ v) -> [OK None]
        | Ok _ -> [Incorrect_result]
        | Error es -> es
      end
  | Type(v, ty) ->
      begin
        match eval_file filename @@ Printf.sprintf "(%s : %s)" v ty with
        | Ok _ -> [OK None]
        | Error (Type_mismatch _ :: es) -> Type_mismatch v :: es
        | Error es -> es
      end
  | TypeOpt(v, ty) ->
      begin
        match eval_file filename @@ Printf.sprintf "(%s : %s)" v ty with
        | Ok _ -> [OK (Some v)]
        | _ -> [OK None]
      end
  | TypeDef(n, ty) ->
      let param =
        assert (n >= 0);
        match n with
        | 0 -> ""
        | 1 -> "'a "
        | _ ->
            List.init n (fun i -> Printf.sprintf "'%c" @@ char_of_int (int_of_char 'a' + i))
            |> String.concat ", "
            |> Printf.sprintf "(%s) "
      in
      begin
        match eval_file filename @@ Printf.sprintf ("type %st = %s%s") param param ty with
        | Ok _ -> [OK None]
        | Error es -> es
      end
  | ModDef m ->
      begin
        match eval_file filename ("module M = "^m) with
        | Ok _ -> [OK None]
        | Error es -> es
      end
  | Module(m, ty) ->
      begin
        match eval_file filename @@ Printf.sprintf "module M = (%s : %s)" m ty with
        | Ok _ -> [OK None]
        | Error (Type_mismatch _::es) -> Type_mismatch m::es
        | Error es -> es
      end
  | Excep e ->
      begin
        match eval_file filename @@ Printf.sprintf "exception E = %s" e with
        | Ok _ -> [OK None]
        | Error es -> es
      end
  | CurryUncurry(h, f) ->
      begin
        match
          let (let*) r f = Result.map f r in
          let* _ = eval_file filename "curry" in
          let* _ = eval_file filename "uncurry" in
          let* s1 = eval_file filename @@ Printf.sprintf "%s %s" h f in
          let* s2 = eval_file filename @@ Printf.sprintf "%s (curry (uncurry %s))" h f in
          if s1 <> s2 then
            Ok("",[])
          else
            Error [Incorrect_result]
        with
        | Ok _ -> [OK None]
        | Error es -> es
      end
  | Build(main, except) ->
      assert is_dir;
      let exec =
        Option.value main ~default:!Config.executable
        |> Format.sprintf "%s/%s" filename
      in
      let prefix = Config.dir ^ "/" in
      let object_files = [".exe";".cmo";".cmx";".cma";".cmxa";".cmxs";".cmt";".cmti";".cmi";".o";".a"] in
      let result =
        let files =
          object_files
          |> List.filter_map (Files.find ~filename)
          |> List.filter (fun f -> not @@ List.mem (Filename.basename f) except)
        in
        match files with
        | file::_ ->
            Object_file_found (String.remove_prefix ~prefix file)
        | [] ->
            let r = Sys.command @@ Printf.sprintf "cd %s; %s > /dev/null 2>&1" filename !Config.build in
            if r <> 0 then
              Build_failed
            else if Sys.file_exists exec then
              OK None
            else
              File_not_found_after_build (String.remove_prefix ~prefix exec)
      in
      [result]
  | Exec tests ->
      let exec = Format.sprintf "%s/%s" filename !Config.executable in
      if not @@ Sys.file_exists exec then
        [OK None] (* Bulid must be checked before this check *)
      else
        let cin,cout,cerr = Unix.open_process_full exec [||] in
        let run acc (input,expected) =
          acc &&
            (output_string cout input;
             output_string cout "\n";
             Util.debug "  input: %S@." input;
             flush cout;
             let output =
               input_line cin
               |> String.trim
             in
             Util.debug "  output:   %S@." output;
             Util.debug "  expected: %S@." expected;
             output = String.trim expected)
        in
        let r =
          match List.fold_left run true tests with
          | true -> OK None
          | false | exception _ -> Incorrect_result
        in
        Util.debug "@.";
        ignore @@ Unix.close_process_full (cin,cout,cerr);
        [r]

let check_file t items =
  let is_dir = is_directory t in
  let filename = !Config.file_dir ^ "/" ^ filename_of t in
  Util.debug "Check %s@." @@ subject_of t;
  if not @@ Sys.file_exists filename then
    let path = Printf.sprintf "%02d-%s/%s" !Config.no !Config.id (filename_of t) in
    [if is_dir then Directory_not_found path else File_not_found path]
  else
    items
    |> List.concat_map (check_item ~is_dir filename)


let file_organization = check_file_organization
let file = check_file
