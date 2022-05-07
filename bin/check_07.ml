type t =
  | Toi of int
  | ToiDir of int
  | Hatten of int
  | HattenDir of int

type item =
  | ValDef of string (* Check the existence of a value *)
  | Value of string * string (* Check the value of an expression *)
  | Type of string * string (* Check the type of an expression *)
  | TypeOpt of string * string (* Check the type of an expression for optional problems *)
  | TypeDef of int * string (* Check the existence of a type *)
  | ModDef of string (* Check the existence of a module *)
  | Module of string * string (* Check the type of a module *)
  | Excep of string (* Check the existence of an exception *)
  | CurryUncurry of string * string (* Just for Hatten 3 of the second lecture *)
  | Build of string option * string list (* Check the buildability *)
  | Exec of (string * string) list (* Check the behavior of the main. "Exec" must follow "Build" *)

module Config = struct
  let no = 7
  let version = "4.13.1"
  let dir = "_fl_tmp_" ^ Util.time()
  let file_dir = ref ""
  let id = ref ""
  let force = ref false
  let jp = ref true
  let file = ref ""
  let for_dir = ref false
  let build = ref "dune build"
  let executable = ref "main.exe"
end

let may_be_included =
  ["constraintSolver.cmi";
   "constraintSolver.cmo";
   "tySyntax.cmi";
   "tySyntax.cmo"]
let assiginments =
  [ToiDir 1, [Build(None, may_be_included)]]


let report_name = "report"
let report_exts = ["txt"; "md"; "pdf"]

type 'a result =
  | OK of 'a
  | Version_mismatch
  | Cannot_extract
  | File_name_invalid of string
  | Directory_not_found of string
  | File_not_found of string
  | File_not_found_after_build of string
  | Value_not_found of string
  | Type_mismatch of string
  | Type_not_found of string
  | Constructor_not_found of string
  | Module_not_found of string
  | Incorrect_result
  | Uncaught_exception
  | Object_file_found of string
  | Build_failed
  | Unknown_error of string

let is_directory = function
  | Toi _ | Hatten _ -> false
  | ToiDir _ | HattenDir _ -> true

let filename_of = function
  | Toi n -> Printf.sprintf "toi%d.ml" n
  | ToiDir n -> Printf.sprintf "toi%d" n
  | Hatten n -> Printf.sprintf "hatten%d.ml" n
  | HattenDir n -> Printf.sprintf "hatten%d" n

let subject_of t =
  match t, !Config.jp with
  | (Toi n | ToiDir n), true -> "問" ^ string_of_int n
  | (Toi n | ToiDir n), false -> "Toi " ^ string_of_int n
  | (Hatten n | HattenDir n), true -> "発展" ^ string_of_int n
  | (Hatten n | HattenDir n), false -> "Hatten " ^ string_of_int n

let message_of r =
  match r, !Config.jp with
  | OK None, _ -> ""
  | OK (Some s), _ -> s
  | Version_mismatch, true -> Printf.sprintf "OCaml %s で実行してください" Config.version
  | Version_mismatch, false -> Printf.sprintf "Execute this program with OCaml %s." Config.version
  | Cannot_extract, true -> Printf.sprintf "入力ファイルの展開に失敗しました"
  | Cannot_extract, false -> Printf.sprintf "Cannot extract the input file"
  | File_name_invalid f, true -> Printf.sprintf "ファイル名 %sが不正です" f
  | File_name_invalid f, false -> Printf.sprintf "Filename %s invalid" f
  | Directory_not_found f, true -> Printf.sprintf "ディレクトリ %s が見つかりません" f
  | Directory_not_found f, false -> Printf.sprintf "Directory %s not found" f
  | File_not_found f, true -> Printf.sprintf "ファイル %s が見つかりません" f
  | File_not_found f, false -> Printf.sprintf "File %s not found" f
  | File_not_found_after_build f, true -> Printf.sprintf "ビルド後にファイル %s が見つかりません" f
  | File_not_found_after_build f, false -> Printf.sprintf "File %s not found after build" f
  | Type_mismatch v, true -> Printf.sprintf "%s の型が合っていません" v
  | Type_mismatch v, false -> Printf.sprintf "Type of %s is mismatched" v
  | (Value_not_found x | Type_not_found x | Constructor_not_found x | Module_not_found x), true -> Printf.sprintf "%s が見つかりません" x
  | (Value_not_found x | Type_not_found x | Constructor_not_found x | Module_not_found x), false -> Printf.sprintf "%s not found" x
  | Incorrect_result, true -> Printf.sprintf "結果が正しくありません"
  | Incorrect_result, false -> Printf.sprintf "Incorrect result"
  | Uncaught_exception, true -> Printf.sprintf "例外が発生しました"
  | Uncaught_exception, false -> Printf.sprintf "Uncaught exception occurred"
  | Object_file_found s, true -> Printf.sprintf "%s を消してください" s
  | Object_file_found s, false -> Printf.sprintf "Remove %s" s
  | Build_failed, true -> Printf.sprintf "ビルドに失敗しました"
  | Build_failed, false -> Printf.sprintf "Build failed"
  | Unknown_error s, true -> Printf.sprintf "不明なエラー (%s)" s
  | Unknown_error s, false -> Printf.sprintf "Unknown error (%s)" s

module Args = struct
  let options =
    ["-f", Arg.Set Config.force, "";
     "-e", Arg.Clear Config.jp, "";
     "-d", Arg.Set Config.for_dir, "";
     "-b", Arg.Set_string Config.build, ""]

  let set_file filename =
    if !Config.file <> "" then
      begin
        if !Config.jp then
          Printf.printf "ファイル引数は一つまでです\n"
        else
          Printf.printf "Only one file argument is allowed.\n";
        exit 1
      end;
    Config.file := filename

  let usage = Printf.sprintf "Usage: ocaml <stdlib>/unix.cma check%02d.ml %02d-XXXXXX.zip" Config.no Config.no

  let parse () = Arg.parse options set_file usage
end

let init () =
  Args.parse();
  if not !Config.force && Sys.ocaml_version <> Config.version then
    Error Version_mismatch
  else
    begin
      if not @@ Sys.file_exists Config.dir then Sys.mkdir Config.dir 0o755;
      Ok ()
    end

let finalize () =
  Util.remove_rec Config.dir

let show_error_and_exit = function
  | Ok _ -> ()
  | Error e ->
      Printf.printf "%s\n" (message_of e);
      finalize ();
      exit 1

let check_filename ?ext s =
  match
    match ext with
    | None -> Some s
    | Some e when Util.ends_with s ("."^e) -> Some String.(sub s 0 (length s - length e - 1))
    | _ -> None
  with
  | None -> None
  | Some s ->
      let s = Filename.basename s in
      if String.length s = 9 && Util.starts_with s (Printf.sprintf "%02d-" Config.no) then
        let s' = String.sub s 3 (String.length s - 3) in
        if Seq.fold_left (fun acc c -> acc && Util.is_int_char c) true (String.to_seq s') then
          Some s'
        else
          None
      else
        None

let check_file_organization () =
  let ext = if !Config.for_dir then None else Some "zip" in
  match check_filename ?ext !Config.file with
  | None -> Error (File_name_invalid !Config.file)
  | Some id ->
      Config.id := id;
      let cmd =
        if !Config.for_dir then
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
        else if not @@ List.exists (fun ext -> Sys.file_exists (Printf.sprintf "%s/%s.%s" dir report_name ext)) report_exts then
          Error (File_not_found (report_name ^ ".*"))
        else
          Ok ()

let rec normalize_output acc_rev ss =
  match ss with
  | [] -> List.rev acc_rev
  | ""::ss' -> normalize_output acc_rev ss'
  | s::ss' when Util.starts_with s "Hint: " -> normalize_output acc_rev ss'
  | s::ss' when Util.starts_with s "    " && acc_rev <> [] -> (* TODO: merge & genereralize with the next case *)
      let s' = List.hd acc_rev ^ String.sub s 3 (String.length s - 3) in
      let acc_rev' = s' :: List.tl acc_rev in
      normalize_output acc_rev' ss'
  | s::ss' when Util.starts_with s "  " && acc_rev <> [] ->
      let s' = List.hd acc_rev ^ String.sub s 1 (String.length s - 1) in
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
  match List.find_opt (fun (p,_) -> Util.starts_with s p) map with
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
    |> List.filter (fun s -> Util.starts_with s "Error: ")
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
        | Ok(v', _) when Util.ends_with v' (" = " ^ v) -> [OK None]
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
          |> List.filter_map (Util.find ~filename)
          |> List.filter (fun f -> not @@ List.mem (Filename.basename f) except)
        in
        match files with
        | file::_ ->
            Object_file_found (Util.remove_prefix ~prefix file)
        | [] ->
            let r = Sys.command @@ Printf.sprintf "cd %s; %s > /dev/null 2>&1" filename !Config.build in
            if r <> 0 then
              Build_failed
            else if Sys.file_exists exec then
              OK None
            else
              File_not_found_after_build (Util.remove_prefix ~prefix exec)
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
    let path = Printf.sprintf "%02d-%s/%s" Config.no !Config.id (filename_of t) in
    [if is_dir then Directory_not_found path else File_not_found path]
  else
    items
    |> List.concat_map (check_item ~is_dir filename)

let show_results (t, items, result) =
  Printf.printf "[%s] " (subject_of t);
  if List.for_all (function OK _ -> true | _ -> false) result then
    let r = List.filter_map (function OK s -> s | _ -> None) result in
    let is_opt = items <> [] && List.for_all (function TypeOpt _ -> true | _ -> false) items in
    match r, is_opt, !Config.jp with
    | [], true, true -> Printf.printf "NG: 答えが見つかりません"
    | [], true, false -> Printf.printf "NG: No solution found"
    | [], false, _ -> Printf.printf "OK"
    | _ -> Printf.printf "%s" (String.concat ", " r)
  else
    result
    |> List.filter (function OK _ -> false | _ -> true)
    |> List.map message_of
    |> Util.unique
    |> String.concat ", "
    |> Printf.printf "NG: %s";
  Printf.printf "\n"

let main () =
  init()
  |> show_error_and_exit;

  if !Config.file = "" then (Printf.printf "%s\n" Args.usage; exit 1);

  check_file_organization()
  |> show_error_and_exit;

  assiginments
  |> List.map (fun (t,items) -> t, items, check_file t items)
  |> List.iter show_results;

  finalize()

let () = if not !Sys.interactive then main()
