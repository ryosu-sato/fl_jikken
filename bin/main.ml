open Util
open Assignment

let init () =
  Command_line.parse();
  if not !Config.force && Sys.ocaml_version <> Config.version then
    Error Version_mismatch
  else
    begin
      if not @@ Sys.file_exists Config.dir then Sys.mkdir Config.dir 0o755;
      Ok ()
    end

let finalize () =
  if Sys.file_exists Config.dir then
    Files.remove_rec Config.dir

let show_error_and_exit = function
  | Ok _ -> ()
  | Error e ->
      Printf.printf "%s\n" (message_of e);
      finalize ();
      exit 1

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
    |> List.unique
    |> String.concat ", "
    |> Printf.printf "NG: %s";
  Printf.printf "\n"

let assiginments =
  [1, Week01.assignments;
   2, Week02.assignments;
   3, Week03.assignments;
   4, Week04.assignments;
   5, Week05.assignments;
   6, Week06.assignments;
   7, Week07.assignments]

let assoc_assignments () =
  try
    List.assoc !Config.no assiginments
  with Not_found ->
         show_error_and_exit (Error (Unsupported_week_no !Config.no));
         assert false

let main () =
  init()
  |> show_error_and_exit;

  if !Config.file = "" then (Printf.printf "%s\n" Command_line.usage; exit 1);

  Check.file_organization()
  |> show_error_and_exit;

  assoc_assignments()
  |> List.map (fun (t,items) -> t, items, Check.file t items)
  |> List.iter show_results;

  finalize()

let () = if not !Sys.interactive then main()
