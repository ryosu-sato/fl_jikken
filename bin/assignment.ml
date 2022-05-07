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
  | Unsupported_week_no of int
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
  | Unsupported_week_no n, true -> Printf.sprintf "第%d週の課題はサポートしていません" n
  | Unsupported_week_no n, false -> Printf.sprintf "Not support: Week %d" n
  | Unknown_error s, true -> Printf.sprintf "不明なエラー (%s)" s
  | Unknown_error s, false -> Printf.sprintf "Unknown error (%s)" s
