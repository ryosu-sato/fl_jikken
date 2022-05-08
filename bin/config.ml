let no = ref 0
let version = "4.13.1"
let dir = "_fl_tmp_" ^ Util.time()
let file_dir = ref ""
let id = ref ""
let force = ref false
let jp = ref true
let file = ref ""
let build = ref "dune build"
let executable = ref "main.exe"

type mode = Check | Print_file_struct of int
let mode = ref Check

let report_name = "report"
let report_exts = ["txt"; "md"; "pdf"]
