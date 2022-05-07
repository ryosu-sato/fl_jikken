let no = ref 0
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

let report_name = "report"
let report_exts = ["txt"; "md"; "pdf"]
