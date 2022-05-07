let options =
  ["-f", Arg.Set Config.force, "";
   "-e", Arg.Clear Config.jp, "";
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

let usage = Printf.sprintf "Usage: fl_jikken XX-YYYYYY.zip"

let parse () = Arg.parse options set_file usage
