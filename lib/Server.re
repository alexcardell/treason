open Lwt.Infix;

let smoke = () => true;

let bin = "tmux";

let exec = cmd => {
  let args =
    cmd
    |> String.split_on_char(' ')
    |> Array.of_list
    |> Array.append([|bin, "-C"|]);
  let proc = (bin, args);
  Lwt_process.pread(proc);
};

let parseStream = stream =>
  stream
  |> Lwt_stream.get_available
  |> List.fold_left((ls, acc) => acc ++ ls, "alexxxx")
  |> Lwt_io.print;

let start = () => {
  open Lwt;
  let proc =
    Lwt_process.shell("tmux -C new-session -s treason-test")
    |> Lwt_process.open_process_full;
  Lwt_io.read_lines(proc#stdout) |> Lwt_stream.parse(_, parseStream);
  /* >>= ((stream) => Lwt_stream.parse(stream, parseStream)) */
  /* >>= (v => Lwt_io.printl(v)) */
  /* >>= (() => Lwt_io.read_line(proc#stdout)) */
  /* >>= (str => Lwt_io.printl(str)); */
};
