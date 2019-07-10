open Lwt;
open Lwt.Infix;

let smoke = () => true;

let bin = "tmux";

let parseStreamUntil = (cmd, stream) =>
  stream
  |> Lwt_stream.get_while(x => !String.equal(cmd, x))
  >>= (
    ls =>
      List.fold_left(
        (acc, l) => acc ++ "\n" ++ l,
        List.hd(ls),
        List.tl(ls),
      )
      |> Lwt_io.printl
  );

let start = () => {
  let proc = Lwt_process.shell("tmux -C") |> Lwt_process.open_process_full;

  let cmd = "ls";

  cmd
  |> Lwt_io.write_line(proc#stdin)
  >>= (_ => Lwt_io.flush(proc#stdin))
  >>= (_ => Lwt_io.read_lines(proc#stdout) |> Lwt.return)
  >>= (stream => parseStreamUntil(cmd) |> Lwt_stream.parse(stream));
  /* >>= ((stream) => Lwt_stream.parse(stream, parseStream)) */
  /* >>= (v => Lwt_io.printl(v)) */
  /* >>= (() => Lwt_io.read_line(proc#stdout)) */
  /* >>= (str => Lwt_io.printl(str)); */
};
