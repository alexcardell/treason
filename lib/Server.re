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

let start = () => {
  let%lwt ch = exec("ls");
  Lwt_io.print(ch);
};
