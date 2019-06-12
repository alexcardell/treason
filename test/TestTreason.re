include ServerTest;
include ParserTest;

let ci =
  Sys.getenv_opt("CI") |> Option.default("false") |> bool_of_string_opt;

let x =
  TestFramework.run(
    {
      open Rely.RunConfig;
      let config = x => {
        let ciReporters = [Default, JUnit("reports/rely.junit.xml")];
        switch (ci) {
        | Some(true) => x |> ciMode(true) |> withReporters(ciReporters)
        | _ => x |> withReporters([Default])
        };
      };

      initialize() |> config;
    },
  );
