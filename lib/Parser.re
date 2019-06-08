type result('a) = Pervasives.result(('a, string), string);

type t('a) =
  | Parser(string => result('a));

let run = (parser, input) => {
  let Parser(fn) = parser;
  fn(input);
};

let pchar = char => {
  let fn = str =>
    if (str == "") {
      Error("Empty Input");
    } else {
      let first = str.[0];
      if (first == char) {
        let remaining = String.sub(str, 1, String.length(str) - 1);
        Ok((char, remaining));
      } else {
        let msg = Printf.sprintf("Expecting '%c'. Got '%c'", char, first);
        Error(msg);
      };
    };
  Parser(fn);
};

let andThen = (p1, p2) => {
  let fn = input =>
    switch (input |> run(p1)) {
    | Error(msg) => Error(msg)
    | Ok((v1, remaining)) =>
      switch (run(p2, remaining)) {
      | Error(msg) => Error(msg)
      | Ok((v2, remaining2)) => Ok(((v1, v2), remaining2))
      }
    };
  Parser(fn);
};

let orElse = (p1, p2) => {
  let fn = input => {
    let result = input |> run(p1);
    switch (result) {
    | Ok(_) => result
    | Error(msg) => input |> run(p2)
    };
  };
  Parser(fn);
};

let reduce = (f, ls) => List.fold_left(f, List.hd(ls), List.tl(ls));

let choice = parsers => parsers |> reduce(orElse);

let mapP = (f, parser) => {
  let fn = input => {
    let result = run(parser, input);
    switch (result) {
    | Ok((value, remaining)) => Ok((f(value), remaining))
    | Error(err) => Error(err)
    };
  };
  Parser(fn);
};

let (@>>@) = (a, b) => andThen(a, b);

let (<|>) = (a, b) => orElse(a, b);

let (|>>) = (x, f) => mapP(f, x);

let anyOf = chars => chars |> List.map(pchar) |> choice;

let pdigit = {
  let rec range = (start, end_) =>
    if (start >= end_) {
      [];
    } else {
      [start, ...range(start + 1, end_)];
    };
  let digits = range(0, 9);
  let ascii0 = 48;
  digits |> List.map(x => x + ascii0) |> List.map(char_of_int) |> anyOf;
};
