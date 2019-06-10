type result('a) =
  | Success('a, string)
  | Failure(string)

type t('a) =
  | Parser(string => result('a));

let run = (parser, input) => {
  let Parser(fn) = parser;
  fn(input);
};

let pchar = char => {
  let fn = str =>
    if (str == "") {
      Failure("Empty Input");
    } else {
      let first = str.[0];
      if (first == char) {
        let remaining = String.sub(str, 1, String.length(str) - 1);
        Success(char, remaining);
      } else {
        let msg = Printf.sprintf("Expecting '%c'. Got '%c'", char, first);
        Failure(msg);
      };
    };
  Parser(fn);
};

let andThen = (p1, p2) => {
  let fn = input =>
    switch (input |> run(p1)) {
    | Failure(msg) => Failure(msg)
    | Success(v1, remaining) =>
      switch (run(p2, remaining)) {
      | Failure(msg) => Failure(msg)
      | Success(v2, remaining2) => Success((v1, v2), remaining2)
      }
    };
  Parser(fn);
};

let orElse = (p1, p2) => {
  let fn = input => {
    let result = input |> run(p1);
    switch (result) {
    | Success(_) => result
    | Failure(msg) => input |> run(p2)
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
    | Success(value, remaining) => Success(f(value), remaining)
    | Failure(err) => Failure(err)
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
