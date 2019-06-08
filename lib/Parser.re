type result('a) =
  | Success('a, string)
  | Failure(string);

type t('a) =
  | Parser(string => result('a));

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

let run = (parser, input) => {
  let Parser(fn) = parser;
  fn(input);
};

module Combinators = {
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

  let choice = parsers =>
    List.fold_left(orElse, List.hd(parsers), List.tl(parsers));

};

open Combinators;

let (@>>@) = (a, b) => andThen(a, b);

let (<|>) = (a, b) => orElse(a, b);

let anyOf = chars =>
  chars
    |> List.map(pchar)
    |> choice
