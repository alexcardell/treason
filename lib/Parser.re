type result('a) =
  | Success('a, string)
  | Failure(string);

type t('a) =
  | Parser(string => result('a));

let run = (parser, input) => {
  let Parser(fn) = parser;
  fn(input);
};

let return = a => {
  let fn = input => Success(a, input);
  Parser(fn);
};

let bind = (f, p) => {
  let fn = input => {
    let res1 = run(p, input);
    switch (res1) {
    | Failure(err) => Failure(err)
    | Success(val1, remaining) =>
      let p2 = f(val1);
      run(p2, remaining);
    };
  };
  Parser(fn);
};

let (>>=) = (p, f) => bind(f, p);

let andThen = (p1, p2) =>
  p1 >>= (p1Result => p2 >>= (p2Result => return((p1Result, p2Result))));

let (@>>@) = (p1, p2) => andThen(p1, p2);

let applyP = (fP, xP) => fP >>= (f => xP >>= (x => return(f(x))));

let (<*>) = (fP, xP) => applyP(fP, xP);

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

let (<|>) = (a, b) => orElse(a, b);

let choice = parsers => {
  open List;
  let reduce = (f, ls) => fold_left(f, hd(ls), tl(ls));

  parsers |> reduce(orElse);
};

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

let (|>>) = (p, f) => p |> mapP(f);

let (@>>) = (p1, p2) => p1 @>>@ p2 |>> fst;

let (>>@) = (p1, p2) => p1 @>>@ p2 |>> snd;

let lift2 = (f, xP, yP) => return(f) <*> xP <*> yP;

let rec sequence = listP => {
  let cons = (hd, tl) => [hd, ...tl];
  let consP = lift2(cons);

  switch (listP) {
  | [] => return([])
  | [hd, ...tl] => consP(hd, sequence(tl))
  };
};

let rec zeroOrMore = (p, input) => {
  let res1 = run(p, input);

  switch (res1) {
  | Failure(msg) => ([], input)
  | Success(firstValue, inputLeft) =>
    let (subsequentValues, remainingInput) = zeroOrMore(p, inputLeft);
    let values = [firstValue, ...subsequentValues];
    (values, remainingInput);
  };
};

let many = p => {
  let rec fn = input => {
    let (parsed, remaining) = zeroOrMore(p, input);
    Success(parsed, remaining);
  };

  Parser(fn);
};

let sepBy = (p, sep) => {
  let atLeastOnePThenMaybeSeparatorP = (p, sep) =>
    p @>>@ many(sep >>@ p) |>> (((p, listP)) => [p, ...listP]);

  atLeastOnePThenMaybeSeparatorP(p, sep) <|> return([]);
};

module Parsers = {
  let char = char => {
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

  let anyOf = (p, input) => input |> List.map(p) |> choice;

  let digit = {
    open List;
    let digits = init(10, x => x);
    let ascii0 = 48;

    digits |> map(x => x + ascii0) |> map(char_of_int) |> anyOf(char);
  };

  let (lowAlpha, upperAlpha) = {
    let alpha = asciiStart => {
      open List;

      let chars = init(26, x => x);

      let toStr = chars =>
        String.concat("", List.map(String.make(1), chars));

      print_endline(
        chars |> map(x => x + asciiStart) |> map(char_of_int) |> toStr,
      );

      chars |> map(x => x + asciiStart) |> map(char_of_int) |> anyOf(char);
    };

    let lowerStart = 97;
    let upperStart = 65;

    (alpha(lowerStart), alpha(upperStart));
  };

  let str = str => {
    let toStr = chars => String.concat("", List.map(String.make(1), chars));
    let toChars = Core.String.to_list;

    str |> toChars |> List.map(char) |> sequence |>> toStr;
  };
};
