type result('a) =
  | Success('a, string)
  | Failure(string);

type t('a) =
  | Parser(string => result('a));

let run = (parser, input) => {
  let Parser(fn) = parser;
  fn(input);
};

let returnP = a => {
  let fn = input => Success(a, input);
  Parser(fn);
};

let bindP = (f, p) => {
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

let (>>=) = (p, f) => bindP(f, p);

let andThen = (p1, p2) =>
  p1 >>= (p1Result => p2 >>= (p2Result => returnP((p1Result, p2Result))));

let (@>>@) = (p1, p2) => andThen(p1, p2);

let applyP = (fP, xP) => fP >>= (f => xP >>= (x => returnP(f(x))));

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

let (|>>) = (x, f) => x |> mapP(f);

let (@>>) = (p1, p2) => p1 @>>@ p2 |>> fst;

let (>>@) = (p1, p2) => p1 @>>@ p2 |>> snd;

let lift2 = (f, xP, yP) => returnP(f) <*> xP <*> yP;

let rec sequence = listP => {
  let cons = (hd, tl) => [hd, ...tl];
  let consP = lift2(cons);

  switch (listP) {
  | [] => returnP([])
  | [hd, ...tl] => consP(hd, sequence(tl))
  };
};

let rec parseZeroOrMore = (p, input) => {
  let res1 = run(p, input);

  switch (res1) {
  | Failure(msg) => ([], input)
  | Success(firstValue, inputLeft) =>
    let (subsequentValues, remainingInput) = parseZeroOrMore(p, inputLeft);
    let values = [firstValue, ...subsequentValues];
    (values, remainingInput);
  };
};

let many = p => {
  let rec fn = input => {
    let (parsed, remaining) = parseZeroOrMore(p, input);
    Success(parsed, remaining);
  };

  Parser(fn);
};

let sepBy = (p, sep) => {
  let atLeastOnePThenMaybeSeparatorP = (p, sep) =>
    p @>>@ many(sep >>@ p) |>> (((p, listP)) => [p, ...listP]);

  atLeastOnePThenMaybeSeparatorP(p, sep) <|> returnP([]);
};

module Parsers = {
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

  let anyOf = (p, input) => input |> List.map(p) |> choice;

  let pDigit = {
    open List;
    let digits = init(10, x => x);
    let ascii0 = 48;

    digits |> map(x => x + ascii0) |> map(char_of_int) |> anyOf(pchar);
  };

  let pStr = str => {
    let toStr = chars => String.concat("", List.map(String.make(1), chars));
    let toChars = Core.String.to_list;

    str |> toChars |> List.map(pchar) |> sequence |>> toStr;
  };
};
