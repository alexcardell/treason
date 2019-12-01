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

/* andThen */
let (@>>@) = (p1, p2) => andThen(p1, p2);

let apply = (fP, xP) => fP >>= (f => xP >>= (x => return(f(x))));

/* apply */
let (<*>) = (fP, xP) => apply(fP, xP);

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

/* orElse */
let (<|>) = (a, b) => orElse(a, b);

let choice = Utils.reduce(orElse);

let map = (f, parser) => {
  let fn = input => {
    let result = run(parser, input);
    switch (result) {
    | Success(value, remaining) => Success(f(value), remaining)
    | Failure(err) => Failure(err)
    };
  };
  Parser(fn);
};

/* map */
let (|>>) = (p, f) => p |> map(f);

/* andThen, keepLeft */
let (@>>) = (p1, p2) => p1 @>>@ p2 |>> fst;

/* andThen, keepRight */
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

let sepBy = (p, sep) =>
  p
  @>>@ many(sep >>@ p)
  |>> (((p, listP)) => [p, ...listP])
  <|> return([]);

let anyOf = (p, input) => input |> List.map(p) |> choice;

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

  let digit = {
    let digits = List.init(10, x => x);
    let ascii0 = 48;

    digits
    |> List.map(x => x + ascii0)
    |> List.map(char_of_int)
    |> anyOf(char);
  };

  let (lowerAlpha, upperAlpha) = {
    let alpha = asciiStart => {
      let chars = List.init(26, x => x);

      chars
      |> List.map(x => x + asciiStart)
      |> List.map(char_of_int)
      |> anyOf(char);
    };

    let lowerStart = 97;
    let upperStart = 65;

    (alpha(lowerStart), alpha(upperStart));
  };

  let alpha = lowerAlpha <|> upperAlpha;

  let whitespace = [' ', '\n', '\t'] |> anyOf(char);

  let str = str =>
    Utils.toChars(str) |> List.map(char) |> sequence |>> Utils.toStr;

  let uint = many(digit) |>> (ls => ls |> Utils.toStr |> int_of_string);

  let plus = uint <|> (char('+') >>@ uint);

  let minus = char('-') >>@ uint |>> (x => - x);

  let integer = plus <|> minus;

  let floating =
    many(digit)
    @>>@ char('.')
    @>>@ many(digit)
    |>> (
      ((pre, (dot, post))) => {
        [pre, [dot], post]
        |> List.map(Utils.toStr)
        |> String.concat("")
        |> float_of_string;
      }
    );
};
