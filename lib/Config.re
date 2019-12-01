open Core;
open Parser;

type element =
  | SessionName(string);

let key = keyName =>
  Parsers.(str(keyName) @>>@ char(':') @>>@ many(whitespace));

let stringVal = keyName =>
  Parsers.(key(keyName) >>@ many(alpha))
  |>> (value => SessionName(value |> Utils.toStr));

/* let numericVal = keyName => Parsers.(key(keyName) >>@ number); */

let name = stringVal("name");
