open Core;
open Parser;

let key = name => Parsers.(
  str(name) @>>@ char(':')
);
