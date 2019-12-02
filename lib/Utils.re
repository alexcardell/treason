let toStr = chars => String.concat("", List.map(String.make(1), chars));

let prependChar = (c, s) => String.concat("", [toStr([c]), s]);

let prependCharTuple = ((c, s)) => prependChar(c, s);

let toChars = Core.String.to_list;

let reduce = (f, ls) => List.(fold_left(f, hd(ls), tl(ls)));

let map2 = (f, (a, b)) => (f(a), f(b));

let map3 = (f, (a, b, c)) => (f(a), f(b), f(c));
