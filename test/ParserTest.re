open TestFramework;
open Treason;
open Parser;
open Parsers;
open SuccessExtensions;

type customMatchers = {success: 'a. result('a) => successExtensions('a)};

let customMatchers = createMatcher => {
  success: actual => successExtensions(actual, createMatcher),
};

let {describe} = extendDescribe(customMatchers);

describe("Parser / Parsers / char", ({test}) => {
  let pA = char('A');

  test("char success", ({expect}) => {
    let input = "ABC";

    let received = run(pA, input);

    expect.ext.success(received).toSucceed();
  });

  test("char failure", ({expect}) => {
    let input = "BC";

    let received = run(pA, input);

    expect.ext.success(received).toFail();
  });
});

describe("Parser / Parsers / digit", ({test}) => {
  test("digit success", ({expect}) => {
    let input = "0123";

    let received = run(digit, input);
    let expected = Success('0', "123");

    expect.ext.success(received).toBe(expected);
  });

  test("digit failure", ({expect}) => {
    let input = "BC";

    let received = run(digit, input);

    expect.ext.success(received).toFail();
  });
});

describe("Parser / Parsers / alpha", ({test}) => {
  test("lowerAlpha success", ({expect}) => {
    let input = "abc";

    let received = run(lowerAlpha, input);
    let expected = Success('a', "bc");

    expect.ext.success(received).toBe(expected);
  });

  test("lowerAlpha failure", ({expect}) => {
    let input = "ABC";

    let received = run(lowerAlpha, input);

    expect.ext.success(received).toFail();
  });

  test("upperAlpha success", ({expect}) => {
    let input = "ABC";

    let received = run(upperAlpha, input);
    let expected = Success('A', "BC");

    expect.ext.success(received).toBe(expected);
  });

  test("upperAlpha failure", ({expect}) => {
    let input = "abc";

    let received = run(upperAlpha, input);

    expect.ext.success(received).toFail();
  });
});

describe("Parser / Combinators / andThen", ({test}) => {
  let pA = char('A');
  let pB = char('B');
  let pAB = pA @>>@ pB;

  let pA0 = pA @>>@ digit;

  test("given 'AB', 'A' andThen 'B' should succeed", ({expect}) => {
    let received = run(pAB, "AB");
    let expected = Success(('A', 'B'), "");

    expect.ext.success(received).toBe(expected);
  });

  test("given 'BB', 'A' andThen 'B' should fail", ({expect}) => {
    let received = run(pAB, "BB");
    expect.ext.success(received).toFail();
  });

  test("given 'AC', 'A' andThen 'B' should fail", ({expect}) => {
    let received = run(pAB, "AC");
    expect.ext.success(received).toFail();
  });

  test("given 'A0', 'A' andThen digit should succeeed", ({expect}) => {
    let received = run(pA0, "A0");
    let expected = Success(('A', '0'), "");
    expect.ext.success(received).toBe(expected);
  });

  test("given 'AA', 'A' andThen digit should succeeed", ({expect}) => {
    let received = run(pA0, "AA");
    expect.ext.success(received).toFail();
  });
});

describe("Parser / Combinators / orElse", ({test}) => {
  let pA = char('A');
  let pB = char('B');
  let p = pA <|> pB;

  test("orElse success 1", ({expect}) => {
    let received = run(p, "AC");
    let expected = Success('A', "C");

    expect.ext.success(received).toBe(expected);
  });

  test("orElse success 2", ({expect}) => {
    let received = run(p, "BC");
    let expected = Success('B', "C");

    expect.ext.success(received).toBe(expected);
  });

  test("orElse failure", ({expect}) => {
    let x = run(p, "DC");

    expect.ext.success(x).toFail();
  });
});

describe("Parser / Combinators / anyOf", ({test}) => {
  let chars = ['A', 'B', 'C'];
  let p = anyOf(char, chars);

  test("anyOf success 1", ({expect}) => {
    let received = run(p, "AD");
    let expected = Success('A', "D");

    expect.ext.success(received).toBe(expected);
  });

  test("anyOf success 2", ({expect}) => {
    let received = run(p, "BD");
    let expected = Success('B', "D");

    expect.ext.success(received).toBe(expected);
  });

  test("anyOf success 3", ({expect}) => {
    let received = run(p, "CD");
    let expected = Success('C', "D");

    expect.ext.success(received).toBe(expected);
  });

  test("anyOf failure 1", ({expect}) => {
    let received = run(p, "DC");

    expect.ext.success(received).toFail();
  });
});

describe("Parser / Parsers / str ", ({test}) => {
  test("parser for \"ABC\" should succeed given \"ABC\"", ({expect}) => {
    let pABC = str("ABC");
    let received = run(pABC, "ABC");
    let expected = Success("ABC", "");

    expect.ext.success(received).toBe(expected);
  });

  test("parser for \"ABC\" should fail given \"ABB\"", ({expect}) => {
    let pABC = str("ABC");
    let received = run(pABC, "ABB");

    expect.ext.success(received).toFail();
  });

  test("parser for \"12AB\" should succeed given \"12AB3C\"", ({expect}) => {
    let pABC = str("12AB");
    let received = run(pABC, "12AB3C");
    let expected = Success("12AB", "3C");

    expect.ext.success(received).toBe(expected);
  });
});

describe("Parser / Parsers / many ", ({test}) =>
  test("parser for \"ABC\" should succeed given \"ABC\"", ({expect}) => {
    let pManyA = many(char('A'));
    let received = run(pManyA, "AAAB");
    let expected = Success(['A', 'A', 'A'], "B");

    expect.ext.success(received).toBe(expected);
  })
);

describe("Parser / Parsers / keep ", ({test}) => {
  test("p1 keepR p2 shoud succeed given \"p1p2\"", ({expect}) => {
    let p1 = char('A');
    let p2 = char('B');

    let received = run(p1 >>@ p2, "AB");
    let expected = Success('B', "");

    expect.ext.success(received).toBe(expected);
  });

  test("p1 keepL p2 shoud succeed given \"p1p2\"", ({expect}) => {
    let p1 = char('A');
    let p2 = char('B');

    let received = run(p1 @>> p2, "AB");
    let expected = Success('A', "");

    expect.ext.success(received).toBe(expected);
  });
});

describe("Parser / Parsers / sepBy ", ({test}) =>
  test("p sepBy ; should succeed given p;p;p;", ({expect}) => {
    let pA = char('A');

    let received = run(sepBy(pA, char(';')), "A;A;A;A");
    let expected = Success(['A', 'A', 'A', 'A'], "");

    expect.ext.success(received).toBe(expected);
  })
);

describe("Parser / Parsers / numbers ", ({test}) => {
  test("plus succeeds on a postive", ({expect}) => {
    let received = run(plus, "123");
    let expected = Success("123", "");

    expect.ext.success(received).toBe(expected);
  });

  test("plus fails on a negative", ({expect}) => {
    let received = run(plus, "-123");

    expect.ext.success(received).toFail();
  });

  test("minus succeeds negative", ({expect}) => {
    let received = run(minus, "-123");
    let expected = Success("-123", "");

    expect.ext.success(received).toBe(expected);
  });

  test("minus fails on a postive", ({expect}) => {
    let received = run(minus, "123");

    expect.ext.success(received).toFail();
  });
});

open Config;

describe("Config", ({test}) => {
  test("name", ({expect}) => {
    let received = run(name, "name: alex");
    let expected = Success(SessionName("alex"), "");

    expect.equal(received, expected);
  });

  test("name", ({expect}) => {
    let received = run(name, "name: alex");
    let expected = Success(SessionName("alex"), "");

    expect.equal(received, expected);
  });
});
