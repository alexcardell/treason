open TestFramework;
open Treason.Parser;
open SuccessExtensions;

type customMatchers = {success: 'a. result('a) => successExtensions('a)};

let customMatchers = createMatcher => {
  success: actual => successExtensions(actual, createMatcher),
};

let {describe} = extendDescribe(customMatchers);

describe("Parser / Parsers / pchar", ({test}) => {
  let pA = pchar('A');

  test("pchar sucess", ({expect}) => {
    let input = "ABC";

    let received = run(pA, input);

    expect.ext.success(received).toSucceed();
  });

  test("pchar failure", ({expect}) => {
    let input = "BC";

    let received = run(pA, input);

    expect.ext.success(received).toFail();
  });
});

describe("Parser / Parsers / pDigit", ({test}) => {
  test("pDigit sucess", ({expect}) => {
    let input = "0123";

    let received = run(pDigit, input);
    let expected = Success('0', "123");

    expect.ext.success(received).toBe(expected);
  });

  test("pDigit failure", ({expect}) => {
    let input = "BC";

    let received = run(pDigit, input);

    expect.ext.success(received).toFail();
  });
});

describe("Parser / Combinators / andThen", ({test}) => {
  let pA = pchar('A');
  let pB = pchar('B');
  let pAB = pA @>>@ pB;

  let pA0 = pA @>>@ pDigit;

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
  let pA = pchar('A');
  let pB = pchar('B');
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
  let p = anyOf(chars);

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

describe("Parser / Parsers / pStr ", ({test}) => {
  test("parser for \"ABC\" should succeed given \"ABC\"", ({expect}) => {
    let pABC = pStr("ABC");
    let received = run(pABC, "ABC");
    let expected = Success("ABC", "");

    expect.ext.success(received).toBe(expected);
  });

  test("parser for \"ABC\" should fail given \"ABB\"", ({expect}) => {
    let pABC = pStr("ABC");
    let received = run(pABC, "ABB");

    expect.ext.success(received).toFail();
  });

  test("parser for \"12AB\" should succeed given \"12AB3C\"", ({expect}) => {
    let pABC = pStr("12AB");
    let received = run(pABC, "12AB3C");
    let expected = Success("12AB", "3C");

    expect.ext.success(received).toBe(expected);
  });
});

describe("Parser / Parsers / many ", ({test}) =>
  test("parser for \"ABC\" should succeed given \"ABC\"", ({expect}) => {
    let pA = pchar('A');
    let pManyA = many(pA);
    let received = run(pManyA, "AAAB");
    let expected = Success(['A', 'A', 'A'], "B");

    expect.ext.success(received).toBe(expected);
  })
);
