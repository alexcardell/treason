open TestFramework;
open Treason.Parser;

open SuccessExtensions;

type customMatchers('a) = {
  success:
    result('a) => successExtensions('a),
};

let customMatchers = createMatcher => {
  success: actual => successExtensions(actual, createMatcher)
};

let { describe } = extendDescribe(customMatchers);

describe("Parser -> Parsers -> pchar", ({test}) => {
  let pA = pchar('A');

  test("pchar sucess", ({expect}) => {
    let input = "ABC";

    let received = run(pA, input);

    expect.ext.success(received).not.toFail();
  });
});

/*   test("pchar failure", ({expect}) => { */
/*     let input = "BC"; */

/*     let received = run(pA, input); */

/*     expect.result(received).toBeError(); */
/*   }); */
/* }); */

/* describe("Parser -> Parsers -> pdigit", ({test}) => { */
/*   test("pchar sucess", ({expect}) => { */
/*     let input = "0123"; */

/*     let received = run(pdigit, input); */
/*     let expected = Ok(('0', "123")); */

/*     expect.result(received).toBe(expected); */
/*   }); */

/*   test("pchar failure", ({expect}) => { */
/*     let input = "BC"; */

/*     let received = run(pdigit, input); */

/*     expect.result(received).toBeError(); */
/*   }); */
/* }); */

/* describe("Parser -> Combinators -> andThen", ({test}) => { */
/*   let pA = pchar('A'); */
/*   let pB = pchar('B'); */
/*   let p = pA @>>@ pB; */

/*   test("andThen success", ({expect}) => { */
/*     let received = run(p, "AB"); */
/*     let expected = Ok((('A', 'B'), "")); */

/*     expect.result(received).toBe(expected); */
/*   }); */

/*   test("andThen failure 1", ({expect}) => { */
/*     let received = run(p, "BB"); */
/*     expect.result(received).toBeError(); */
/*   }); */

/*   test("andThen failure 2", ({expect}) => { */
/*     let received = run(p, "AC"); */

/*     expect.result(received).toBeError(); */
/*   }); */
/* }); */

/* describe("Parser -> Combinators -> orElse", ({test}) => { */
/*   let pA = pchar('A'); */
/*   let pB = pchar('B'); */
/*   let p = pA <|> pB; */

/*   test("orElse success 1", ({expect}) => { */
/*     let received = run(p, "AC"); */
/*     let expected = Ok(('A', "C")); */

/*     expect.result(received).toBe(expected); */
/*   }); */

/*   test("orElse success 2", ({expect}) => { */
/*     let received = run(p, "BC"); */
/*     let expected = Ok(('B', "C")); */

/*     expect.result(received).toBe(expected); */
/*   }); */

/*   test("orElse failure", ({expect}) => { */
/*     let x = run(p, "DC"); */

/*     expect.result(x).toBeError(); */
/*   }); */
/* }); */

/* describe("Parser -> Combinators -> anyOf", ({test}) => { */
/*   let chars = ['A', 'B', 'C']; */
/*   let p = anyOf(chars); */

/*   test("anyOf success 1", ({expect}) => { */
/*     let received = run(p, "AD"); */
/*     let expected = Ok(('A', "D")); */

/*     expect.result(received).toBe(expected); */
/*   }); */

/*   test("anyOf success 2", ({expect}) => { */
/*     let received = run(p, "BD"); */
/*     let expected = Ok(('B', "D")); */

/*     expect.result(received).toBe(expected); */
/*   }); */

/*   test("anyOf success 3", ({expect}) => { */
/*     let received = run(p, "CD"); */
/*     let expected = Ok(('C', "D")); */

/*     expect.result(received).toBe(expected); */
/*   }); */

/*   test("anyOf failure 1", ({expect}) => { */
/*     let received = run(p, "DC"); */

/*     expect.result(received).toBeError(); */
/*   }); */
/* }); */
