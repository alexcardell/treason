open TestFramework;
open Treason.Parser;

describe("Parser", ({test}) => {

  open Parsers;

  let pA = pchar('A');
  let pB = pchar('B');

  test("pchar sucess", ({expect}) => {
    let input = "ABC";

    let received = run(pA, input);

    expect.result(received).toBeOk();
  });

  test("pchar failure", ({expect}) => {
    let input = "BC";

    let received = run(pA, input);

    expect.result(received).toBeError();
  });

  describe("Parser -> Combinators -> andThen", ({test}) => {
    let p = pA @>>@ pB;

    test("andThen success", ({expect}) => {
      let received = run(p, "AB");

      expect.result(received).toBeOk();
    });

    test("andThen failure 1", ({expect}) => {
      let x = run(p, "BB");
      expect.result(x).toBeError();
    });

    test("andThen failure 2", ({expect}) => {
      let x = run(p, "AC");

      expect.result(x).toBeError();
    });
  });

  describe("Parser -> Combinators -> orElse", ({test}) => {
    let p = pA <|> pB;

    test("orElse success 1", ({expect}) => {
      let received = run(p, "AC");

      expect.result(received).toBeOk();
    });

    test("orElse success 2", ({expect}) => {
      let received = run(p, "BC");

      expect.result(received).toBeOk();
    });

    test("orElse failure", ({expect}) => {
      let x = run(p, "DC") ;

      expect.result(x).toBeError();
    });
  });

  describe("Parser -> Combinators -> anyOf", ({test}) => {
    let chars = ['A', 'B', 'C'];
    let p = anyOf(chars);

    test("anyOf success 1", ({expect}) => {
      let received = run(p, "AD");

      expect.result(received).toBeOk();
    });

    test("anyOf success 2", ({expect}) => {
      let received = run(p, "BD");

      expect.result(received).toBeOk();
    });

    test("anyOf success 3", ({expect}) => {
      let received = run(p, "CD");

      expect.result(received).toBeOk();
    });

    test("anyOf failure 1", ({expect}) => {
      let x = run(p, "DC");

      expect.result(x).toBeError();
    });
  });
});
