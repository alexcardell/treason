open TestFramework;
open Treason.Parser;

describe("Parser", ({test}) => {
  let pA = pchar('A');
  let pB = pchar('B');

  test("pchar sucess", ({expect}) => {
    let input = "ABC";

    let received = run(pA, input);
    let expected = Success('A', "BC");

    expect.equal(received, expected);
  });

  test("pchar failure", ({expect}) => {
    let input = "BC";

    let received = run(pA, input);
    let expected = Failure("Expecting 'A'. Got 'B'");

    expect.equal(received, expected);
  });

  describe("Parser -> Combinators -> andThen", ({test}) => {
    let p = pA @>>@ pB;

    test("andThen success", ({expect}) => {
      let received = run(p, "AB");
      let expected = Success(('A', 'B'), "");

      expect.equal(received, expected);
    });

    test("andThen failure 1", ({expect}) => {
      let x =
        switch (run(p, "BB")) {
        | Success(_) => false
        | Failure(_) => true
        };
      expect.bool(x).toBe(true);
    });

    test("andThen failure 2", ({expect}) => {
      let x =
        switch (run(p, "AC")) {
        | Success(_) => false
        | Failure(_) => true
        };
      expect.bool(x).toBe(true);
    });
  });

  describe("Parser -> Combinators -> orElse", ({test}) => {
    let p = pA <|> pB;

    test("orElse success 1", ({expect}) => {
      let received = run(p, "AC");
      let expected = Success('A', "C");

      expect.equal(received, expected);
    });

    test("orElse succes 2", ({expect}) => {
      let received = run(p, "BC");
      let expected = Success('B', "C");

      expect.equal(received, expected);
    });

    test("orElse failure", ({expect}) => {
      let x =
        switch (run(p, "DC")) {
        | Success(_) => false
        | Failure(_) => true
        };
      expect.bool(x).toBe(true);
    });
  });

  describe("Parser -> Combinators -> anyOf", ({test}) => {
    let chars = ['A', 'B', 'C'];
    let p = anyOf(chars);

    test("anyOf success 1", ({expect}) => {
      let received = run(p, "AD");
      let expected = Success('A', "D");

      expect.equal(received, expected);
    });

    test("anyOf success 2", ({expect}) => {
      let received = run(p, "BD");
      let expected = Success('B', "D");

      expect.equal(received, expected);
    });

    test("anyOf success 3", ({expect}) => {
      let received = run(p, "CD");
      let expected = Success('C', "D");

      expect.equal(received, expected);
    });

    test("anyOf failure 1", ({expect}) => {
      let x =
        switch (run(p, "DC")) {
        | Success(_) => false
        | Failure(_) => true
        };
      expect.bool(x).toBe(true);
    });
  });
});
