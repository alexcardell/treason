open TestFramework;
open Treason;

describe("Treason", ({test}) => {
  test("tests work", ({expect}) => {
    let actual = Server.exec();
    expect.bool(actual).toBe(true);
  });
});
