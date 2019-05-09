open TestFramework;
/* open Lib; */

describe("suite", ({test}) => {
  test("test", ({expect}) => {
    expect.int(1 + 1).toBe(2)
  });
});
