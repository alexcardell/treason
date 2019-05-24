open TestFramework;
open Treason;

describe("Treason", ({test}) =>
  test("smoke", ({expect}) =>
    expect.bool(Server.smoke()).toBe(true)
  )
);
