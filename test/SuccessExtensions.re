open Treason.Parser;
open RelyInternal.RelyAPI.MatcherTypes;

type toBe('a) = result('a) => unit;

type not('a) = {toBe: toBe('a)};

type successExtensions('a) = {
  toFail: unit => unit,
  toSucceed: unit => unit,
  toBe: result('a) => unit,
  not: not('a),
};

let printResult = res =>
  switch (res) {
  | Success(parsed, remaining) =>
    Printf.sprintf(
      "Success(%s, %s)",
      RelyInternal__PolymorphicPrint.print(parsed),
      remaining,
    )
  | Failure(msg) => Printf.sprintf("Failure(%s)", msg)
  };

let isSuccess = res =>
  switch (res) {
  | Success(_) => true
  | Failure(_) => false
  };

let isFailure = res => !isSuccess(res);

let successExtensions = (actual, {createMatcher}) => {
  let toSucceed =
    createMatcher(({formatReceived}, actualThunk, expectedThunk) => {
      let actual = actualThunk();
      isSuccess(actual) ?
        (() => "", true) :
        {
          let message =
            String.concat(
              "",
              [
                "\n\n",
                "Expected value to be Success, but received: ",
                formatReceived(printResult(actual)),
              ],
            );
          (() => message, false);
        };
    });

  let toFail =
    createMatcher(({formatReceived}, actualThunk, expectedThunk) => {
      let actual = actualThunk();
      isFailure(actual) ?
        (() => "", true) :
        {
          let message =
            String.concat(
              "",
              [
                "\n\n",
                "Expected value to be Failure, but received: ",
                formatReceived(printResult(actual)),
              ],
            );
          (() => message, false);
        };
    });

  let toBe = isNot =>
    createMatcher(
      ({formatExpected, formatReceived}, actualThunk, expectedThunk) => {
      let actual = actualThunk();
      let expected = expectedThunk();

      let actualEqualsExpected =
        switch (actual, expected) {
        | (Failure(actual), Failure(expected)) when actual == expected =>
          true
        | (Success(actual, remaining), Success(expected, remaining2))
            when actual == expected && remaining == remaining2 =>
          true
        | (_, _) => false
        };

      let pass =
        actualEqualsExpected && !isNot || !actualEqualsExpected && isNot;

      if (!pass) {
        let message =
          String.concat(
            "",
            [
              "\n\n",
              "Expected: ",
              formatExpected(printResult(expected)),
              "\n",
              "Received: ",
              formatReceived(printResult(actual)),
            ],
          );
        (() => message, pass);
      } else {
        (() => "", pass);
      };
    });

  let successMatchers = {
    toFail: () => toFail(() => actual, () => ()),
    toSucceed: () => toSucceed(() => actual, () => ()),
    toBe: expected => toBe(false, () => actual, () => expected),
    not: {
      toBe: expected => toBe(true, () => actual, () => expected),
    },
  };

  successMatchers;
};
