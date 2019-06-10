open Treason__Parser;
open RelyInternal__RelyAPI.MatcherTypes;

type successExtensionsWithoutNot('a) = {
  toFail: unit => unit,
  /* toSucceed: unit => unit, */
  /* toBe: (result('a)) => unit, */
};

type successExtensions('a) = {
  not: successExtensionsWithoutNot('a),
  toFail: unit => unit,
  /* toSucceed: unit => unit, */
  /* toBe: (result('a)) => unit, */
};

let printResult = res =>
  switch (res) {
  | Success(parsed, remaining) =>
    Printf.sprintf("Success(%c, %s)", parsed, remaining)
  | Failure(msg) => Printf.sprintf("Failure(%s)", msg)
  };

let isSuccess = res =>
  switch (res) {
  | Success(_) => true
  | Failure(_) => false
  };

let isFailure = res => !isSuccess(res);

let successExtensions = (actual, {createMatcher}) => {

  let toFail = isNot =>
    createMatcher(({formatReceived}, actualThunk, expectedThunk) => {
      let actual = actualThunk();
      let actualIsFailure = isFailure(actual);

      switch (actualIsFailure, isNot) {
      | (true, false)
      | (false, true) => ((() => ""), true)
      | (false, false) =>
        let message =
          String.concat(
            "",
            [
              "\n\n",
              "Expected value to be Failure, but received: ",
              formatReceived(printResult(actual)),
            ],
          );
        ((() => message), false);
      | (true, true) =>
        let message =
          String.concat(
            "",
            [
              "\n\n",
              "Expected value to not be Failure, but received: ",
              formatReceived(printResult(actual)),
            ],
          );
        ((() => message), false);
      };
    });

  let makeSuccessMatchers = isNot => {
    toFail: () => toFail(isNot, () => actual, () => ()),
  };

  let successMatchers = makeSuccessMatchers(false);

  {
    toFail: successMatchers.toFail,
    not: makeSuccessMatchers(true),
  };
};
