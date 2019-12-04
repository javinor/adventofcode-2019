let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";

let (bottom, top) = (168630, 718098);

let toDigits = n => {
  let rec go = (acc, n) =>
    switch (n) {
    | 0 => acc
    | n => go([n mod 10, ...acc], n / 10)
    };

  go([], n) |> List.rev |> Array.of_list;
};

let areDigitsAscending = digits => {
  let ascendingDigits =
    digits |> Belt.SortArray.Int.stableSort |> Js.Array.reverseInPlace;
  digits == ascendingDigits;
};

module Part1 = {
  let hasRepeatingDigits = digits => {
    let uniqueDigits = Belt.Set.Int.fromArray(digits);
    Belt.Set.Int.size(uniqueDigits) <= 5;
  };

  let qualifyingPasswords = (from, to_) => {
    let rec go = (passwords, n, to_) =>
      if (n > to_) {
        passwords;
      } else {
        let digits = toDigits(n);
        let nextPasswords =
          areDigitsAscending(digits) && hasRepeatingDigits(digits)
            ? [n, ...passwords] : passwords;
        go(nextPasswords, n + 1, to_);
      };

    go([], from, to_);
  };

  let result = qualifyingPasswords(bottom, top);

  Js.log2("Part1 result: ", result |> List.length);
};

module Part2 = {
  let hasDoubleDigits = digits => {
    let histogram = Array.make(10, 0);

    digits |> Array.iter(digit => histogram[digit] = histogram[digit] + 1);

    histogram |> Js.Array.find(n => n == 2) |> Js.Option.isSome;
  };

  let qualifyingPasswords = (from, to_) => {
    let rec go = (passwords, n, to_) =>
      if (n > to_) {
        passwords;
      } else {
        let digits = toDigits(n);
        let nextPasswords =
          areDigitsAscending(digits) && hasDoubleDigits(digits)
            ? [n, ...passwords] : passwords;
        go(nextPasswords, n + 1, to_);
      };

    go([], from, to_);
  };

  let result = qualifyingPasswords(bottom, top);

  Js.log2("Part2 result: ", result |> List.length);
};