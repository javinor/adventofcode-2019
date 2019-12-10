let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync;

module CartesianPoints =
  Belt.Id.MakeComparable({
    type t = (int, int);
    let cmp = compare;
  });

module ThreeTuple =
  Belt.Id.MakeComparable({
    type t = (int, int, int);
    let cmp = compare;
  });

let parseAsteroids = input =>
  input
  |> Js.String.split("\n")
  |> Array.map(Js.String.split(""))
  |> Array.mapi((y, row) =>
       row |> Array.mapi((x, str) => str == "#" ? Some((x, y)) : None)
     )
  |> Belt.Array.concatMany
  |> Array.fold_left(
       (asteroids, coords) =>
         switch (coords) {
         | None => asteroids
         | Some(p) => Belt.Set.add(asteroids, p)
         },
       Belt.Set.make(~id=(module CartesianPoints)),
     );

let gcd = (m, n) => {
  let rec go = (m, n) => {
    n == 0 ? m : go(n, m mod n);
  };

  let m = abs(m);
  let n = abs(n);
  n > m ? go(n, m) : go(m, n);
};

let linearCoefficients = ((x, y), (x0, y0)) => {
  let a = y - y0;
  let b = x - x0;
  let c = x * y0 - x0 * y;

  let gcd_ab = gcd(a, b);
  let gcd_ac = gcd(a, c);
  let gcd_bc = gcd(b, c);

  switch (a, b, c) {
  | (0, 0, 0) => failwith("input points must be different from each other")
  | (0, _, _) => (0, b / gcd_bc, c / gcd_bc)
  | (_, 0, _) => (a / gcd_ac, 0, c / gcd_ac)
  | (_, _, 0) => (a / gcd_ab, b / gcd_ab, 0)
  | (_, _, _) =>
    let gcd_abc = gcd(gcd_ab, c);
    (a / gcd_abc, b / gcd_abc, c / gcd_abc);
  };
};

module Part1 = {
  let countVisibleAsteroids = ((x, y), asteroids) => {
    asteroids
    |> Array.map(ast => linearCoefficients((x, y), ast))
    |> Belt.Set.fromArray(_, ~id=(module ThreeTuple))
    |> Belt.Set.size;
  };

  let asteroids = parseAsteroids(input);

  let asteroidScores =
    asteroids
    |> Belt.Set.toArray
    |> Array.map(p =>
         (
           p,
           countVisibleAsteroids(
             p,
             Belt.Set.(asteroids |> remove(_, p) |> toArray),
           ),
         )
       );

  let result =
    asteroidScores
    |> Array.fold_left(
         ((_, maxScore) as p, (_, score) as q) => maxScore > score ? p : q,
         ((0, 0), min_int),
       );

  Js.log2("Part1 output: ", result);
};

module Part2 = {
  let square = x => x * x;
  let norm = ((x, y), (x', y')) =>
    sqrt(float_of_int(square(x - x') + square(y - y')));

  let station = Part1.result |> fst;

  let asteroidsGroupedByLineOfSite =
    parseAsteroids(input)
    |> Belt.Set.remove(_, station)
    |> Belt.Set.toArray
    |> Array.map(p => (p, linearCoefficients(p, station)))
    |> Array.fold_left(
         (abcToPoint, (p, abc)) =>
           Belt.Map.update(abcToPoint, abc, value =>
             switch (value) {
             | None =>
               Some(Belt.Set.fromArray([|p|], ~id=(module CartesianPoints)))
             | Some(ps) => Some(Belt.Set.add(ps, p))
             }
           ),
         Belt.Map.make(~id=(module ThreeTuple)),
       )
    |> Belt.Map.toArray;

  let orderedAsteroids =
    asteroidsGroupedByLineOfSite
    |> Array.map(((abc, ps)) => {
         // PAY ATTENTION TO HOW THE COORDINATE SYSTEM MAPS TO POLAR!
         let phi =
           switch (abc) {
           | (0, b, _) => b > 0 ? 0. : Js.Math._PI
           | (a, 0, _) => (a > 0 ? 1. : (-1.)) *. Js.Math._PI /. 2.
           | (a, b, _) => atan2(float_of_int(a), float_of_int(b))
           };

         let orderedPhi =
           phi < (-1.) *. Js.Math._PI /. 2. ? phi +. Js.Math._PI *. 2. : phi;

         (orderedPhi, ps);
       })
    |> Array.map(((phi, ps)) =>
         ps
         |> Belt.Set.toArray
         |> Belt.SortArray.stableSortBy(_, (p1, p2) =>
              compare(norm(station, p1), norm(station, p2))
            )
         |> Array.mapi((i, p) =>
              (phi +. float_of_int(i) *. 2. *. Js.Math._PI, p)
            )
       )
    |> Belt.Array.concatMany
    |> Belt.SortArray.stableSortBy(_, ((phi, _), (phi', _)) =>
         compare(phi, phi')
       );

  let result = orderedAsteroids[199];

  Js.log2("Part2 output: ", result);
};