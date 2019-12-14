let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");

type position = {
  x: int,
  y: int,
  z: int,
};
type velocity = {
  vx: int,
  vy: int,
  vz: int,
};

let parseMoon = str => {
  let re = [%bs.re "/^<x=(.+), y=(.+), z=(.+)>$/"];

  switch (
    Js.String.match(re, str)
    |> Js.Option.getExn
    |> Js.Array.sliceFrom(1)
    |> Array.map(Js.String.trim)
    |> Array.map(int_of_string)
  ) {
  | exception e =>
    Js.log2("exception was raised while parsing input: " ++ str, e);
    raise(e);
  | [|x, y, z|] => ({x, y, z}, {vx: 0, vy: 0, vz: 0})
  | _ => failwith("failed to parse input: " ++ str)
  };
};

let sign = n => n > 0 ? 1 : n < 0 ? (-1) : 0;

let applyGravity = (moon1, moon2) => {
  let ({x: x1, y: y1, z: z1} as p1, {vx: vx1, vy: vy1, vz: vz1}) = moon1;
  let ({x: x2, y: y2, z: z2} as p2, {vx: vx2, vy: vy2, vz: vz2}) = moon2;

  let dvx1 = sign(compare(x2, x1));
  let dvy1 = sign(compare(y2, y1));
  let dvz1 = sign(compare(z2, z1));

  let dvx2 = (-1) * dvx1;
  let dvy2 = (-1) * dvy1;
  let dvz2 = (-1) * dvz1;

  let newMoon1 = (p1, {vx: vx1 + dvx1, vy: vy1 + dvy1, vz: vz1 + dvz1});
  let newMoon2 = (p2, {vx: vx2 + dvx2, vy: vy2 + dvy2, vz: vz2 + dvz2});
  (newMoon1, newMoon2);
};

let applyVelocity = (({x, y, z}, {vx, vy, vz} as v)) => {
  ({x: x + vx, y: y + vy, z: z + vz}, v);
};

let totalEnergy = (({x, y, z}, {vx, vy, vz})) => {
  let energy = coords => coords |> Array.map(abs) |> Array.fold_left((+), 0);
  energy([|x, y, z|]) * energy([|vx, vy, vz|]);
};

module Part1 = {
  let tick = (moons, n) => {
    let rec go = (moons, n) =>
      if (n == 0) {
        moons;
      } else {
        for (i in 0 to Array.length(moons) - 1) {
          for (j in i + 1 to Array.length(moons) - 1) {
            let (p1, p2) = applyGravity(moons[i], moons[j]);
            moons[i] = p1;
            moons[j] = p2;
          };
        };

        let nextMoons = moons |> Array.map(applyVelocity);
        go(nextMoons, n - 1);
      };
    go(Array.copy(moons), n);
  };

  let moons = input |> Array.map(parseMoon);

  let result =
    tick(moons, 1000) |> Array.map(totalEnergy) |> Array.fold_left((+), 0);

  Js.log2("Part1 output: ", result);
};

module Part2 = {
  let gcd = (m, n) => {
    let rec go = (m, n) => {
      n == 0 ? m : go(n, m mod n);
    };

    let m = abs(m);
    let n = abs(n);
    n > m ? go(n, m) : go(m, n);
  };
  let lcm = (m, n) =>
    m *. n /. float(gcd(int_of_float(m), int_of_float(n)));

  let calculatePeriods = initialMoons => {
    let rec go = (moons, nTicks, periods) =>
      // Js.log2(nTicks, moons);
      if (Js.Array.every(Js.Option.isSome, periods)) {
        periods |> Array.map(Js.Option.getExn) |> Array.map(float);
      } else {
        for (i in 0 to Array.length(moons) - 1) {
          for (j in i + 1 to Array.length(moons) - 1) {
            let (p1, p2) = applyGravity(moons[i], moons[j]);
            moons[i] = p1;
            moons[j] = p2;
          };
        };

        let nextMoons = moons |> Array.map(applyVelocity);
        let nTicks' = nTicks + 1;

        if (nTicks' == 2772) {
          Js.log(nextMoons);
        };

        let periodOfX =
          nextMoons
          |> Js.Array.everyi(((p, v), i) =>
               p.x == fst(initialMoons[i]).x
               && v.vx == snd(initialMoons[i]).vx
             );
        let periodOfY =
          nextMoons
          |> Js.Array.everyi(((p, v), i) =>
               p.y == fst(initialMoons[i]).y
               && v.vy == snd(initialMoons[i]).vy
             );
        let periodOfZ =
          nextMoons
          |> Js.Array.everyi(((p, v), i) =>
               p.z == fst(initialMoons[i]).z
               && v.vz == snd(initialMoons[i]).vz
             );

        if (periodOfX && Js.Option.isNone(periods[0])) {
          periods[0] = Some(nTicks');
        };
        if (periodOfY && Js.Option.isNone(periods[1])) {
          periods[1] = Some(nTicks');
        };
        if (periodOfZ && Js.Option.isNone(periods[2])) {
          periods[2] = Some(nTicks');
        };

        go(nextMoons, nTicks', periods);
      };

    go(Array.copy(initialMoons), 0, Array.make(3, None));
  };

  // let dummy_input = [|
  //   "<x=-1, y=0, z=2>",
  //   "<x=2, y=-10, z=-7>",
  //   "<x=4, y=-8, z=8>",
  //   "<x=3, y=5, z=-1>",
  // |];
  // let dummy_input = [|
  //   "<x=-8, y=-10, z=0>",
  //   "<x=5, y=5, z=10>",
  //   "<x=2, y=-7, z=3>",
  //   "<x=9, y=-8, z=-3>",
  // |];
  // let input = dummy_input;

  let moons = input |> Array.map(parseMoon);
  let periods = calculatePeriods(moons);

  let result = lcm(periods[0], lcm(periods[1], periods[2]));

  Js.log2("Part2 output: ", result);
  Js.log(periods);
};