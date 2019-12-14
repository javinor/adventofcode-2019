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
  // let dummy_input = [|
  //   "<x=-1, y=0, z=2>",
  //   "<x=2, y=-10, z=-7>",
  //   "<x=4, y=-8, z=8>",
  //   "<x=3, y=5, z=-1>",
  // |];
  // let input = dummy_input;

  let result = "TBD";

  Js.log2("Part2 output: ", result);
};