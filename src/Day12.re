let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");

type point =
  | Position(int, int, int)
  | Velocity(int, int, int);

let parsePoint = str => {
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
  | [|x, y, z|] => (Position(x, y, z), Velocity(0, 0, 0))
  | _ => failwith("failed to parse input: " ++ str)
  };
};

module Part1 = {
  let sign = n => n > 0 ? 1 : n < 0 ? (-1) : 0;

  let applyGravity = (p1, p2) => {
    let (Position(x1, y1, z1), Velocity(vx1, vy1, vz1)) = p1;
    let (Position(x2, y2, z2), Velocity(vx2, vy2, vz2)) = p2;

    let dvx1 = sign(compare(x2, x1));
    let dvy1 = sign(compare(y2, y1));
    let dvz1 = sign(compare(z2, z1));

    let dvx2 = (-1) * dvx1;
    let dvy2 = (-1) * dvy1;
    let dvz2 = (-1) * dvz1;

    let p1' = (
      Position(x1, y1, z1),
      Velocity(vx1 + dvx1, vy1 + dvy1, vz1 + dvz1),
    );
    let p2' = (
      Position(x2, y2, z2),
      Velocity(vx2 + dvx2, vy2 + dvy2, vz2 + dvz2),
    );

    (p1', p2');
  };

  let applyVelocity = p => {
    let (Position(x, y, z), Velocity(vx, vy, vz)) = p;
    let p' = (Position(x + vx, y + vy, z + vz), Velocity(vx, vy, vz));
    p';
  };

  let totalEnergy = p => {
    let (Position(x, y, z), Velocity(vx, vy, vz)) = p;
    let energy = coords =>
      coords |> Array.map(abs) |> Array.fold_left((+), 0);
    energy([|x, y, z|]) * energy([|vx, vy, vz|]);
  };

  let tick = (pvs, n) => {
    let rec go = (pvs, n) =>

      if (n == 0) {
        pvs;
      } else {
        for (i in 0 to Array.length(pvs) - 1) {
          for (j in i + 1 to Array.length(pvs) - 1) {
            let (p1, p2) = applyGravity(pvs[i], pvs[j]);
            pvs[i] = p1;
            pvs[j] = p2;
          };
        };

        let pvs' = pvs |> Array.map(applyVelocity);
        go(pvs', n - 1);
      };
    go(Array.copy(pvs), n);
  };

  // let dummy_input = [|
  //   "<x=-1, y=0, z=2>",
  //   "<x=2, y=-10, z=-7>",
  //   "<x=4, y=-8, z=8>",
  //   "<x=3, y=5, z=-1>",
  // |];
  // let input = dummy_input;

  let pvs = input |> Array.map(parsePoint);

  let result = tick(pvs, 1000) |> Array.map(totalEnergy) |> Array.fold_left((+), 0);
  Js.log2("Part1 output: ", result);
};

module Part2 = {
  let result = "TBD";

  Js.log2("Part2 output: ", result);
};