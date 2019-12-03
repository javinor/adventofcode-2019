let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";

let dummy_input = [|"R8,U5,L5,D3", "U7,R6,D4,L4"|];

type direction =
  | Up
  | Right
  | Down
  | Left;

module Points =
  Belt.Id.MakeComparable({
    type t = (int, int);
    let cmp = compare;
  });

let parseWire = str => {
  str
  |> Js.String.split(",")
  |> Array.map(segment => {
       let direction =
         switch (segment.[0]) {
         | 'U' => Up
         | 'R' => Right
         | 'D' => Down
         | 'L' => Left
         | _ => failwith("bad input - unknown direction")
         };

       let magnitude =
         segment |> Js.String.sliceToEnd(~from=1) |> int_of_string;
       (direction, magnitude);
     });
};

let dist = ((u, v), (x, y)) => abs(u - x) + abs(v - y);

module Part1 = {
  let wireToPoints = wire => {
    let rec go = (acc, wire, (x, y)) => {
      switch (wire) {
      | [] => acc
      | [(_, 0), ...rest] => go(acc, rest, (x, y))
      | [(direction, n), ...rest] =>
        let p =
          switch (direction) {
          | Up => (x, y + 1)
          | Right => (x + 1, y)
          | Down => (x, y - 1)
          | Left => (x - 1, y)
          };

        go(Belt.Set.add(acc, p), [(direction, n - 1), ...rest], p);
      };
    };

    let empty = Belt.Set.make(~id=(module Points));
    go(empty, Array.to_list(wire), (0, 0));
  };

  let points =
    inputPath
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String.split("\n")
    |> Array.map(parseWire)
    |> Array.map(wireToPoints);

  let intersection = Belt.Set.intersect(points[0], points[1]);

  let result =
    Belt.Set.toArray(intersection) |> Array.map(p => (p, dist(p, (0, 0))));

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  let wireToPath = wire => {
    let rec go = (acc, wire, (x, y), steps) => {
      switch (wire) {
      | [] => acc
      | [(_, 0), ...rest] => go(acc, rest, (x, y), steps)
      | [(direction, n), ...rest] =>
        let nextP =
          switch (direction) {
          | Up => (x, y + 1)
          | Right => (x + 1, y)
          | Down => (x, y - 1)
          | Left => (x - 1, y)
          };

        let nextAcc =
          Belt.Map.update(acc, nextP, prev =>
            switch (prev) {
            | None => Some(steps + 1)
            | _ => prev
            }
          );
        go(nextAcc, [(direction, n - 1), ...rest], nextP, steps + 1);
      };
    };

    let empty = Belt.Map.make(~id=(module Points));
    go(empty, Array.to_list(wire), (0, 0), 0);
  };

  let input =
    inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");
  // let input = dummy_input;

  let paths = input |> Array.map(parseWire) |> Array.map(wireToPath);
  let intersection =
    Belt.Map.merge(paths[0], paths[1], (_, steps, steps') =>
      switch (steps, steps') {
      | (None, _)
      | (_, None) => None
      | (Some(n), Some(n')) => Some(n + n')
      }
    );

  Js.log2("Part2 result: ", intersection |> Belt.Map.toArray);
};