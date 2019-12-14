let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync;

module HullPaintingRobot: {
  type t;
  type color =
    | Black
    | White;
  type rotate =
    | Left
    | Right;

  let make: color => t;
  let detectColor: t => color;
  let paintAndMove: (t, color, rotate) => t;
  let countPaintedPanels: t => int;
  let printHull: t => unit;
} = {
  type coord = (int, int);
  type color =
    | Black
    | White;
  type rotate =
    | Left
    | Right;
  type direction =
    | Up
    | Right
    | Down
    | Left;

  module Coords =
    Belt.Id.MakeComparable({
      type t = coord;
      let cmp = compare;
    });

  type hull = Belt.Map.t(coord, color, Coords.identity);
  type robot = (coord, direction);

  type t = {
    hull,
    robot,
  };

  let make = color => {
    hull: Belt.Map.fromArray([|((0, 0), color)|], ~id=(module Coords)),
    robot: ((0, 0), Up),
  };

  let detectColor = ({hull, robot}) => {
    let coords = fst(robot);
    Belt.Map.getWithDefault(hull, coords, Black);
  };

  let moveRobot = (robot, rotate: rotate) => {
    let ((x, y), direction) = robot;
    let newDirection =
      switch (rotate, direction) {
      | (Left, Up) => Left
      | (Left, Right) => Up
      | (Left, Down) => Right
      | (Left, Left) => Down
      | (Right, Up) => Right
      | (Right, Right) => Down
      | (Right, Down) => Left
      | (Right, Left) => Up
      };

    let newCoords =
      switch (newDirection) {
      | Up => (x, y + 1)
      | Right => (x + 1, y)
      | Down => (x, y - 1)
      | Left => (x - 1, y)
      };

    // Js.log2("from: ", robot);
    // Js.log2("to: ", (newCoords, newDirection));

    (newCoords, newDirection);
  };

  let paintAndMove = ({hull, robot}, color, rotate) => {
    let coords = fst(robot);
    let hull = Belt.Map.set(hull, coords, color);
    let robot = moveRobot(robot, rotate);

    {hull, robot};
  };

  let countPaintedPanels = ({hull}) => Belt.Map.size(hull);

  let printHull = ({hull, robot: _}) => {
    let coords = Belt.Map.keysToArray(hull);
    let (minX, maxX, minY, maxY) =
      coords
      |> Array.fold_left(
           ((minX, maxX, minY, maxY), (x, y)) =>
             (min(minX, x), max(maxX, x), min(minY, y), max(maxY, y)),
           (max_int, min_int, max_int, min_int),
         );

    let grid = Array.make_matrix(maxX - minX + 1, maxY - minY + 1, ".");

    // let ((x, y), direction) = robot;
    // if (x - minX >= 0 && y - minY >= 0) {
    //   grid[x - minX][y - minY] = (
    //     switch (direction) {
    //     | Up => "^"
    //     | Right => ">"
    //     | Down => "v"
    //     | Left => "<"
    //     }
    //   );
    // };

    coords
    |> Array.iter(((x, y) as coord) => {
         let color = Belt.Map.getExn(hull, coord);
         grid[x - minX][y - minY] = (
           switch (color) {
           | Black => "."
           | White => "#"
           }
         );
       });

    Array.iter(row => row |> Js.Array.joinWith("") |> Js.log, grid);
  };
};

module Part1 = {
  module HPR = HullPaintingRobot;

  let program = input |> Js.String.split(",") |> Array.map(float_of_string);
  let break = ref(false);
  let computer = ref(IntCodeComputer.make(program));
  let robot = ref(HullPaintingRobot.make(HPR.Black));

  while (! break^) {
    let color =
      switch (HPR.detectColor(robot^)) {
      | Black => 0.
      | White => 1.
      };

    let (computer', exitCode1) = IntCodeComputer.run(computer^, [|color|]);

    switch (exitCode1) {
    | Halt => break := true
    | Input => failwith("program should not request input here!")
    | Output(value) =>
      let newColor = value == 0. ? HPR.Black : White;
      let (computer'', exitCode2) = IntCodeComputer.run(computer', [||]);

      computer := computer'';

      switch (exitCode2) {
      | Halt => failwith("should not halt!")
      | Input => failwith("should not request input!")
      | Output(value) =>
        let rotate = value == 0. ? HPR.Left : Right;

        robot := HPR.paintAndMove(robot^, newColor, rotate);
      };
    };
  };

  let result = HPR.countPaintedPanels(robot^);
  Js.log2("Part1 result: ", result);
};

module Part2 = {
  module HPR = HullPaintingRobot;

  let program = input |> Js.String.split(",") |> Array.map(float_of_string);
  let break = ref(false);
  let computer = ref(IntCodeComputer.make(program));
  let robot = ref(HullPaintingRobot.make(HPR.White));

  while (! break^) {
    let color =
      switch (HPR.detectColor(robot^)) {
      | Black => 0.
      | White => 1.
      };

    let (computer', exitCode1) = IntCodeComputer.run(computer^, [|color|]);

    switch (exitCode1) {
    | Halt => break := true
    | Input => failwith("program should not request input here!")
    | Output(value) =>
      let newColor = value == 0. ? HPR.Black : White;
      let (computer'', exitCode2) = IntCodeComputer.run(computer', [||]);

      computer := computer'';

      switch (exitCode2) {
      | Halt => failwith("should not halt!")
      | Input => failwith("should not request input!")
      | Output(value) =>
        let rotate = value == 0. ? HPR.Left : Right;

        robot := HPR.paintAndMove(robot^, newColor, rotate);
      };
    };
  };


  let result = HPR.countPaintedPanels(robot^);
  Js.log2("Part2 result:", result);
  HPR.printHull(robot^)
};