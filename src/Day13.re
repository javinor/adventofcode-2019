let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync;

type tile =
  | Empty
  | Wall
  | Block
  | Paddle
  | Ball;

let parseTileId = n =>
  switch (n) {
  | 0 => Empty
  | 1 => Wall
  | 2 => Block
  | 3 => Paddle
  | 4 => Ball
  | tileId => failwith("invalid tileId: " ++ string_of_int(tileId))
  };

type coord = (int, int);
module Coords =
  Belt.Id.MakeComparable({
    type t = coord;
    let cmp = compare;
  });

type screen = {
  score: int,
  grid: Belt.Map.t(coord, tile, Coords.identity),
};

let screen_of_outputs = xs => {
  let rec go = (screen, xs) =>
    switch (xs) {
    | [] => screen
    | [x, y, tileId, ...rest] =>
      let x = int_of_float(x);
      let y = int_of_float(y);
      let tileId = int_of_float(tileId);

      if (x == (-1) && y == 0) {
        go({...screen, score: tileId}, rest);
      } else {
        let tile = parseTileId(tileId);
        let grid = Belt.Map.set(screen.grid, (x, y), tile);
        go({...screen, grid}, rest);
      };
    | _ => failwith("outputs length should be divisible by 3")
    };

  go({score: 0, grid: Belt.Map.make(~id=(module Coords))}, xs);
};

let consumeOutputs = (computer, inputs) => {
  let rec go = (computer, outputs) => {
    // Js.log2("inputs: ", inputs)
    let (computer, exitCode) = IntCodeComputer.run(computer, inputs);

    // switch (exitCode) {
    // | Halt => Js.log("Halt")
    // | Input => Js.log("Input")
    // | Output(v) => Js.log2("Output", v)
    // };

    switch (exitCode) {
    | Halt
    | Input => (computer, exitCode, outputs |> List.rev)
    | Output(value) => go(computer, [value, ...outputs])
    };
  };

  let (computer, exitCode, outputs) = go(computer, []);

  let screen = screen_of_outputs(outputs);
  (computer, exitCode, screen);
};

let printGrid = grid => {
  let coords = Belt.Map.keysToArray(grid);
  let (maxX, maxY) =
    coords
    |> Array.fold_left(
         ((maxX, maxY), (x, y)) => (max(maxX, x), max(maxY, y)),
         (min_int, min_int),
       );

  // Js.log((maxX, maxY));

  let grid' = Array.make_matrix(maxY + 1, maxX + 1, "");
  let () =
    Belt.Map.forEach(
      grid,
      ((x, y), tile) => {
        let str =
          switch (tile) {
          | Empty => " "
          | Wall => "W"
          | Block => "b"
          | Paddle => "="
          | Ball => "*"
          };

        grid'[y][x] = str;
      },
    );
  ();
  grid' |> Array.iter(row => row |> Js.Array.joinWith("") |> Js.log);
};

module Part1 = {
  let program = input |> Js.String.split(",") |> Array.map(float_of_string);

  let (_, _, screen) = consumeOutputs(IntCodeComputer.make(program), [||]);

  let result =
    screen.grid->Belt.Map.valuesToArray
    |> Array.fold_left(
         (nBlocks, tile) =>
           switch (tile) {
           | Block => nBlocks + 1
           | Empty
           | Wall
           | Paddle
           | Ball => nBlocks
           },
         0,
       );

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  type joystickPosition =
    | Neutral
    | Left
    | Right;

  let float_of_joystick = j =>
    switch (j) {
    | Neutral => 0.
    | Left => (-1.)
    | Right => 1.
    };

  let play = computer => {
    let rec go = (computer, inputs, screen) => {
      let (computer, exitCode, screen') = consumeOutputs(computer, inputs);
      Js.log2("screen'.grid: ", screen'.grid |> Belt.Map.toArray)


      let nextScreen = {
        score: screen'.score,
        grid:
          Belt.Map.merge(screen.grid, screen'.grid, (_, v1, v2) =>
            Js.Option.isNone(v2) ? v1 : v2
          ),
      };

      printGrid(nextScreen.grid);

      switch (exitCode) {
      | Halt => nextScreen
      | Output(_) => failwith("all output should have been consumed!")
      | Input =>
        let ((ballX, ballY), _) =
          Belt.Map.findFirstBy(nextScreen.grid, (_, tile) => tile == Ball)
          |> Js.Option.getExn;

        let ((paddleX, paddleY), _) =
          Belt.Map.findFirstBy(nextScreen.grid, (_, tile) => tile == Paddle)
          |> Js.Option.getExn;

        let joystick =
          ballX < paddleX ? Left : ballX > paddleX ? Right : Neutral;

        Js.log2("ball: ", (ballX, ballY))
        Js.log2("paddle: ", (paddleX, paddleY))
        go(computer, [|float_of_joystick(joystick)|], nextScreen);
      };
    };

    go(
      computer,
      [||],
      {score: 0, grid: Belt.Map.make(~id=(module Coords))},
    );
  };

  let program = input |> Js.String.split(",") |> Array.map(float_of_string);
  program[0] = 2.;

  let screen = play(IntCodeComputer.make(program));

  let result = screen.score;
  Js.log2("Part2 result: ", result);
};