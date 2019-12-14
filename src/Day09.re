let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync;

module Part1 = {
  let program = input |> Js.String.split(",") |> Array.map(float_of_string);

  let break = ref(false);
  let computer = ref(IntCodeComputer.make(program));

  while (! break^) {
    let (computer', exitCode) = IntCodeComputer.run(computer^, [|1.|]);
    computer := computer';

    switch (exitCode) {
    | Halt => break := true
    | Input => failwith("program should finish with output")
    | Output(output) => Js.log3("Part1 output: ", output, "\n")
    };
  };
};

module Part2 = {
  let program = input |> Js.String.split(",") |> Array.map(float_of_string);

  let break = ref(false);
  let computer = ref(IntCodeComputer.make(program));

  while (! break^) {
    let (computer', exitCode) = IntCodeComputer.run(computer^, [|2.|]);
    computer := computer';

    switch (exitCode) {
    | Halt => break := true
    | Input => failwith("program should finish with output")
    | Output(output) => Js.log3("Part2 output: ", output, "\n")
    };
  };
};