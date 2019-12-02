let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";

let dummy_input = "1,9,10,3,2,3,11,0,99,30,40,50";

module Part1 = {
  let input = inputPath |> Node.Fs.readFileAsUtf8Sync;
  // let input = dummy_input;

  let program = input |> Js.String.split(",") |> Array.map(int_of_string);

  let runProgram = program => {
    let rec go = (instructionPointer, program) => {
      switch (program[instructionPointer]) {
      | 1 =>
        let address1 = program[instructionPointer + 1];
        let address2 = program[instructionPointer + 2];
        let address3 = program[instructionPointer + 3];
        program[address3] = program[address1] + program[address2];
        go(instructionPointer + 4, program);
      | 2 =>
        let address1 = program[instructionPointer + 1];
        let address2 = program[instructionPointer + 2];
        let address3 = program[instructionPointer + 3];
        program[address3] = program[address1] * program[address2];
        go(instructionPointer + 4, program);
      | 99 => program
      | opcode => failwith("invalid opcode: " ++ string_of_int(opcode))
      };
    };

    go(0, Array.copy(program));
  };

  program[1] = 12;
  program[2] = 2;
  let result = runProgram(program);

  Js.log2("Part1 result: ", result[0]);
};

module Part2 = {
  let originalProgram =
    inputPath
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String.split(",")
    |> Array.map(int_of_string);

  let result = ref((0, 0));
  let keepWorking = ref(true);
  let expectedOutput = 19690720;

  for (noun in 0 to 99) {
    for (verb in 0 to 99) {
      if (keepWorking^) {
        let program = Array.copy(originalProgram);
        program[1] = noun;
        program[2] = verb;
        let output = Part1.runProgram(program);
        if (output[0] == expectedOutput) {
          keepWorking := false;
          result := (noun, verb);
        };
      };
    };
  };

  Js.log2("Part2 result: ", result);
};