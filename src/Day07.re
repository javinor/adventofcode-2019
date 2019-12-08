let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync;

type mode =
  | Position
  | Immediate;

let toDigits = n => {
  let rec go = (acc, n) => n == 0 ? acc : go([n mod 10, ...acc], n / 10);

  go([], n);
};

let rec padWithZeroes = (nums, size) => {
  List.length(nums) == size ? nums : padWithZeroes([0, ...nums], size);
};

let parseOpcode = instruction => instruction mod 100;
let parseModes = instruction => {
  let digits = toDigits(instruction / 100);
  padWithZeroes(digits, 3)
  |> Array.of_list
  |> Js.Array.reverseInPlace
  |> Array.map(n =>
       switch (n) {
       | 0 => Position
       | 1 => Immediate
       | _ => failwith("invalid input - unknown parameter mode")
       }
     );
};

let getByMode = (program, address, mode) => {
  let parameter = program[address];
  switch (mode) {
  | Position => program[parameter]
  | Immediate => parameter
  };
};

let runProgram = (program, inputs) => {
  let inputs = Array.copy(inputs);

  let rec go = (instructionPointer, program) => {
    let instruction = program[instructionPointer];
    let opcode = parseOpcode(instruction);
    let modes = parseModes(instruction);

    // Js.log2("instruction, opcode, modes, inputs, outputs: ", (instruction, opcode, modes, inputs, outputs))
    switch (opcode) {
    | 1 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      let resultAddress = program[instructionPointer + 3];
      program[resultAddress] = value1 + value2;
      go(instructionPointer + 4, program);
    | 2 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      let resultAddress = program[instructionPointer + 3];
      program[resultAddress] = value1 * value2;
      go(instructionPointer + 4, program);
    | 3 =>
      let resultAddress = program[instructionPointer + 1];
      program[resultAddress] = Js.Array.shift(inputs) |> Js.Option.getExn;
      go(instructionPointer + 2, program);
    | 4 =>
      let value = getByMode(program, instructionPointer + 1, modes[0]);
      let _ = Js.Array.push(value, inputs);
      go(instructionPointer + 2, program);
    | 5 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      let nextInstructionPointer =
        value1 != 0 ? value2 : instructionPointer + 3;
      go(nextInstructionPointer, program);
    | 6 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      let nextInstructionPointer =
        value1 == 0 ? value2 : instructionPointer + 3;
      go(nextInstructionPointer, program);
    | 7 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      let resultAddress = program[instructionPointer + 3];
      program[resultAddress] = value1 < value2 ? 1 : 0;
      go(instructionPointer + 4, program);
    | 8 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      let resultAddress = program[instructionPointer + 3];
      program[resultAddress] = value1 == value2 ? 1 : 0;
      go(instructionPointer + 4, program);
    | 99 => (program, inputs)
    | opcode => failwith("invalid opcode: " ++ string_of_int(opcode))
    };
  };

  go(0, Array.copy(program));
};

let permutations = xs => {
  let results: array(array(int)) = [||];

  let rec go = (acc, xs) =>
    if (Belt.Set.Int.size(xs) == 0) {
      let _ = Js.Array.push(acc |> Array.of_list, results);
      ();
    } else {
      Belt.Set.Int.forEach(xs, x =>
        go([x, ...acc], Belt.Set.Int.remove(xs, x))
      );
    };

  go([], Belt.Set.Int.fromArray(xs));
  results;
};

module Part1 = {
  // Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):
  // let input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
  // Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):
  // let input = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0";
  // Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):
  // let input = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0";

  let program = input |> Js.String.split(",") |> Array.map(int_of_string);

  let settingSequences = permutations(Belt.Array.range(0, 4));

  let runSequence = (seq: array(int)) => {
    let value = ref(0);

    seq
    |> Array.iter(setting => {
         let (_, outputs) = runProgram(program, [|setting, value^|]);
         value := outputs[0];
       });

    value^;
  };

  let result =
    settingSequences
    |> Array.map(runSequence)
    |> Array.fold_left(max, min_int);

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  // Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5):
  // let input = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";
  // Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6):
  // let input = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10";

  let initialProgram =
    input |> Js.String.split(",") |> Array.map(int_of_string);

  // let settingSequences = permutations(Belt.Array.range(5, 9));

  let runSequence = (seq: array(int)) => {
    let value = ref(0);
    let program = ref(initialProgram);

    seq
    |> Array.iter(setting => {
        //  Js.log3("[|setting, value^|] : ", setting, value^);
         let (_, outputs) =
           runProgram(program^, [|setting, value^|]);
        //  Js.log3("program', outputs : ", program', outputs);
         value := outputs[0];
        //  program := program';
       });

    Js.log2("value: ", value)
    value^;
  };

  runSequence([|9, 7, 8, 5, 6|]);

  Js.log2("Part2 result: ", "TBD");
};