let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync;

type mode =
  | Position
  | Immediate
  | Relative(int);

let toDigits = n => {
  let rec go = (acc, n) => n == 0 ? acc : go([n mod 10, ...acc], n / 10);

  go([], n);
};

let rec padWithZeroes = (nums, size) => {
  List.length(nums) == size ? nums : padWithZeroes([0, ...nums], size);
};

let parseOpcode = instruction => instruction mod 100;
let parseModes = (instruction, relativeBase) => {
  let digits = toDigits(instruction / 100);
  padWithZeroes(digits, 3)
  |> Array.of_list
  |> Js.Array.reverseInPlace
  |> Array.map(n =>
       switch (n) {
       | 0 => Position
       | 1 => Immediate
       | 2 => Relative(relativeBase)
       | _ => failwith("invalid input - unknown parameter mode")
       }
     );
};

let getByMode = (program, address, mode) => {
  let parameter = program[address];
  switch (mode) {
  | Position => program[int_of_float(parameter)]
  | Immediate => parameter
  | Relative(base) => program[base + int_of_float(parameter)]
  };
};

let setByMode = (program, address, mode, value) => {
  let resultAddress = int_of_float(program[address]);
  switch (mode) {
  | Position => program[resultAddress] = value
  | Immediate => failwith("should not set in Immediate mode!")
  | Relative(base) => program[base + resultAddress] = value
  };
};

type code =
  | Halt
  | Input
  | Output(float);

let runProgram =
    (
      program: array(float),
      instructionPointer,
      relativeBase,
      inputs: array(float),
    ) => {
  let inputs = Array.copy(inputs);
  let relativeBase = ref(relativeBase);
  let program' = Array.make(10000, 0.);
  Array.blit(program, 0, program', 0, Array.length(program));

  let rec go = (instructionPointer, program: array(float)) => {
    let instruction = int_of_float(program[instructionPointer]);
    let opcode = parseOpcode(instruction);
    let modes = parseModes(instruction, relativeBase^);

    switch (opcode) {
    | 1 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      setByMode(program, instructionPointer + 3, modes[2], value1 +. value2);
      go(instructionPointer + 4, program);
    | 2 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      setByMode(program, instructionPointer + 3, modes[2], value1 *. value2);
      go(instructionPointer + 4, program);
    | 3 =>
      switch (Js.Array.shift(inputs: array(float))) {
      | None => (program, instructionPointer, relativeBase^, Input)
      | Some(input) =>
        setByMode(program, instructionPointer + 1, modes[0], input);
        go(instructionPointer + 2, program);
      }
    | 4 =>
      let value = getByMode(program, instructionPointer + 1, modes[0]);
      (program, instructionPointer + 2, relativeBase^, Output(value));
    | 5 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      let nextInstructionPointer =
        value1 != 0. ? int_of_float(value2) : instructionPointer + 3;
      go(nextInstructionPointer, program);
    | 6 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      let nextInstructionPointer =
        value1 == 0. ? int_of_float(value2) : instructionPointer + 3;
      go(nextInstructionPointer, program);
    | 7 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      setByMode(
        program,
        instructionPointer + 3,
        modes[2],
        value1 < value2 ? 1. : 0.,
      );

      go(instructionPointer + 4, program);
    | 8 =>
      let value1 = getByMode(program, instructionPointer + 1, modes[0]);
      let value2 = getByMode(program, instructionPointer + 2, modes[1]);
      setByMode(
        program,
        instructionPointer + 3,
        modes[2],
        value1 == value2 ? 1. : 0.,
      );
      go(instructionPointer + 4, program);
    | 9 =>
      let value = getByMode(program, instructionPointer + 1, modes[0]);
      relativeBase := relativeBase^ + int_of_float(value);
      go(instructionPointer + 2, program);
    | 99 => (program, instructionPointer, relativeBase^, Halt)
    | opcode => failwith("invalid opcode: " ++ string_of_int(opcode))
    };
  };

  go(instructionPointer, program');
};

module Part1 = {
  // let input = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99";
  // let input = "1102,34915192,34915192,7,4,7,99,0";
  // let input = "104,1125899906842624,99";

  let program = input |> Js.String.split(",") |> Array.map(float_of_string);

  let break = ref(false);
  let computer = ref((program, 0, 0));

  while (! break^) {
    let (prevProgram, prevInstructionPointer, prevRelativeBase) = computer^;
    let (program, instructionPointer, relativeBase, output) =
      runProgram(
        prevProgram,
        prevInstructionPointer,
        prevRelativeBase,
        [|1.|],
      );
    computer := (program, instructionPointer, relativeBase);

    switch (output) {
    | Halt => break := true
    | Input => failwith("program should finish with output")
    | Output(output) => Js.log3("Part1 output: ", output, "\n")
    };
  };
};

module Part2 = {
  let program = input |> Js.String.split(",") |> Array.map(float_of_string);

  let break = ref(false);
  let computer = ref((program, 0, 0));

  while (! break^) {
    let (prevProgram, prevInstructionPointer, prevRelativeBase) = computer^;
    let (program, instructionPointer, relativeBase, output) =
      runProgram(
        prevProgram,
        prevInstructionPointer,
        prevRelativeBase,
        [|2.|],
      );
    computer := (program, instructionPointer, relativeBase);

    switch (output) {
    | Halt => break := true
    | Input => failwith("program should finish with output")
    | Output(output) => Js.log3("Part2 output: ", output, "\n")
    };
  };
};