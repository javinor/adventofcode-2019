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

module Part1 = {
  let runProgram = program => {
    let rec go = (instructionPointer, program) => {
      let instruction = program[instructionPointer];
      let opcode = parseOpcode(instruction);
      let modes = parseModes(instruction);

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
        program[resultAddress] = 1; // HARD CODED INPUT
        go(instructionPointer + 2, program);
      | 4 =>
        let value = getByMode(program, instructionPointer + 1, modes[0]);
        Js.log2("opcode 4: ", value);
        go(instructionPointer + 2, program);
      | 99 => program
      | opcode => failwith("invalid opcode: " ++ string_of_int(opcode))
      };
    };

    go(0, Array.copy(program));
  };

  let result =
    input |> Js.String.split(",") |> Array.map(int_of_string) |> runProgram;

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  let userInput = 5;

  let runProgram = program => {
    let rec go = (instructionPointer, program) => {
      let instruction = program[instructionPointer];
      let opcode = parseOpcode(instruction);
      let modes = parseModes(instruction);

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
        program[resultAddress] = userInput; // HARD CODED INPUT
        go(instructionPointer + 2, program);
      | 4 =>
        let value = getByMode(program, instructionPointer + 1, modes[0]);
        Js.log2("opcode 4: ", value);
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
      | 99 => program
      | opcode => failwith("invalid opcode: " ++ string_of_int(opcode))
      };
    };

    go(0, Array.copy(program));
  };

  // let input = "3,9,8,9,10,9,4,9,99,-1,8";
  // let input = "3,9,7,9,10,9,4,9,99,-1,8";
  // let input = "3,3,1108,-1,8,3,4,3,99";
  // let input = "3,3,1107,-1,8,3,4,3,99";
  // let input = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9";
  // let input = "3,3,1105,-1,9,1101,0,0,12,4,12,99,1";
  // let input = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99";

  let result =
    input |> Js.String.split(",") |> Array.map(int_of_string) |> runProgram;

  Js.log2("Part2 result: ", "TBD");
};