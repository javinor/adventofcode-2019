type tape = array(float);
type instructionPointer = int;
type relativeBase = int;
type t = (tape, instructionPointer, relativeBase);

type exitCode =
  | Halt
  | Input
  | Output(float);

let make = (~memSize: int=10000, program) => {
  let tape = Array.make(memSize, 0.);
  Array.blit(program, 0, tape, 0, Array.length(program));
  (tape, 0, 0);
};

type opCode =
  | Add
  | Multiply
  | Input
  | Output
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | AdjuestRelativeBase
  | Halt;

type mode =
  | Position
  | Immediate
  | Relative(int);

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

let parseInstruction = ((tape, instructionPointer, relativeBase)) => {
  let parseOpcode = instruction =>
    switch (instruction mod 100) {
    | 1 => Add
    | 2 => Multiply
    | 3 => Input
    | 4 => Output
    | 5 => JumpIfTrue
    | 6 => JumpIfFalse
    | 7 => LessThan
    | 8 => Equals
    | 9 => AdjuestRelativeBase
    | 99 => Halt
    | opcode => failwith("unknown opcode: " ++ string_of_int(opcode))
    };

  let parseModes = (instruction, relativeBase) => {
    let toDigits = n => {
      let rec go = (acc, n) =>
        n == 0 ? acc : go([n mod 10, ...acc], n / 10);

      go([], n);
    };

    let rec padWithZeroes = (nums, size) => {
      List.length(nums) == size ? nums : padWithZeroes([0, ...nums], size);
    };

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

  let instruction = int_of_float(tape[instructionPointer]);
  let opcode = parseOpcode(instruction);
  let modes = parseModes(instruction, relativeBase);
  (opcode, modes);
};

let run = (computer, inputs) => {
  let rec tick =
          (
            (tape, instructionPointer, relativeBase) as computer: t,
            inputs: array(float),
          )
          : (t, exitCode) => {
    let (opcode, modes) = parseInstruction(computer);

    switch (opcode) {
    | Add =>
      let value1 = getByMode(tape, instructionPointer + 1, modes[0]);
      let value2 = getByMode(tape, instructionPointer + 2, modes[1]);
      setByMode(tape, instructionPointer + 3, modes[2], value1 +. value2);
      tick((tape, instructionPointer + 4, relativeBase), inputs);
    | Multiply =>
      let value1 = getByMode(tape, instructionPointer + 1, modes[0]);
      let value2 = getByMode(tape, instructionPointer + 2, modes[1]);
      setByMode(tape, instructionPointer + 3, modes[2], value1 *. value2);
      tick((tape, instructionPointer + 4, relativeBase), inputs);

    | Input =>
      switch (Js.Array.shift(inputs: array(float))) {
      | None => (computer, Input)
      | Some(input) =>
        setByMode(tape, instructionPointer + 1, modes[0], input);
        tick((tape, instructionPointer + 2, relativeBase), inputs);
      }
    | Output =>
      let value = getByMode(tape, instructionPointer + 1, modes[0]);
      ((tape, instructionPointer + 2, relativeBase), Output(value));
    | JumpIfTrue =>
      let value1 = getByMode(tape, instructionPointer + 1, modes[0]);
      let value2 = getByMode(tape, instructionPointer + 2, modes[1]);
      let nextInstructionPointer =
        value1 != 0. ? int_of_float(value2) : instructionPointer + 3;
      tick((tape, nextInstructionPointer, relativeBase), inputs);
    | JumpIfFalse =>
      let value1 = getByMode(tape, instructionPointer + 1, modes[0]);
      let value2 = getByMode(tape, instructionPointer + 2, modes[1]);
      let nextInstructionPointer =
        value1 == 0. ? int_of_float(value2) : instructionPointer + 3;
      tick((tape, nextInstructionPointer, relativeBase), inputs);

    | LessThan =>
      let value1 = getByMode(tape, instructionPointer + 1, modes[0]);
      let value2 = getByMode(tape, instructionPointer + 2, modes[1]);
      setByMode(
        tape,
        instructionPointer + 3,
        modes[2],
        value1 < value2 ? 1. : 0.,
      );

      tick((tape, instructionPointer + 4, relativeBase), inputs);
    | Equals =>
      let value1 = getByMode(tape, instructionPointer + 1, modes[0]);
      let value2 = getByMode(tape, instructionPointer + 2, modes[1]);
      setByMode(
        tape,
        instructionPointer + 3,
        modes[2],
        value1 == value2 ? 1. : 0.,
      );
      tick((tape, instructionPointer + 4, relativeBase), inputs);
    | AdjuestRelativeBase =>
      let value = getByMode(tape, instructionPointer + 1, modes[0]);
      let newRelativeBase = relativeBase + int_of_float(value);
      tick((tape, instructionPointer + 2, newRelativeBase), inputs);
    | Halt => (computer, Halt)
    };
  };

  tick(computer, Array.copy(inputs));
};