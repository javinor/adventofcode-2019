type t;

type exitCode =
  | Halt
  | Input
  | Output(float);

let make: (~memSize: int=?, array(float)) => t;

let run: (t, array(float)) => (t, exitCode);