// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Path = require("path");
var $$Array = require("bs-platform/lib/js/array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

var inputPath = "./src/" + (Path.parse("Day02.re").name + ".input");

var input = Fs.readFileSync(inputPath, "utf8");

var program = $$Array.map(Caml_format.caml_int_of_string, input.split(","));

function runProgram(program) {
  var _instructionPointer = 0;
  var program$1 = $$Array.copy(program);
  while(true) {
    var instructionPointer = _instructionPointer;
    var opcode = Caml_array.caml_array_get(program$1, instructionPointer);
    var switcher = opcode - 1 | 0;
    if (switcher === 0 || switcher === 1) {
      if (switcher !== 0) {
        var address1 = Caml_array.caml_array_get(program$1, instructionPointer + 1 | 0);
        var address2 = Caml_array.caml_array_get(program$1, instructionPointer + 2 | 0);
        var address3 = Caml_array.caml_array_get(program$1, instructionPointer + 3 | 0);
        Caml_array.caml_array_set(program$1, address3, Caml_int32.imul(Caml_array.caml_array_get(program$1, address1), Caml_array.caml_array_get(program$1, address2)));
        _instructionPointer = instructionPointer + 4 | 0;
        continue ;
      } else {
        var address1$1 = Caml_array.caml_array_get(program$1, instructionPointer + 1 | 0);
        var address2$1 = Caml_array.caml_array_get(program$1, instructionPointer + 2 | 0);
        var address3$1 = Caml_array.caml_array_get(program$1, instructionPointer + 3 | 0);
        Caml_array.caml_array_set(program$1, address3$1, Caml_array.caml_array_get(program$1, address1$1) + Caml_array.caml_array_get(program$1, address2$1) | 0);
        _instructionPointer = instructionPointer + 4 | 0;
        continue ;
      }
    } else if (switcher !== 98) {
      return Pervasives.failwith("invalid opcode: " + String(opcode));
    } else {
      return program$1;
    }
  };
}

Caml_array.caml_array_set(program, 1, 12);

Caml_array.caml_array_set(program, 2, 2);

var result = runProgram(program);

console.log("Part1 result: ", Caml_array.caml_array_get(result, 0));

var Part1 = /* module */[
  /* input */input,
  /* program */program,
  /* runProgram */runProgram,
  /* result */result
];

var originalProgram = $$Array.map(Caml_format.caml_int_of_string, Fs.readFileSync(inputPath, "utf8").split(","));

var result$1 = /* record */[/* contents : tuple */[
    0,
    0
  ]];

var keepWorking = /* record */[/* contents */true];

for(var noun = 0; noun <= 99; ++noun){
  for(var verb = 0; verb <= 99; ++verb){
    if (keepWorking[0]) {
      var program$1 = $$Array.copy(originalProgram);
      Caml_array.caml_array_set(program$1, 1, noun);
      Caml_array.caml_array_set(program$1, 2, verb);
      var output = runProgram(program$1);
      if (Caml_array.caml_array_get(output, 0) === 19690720) {
        keepWorking[0] = false;
        result$1[0] = /* tuple */[
          noun,
          verb
        ];
      }
      
    }
    
  }
}

console.log("Part2 result: ", result$1);

var Part2 = /* module */[
  /* originalProgram */originalProgram,
  /* result */result$1,
  /* keepWorking */keepWorking,
  /* expectedOutput */19690720
];

var dummy_input = "1,9,10,3,2,3,11,0,99,30,40,50";

exports.inputPath = inputPath;
exports.dummy_input = dummy_input;
exports.Part1 = Part1;
exports.Part2 = Part2;
/* inputPath Not a pure module */