// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var List = require("bs-platform/lib/js/list.js");
var Path = require("path");
var $$Array = require("bs-platform/lib/js/array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

var inputPath = "./src/" + (Path.parse("Day05.re").name + ".input");

var input = Fs.readFileSync(inputPath, "utf8");

function toDigits(n) {
  var _acc = /* [] */0;
  var _n = n;
  while(true) {
    var n$1 = _n;
    var acc = _acc;
    var match = n$1 === 0;
    if (match) {
      return acc;
    } else {
      _n = n$1 / 10 | 0;
      _acc = /* :: */[
        n$1 % 10,
        acc
      ];
      continue ;
    }
  };
}

function padWithZeroes(_nums, size) {
  while(true) {
    var nums = _nums;
    var match = List.length(nums) === size;
    if (match) {
      return nums;
    } else {
      _nums = /* :: */[
        0,
        nums
      ];
      continue ;
    }
  };
}

function parseOpcode(instruction) {
  return instruction % 100;
}

function parseModes(instruction) {
  var digits = toDigits(instruction / 100 | 0);
  return $$Array.map((function (n) {
                if (n !== 0) {
                  if (n !== 1) {
                    return Pervasives.failwith("invalid input - unknown parameter mode");
                  } else {
                    return /* Immediate */1;
                  }
                } else {
                  return /* Position */0;
                }
              }), $$Array.of_list(padWithZeroes(digits, 3)).reverse());
}

function getByMode(program, address, mode) {
  var parameter = Caml_array.caml_array_get(program, address);
  if (mode) {
    return parameter;
  } else {
    return Caml_array.caml_array_get(program, parameter);
  }
}

function runProgram(program) {
  var _instructionPointer = 0;
  var program$1 = $$Array.copy(program);
  while(true) {
    var instructionPointer = _instructionPointer;
    var instruction = Caml_array.caml_array_get(program$1, instructionPointer);
    var opcode = instruction % 100;
    var modes = parseModes(instruction);
    if (opcode >= 5) {
      if (opcode !== 99) {
        return Pervasives.failwith("invalid opcode: " + String(opcode));
      } else {
        return program$1;
      }
    } else if (opcode > 0) {
      switch (opcode - 1 | 0) {
        case 0 : 
            var value1 = getByMode(program$1, instructionPointer + 1 | 0, Caml_array.caml_array_get(modes, 0));
            var value2 = getByMode(program$1, instructionPointer + 2 | 0, Caml_array.caml_array_get(modes, 1));
            var resultAddress = Caml_array.caml_array_get(program$1, instructionPointer + 3 | 0);
            Caml_array.caml_array_set(program$1, resultAddress, value1 + value2 | 0);
            _instructionPointer = instructionPointer + 4 | 0;
            continue ;
        case 1 : 
            var value1$1 = getByMode(program$1, instructionPointer + 1 | 0, Caml_array.caml_array_get(modes, 0));
            var value2$1 = getByMode(program$1, instructionPointer + 2 | 0, Caml_array.caml_array_get(modes, 1));
            var resultAddress$1 = Caml_array.caml_array_get(program$1, instructionPointer + 3 | 0);
            Caml_array.caml_array_set(program$1, resultAddress$1, Caml_int32.imul(value1$1, value2$1));
            _instructionPointer = instructionPointer + 4 | 0;
            continue ;
        case 2 : 
            var resultAddress$2 = Caml_array.caml_array_get(program$1, instructionPointer + 1 | 0);
            Caml_array.caml_array_set(program$1, resultAddress$2, 1);
            _instructionPointer = instructionPointer + 2 | 0;
            continue ;
        case 3 : 
            var value = getByMode(program$1, instructionPointer + 1 | 0, Caml_array.caml_array_get(modes, 0));
            console.log("opcode 4: ", value);
            _instructionPointer = instructionPointer + 2 | 0;
            continue ;
        
      }
    } else {
      return Pervasives.failwith("invalid opcode: " + String(opcode));
    }
  };
}

var result = runProgram($$Array.map(Caml_format.caml_int_of_string, input.split(",")));

console.log("Part1 result: ", result);

var Part1 = /* module */[
  /* runProgram */runProgram,
  /* result */result
];

function runProgram$1(program) {
  var _instructionPointer = 0;
  var program$1 = $$Array.copy(program);
  while(true) {
    var instructionPointer = _instructionPointer;
    var instruction = Caml_array.caml_array_get(program$1, instructionPointer);
    var opcode = instruction % 100;
    var modes = parseModes(instruction);
    if (opcode >= 9) {
      if (opcode !== 99) {
        return Pervasives.failwith("invalid opcode: " + String(opcode));
      } else {
        return program$1;
      }
    } else if (opcode > 0) {
      switch (opcode - 1 | 0) {
        case 0 : 
            var value1 = getByMode(program$1, instructionPointer + 1 | 0, Caml_array.caml_array_get(modes, 0));
            var value2 = getByMode(program$1, instructionPointer + 2 | 0, Caml_array.caml_array_get(modes, 1));
            var resultAddress = Caml_array.caml_array_get(program$1, instructionPointer + 3 | 0);
            Caml_array.caml_array_set(program$1, resultAddress, value1 + value2 | 0);
            _instructionPointer = instructionPointer + 4 | 0;
            continue ;
        case 1 : 
            var value1$1 = getByMode(program$1, instructionPointer + 1 | 0, Caml_array.caml_array_get(modes, 0));
            var value2$1 = getByMode(program$1, instructionPointer + 2 | 0, Caml_array.caml_array_get(modes, 1));
            var resultAddress$1 = Caml_array.caml_array_get(program$1, instructionPointer + 3 | 0);
            Caml_array.caml_array_set(program$1, resultAddress$1, Caml_int32.imul(value1$1, value2$1));
            _instructionPointer = instructionPointer + 4 | 0;
            continue ;
        case 2 : 
            var resultAddress$2 = Caml_array.caml_array_get(program$1, instructionPointer + 1 | 0);
            Caml_array.caml_array_set(program$1, resultAddress$2, 5);
            _instructionPointer = instructionPointer + 2 | 0;
            continue ;
        case 3 : 
            var value = getByMode(program$1, instructionPointer + 1 | 0, Caml_array.caml_array_get(modes, 0));
            console.log("opcode 4: ", value);
            _instructionPointer = instructionPointer + 2 | 0;
            continue ;
        case 4 : 
            var value1$2 = getByMode(program$1, instructionPointer + 1 | 0, Caml_array.caml_array_get(modes, 0));
            var value2$2 = getByMode(program$1, instructionPointer + 2 | 0, Caml_array.caml_array_get(modes, 1));
            var match = value1$2 !== 0;
            var nextInstructionPointer = match ? value2$2 : instructionPointer + 3 | 0;
            _instructionPointer = nextInstructionPointer;
            continue ;
        case 5 : 
            var value1$3 = getByMode(program$1, instructionPointer + 1 | 0, Caml_array.caml_array_get(modes, 0));
            var value2$3 = getByMode(program$1, instructionPointer + 2 | 0, Caml_array.caml_array_get(modes, 1));
            var match$1 = value1$3 === 0;
            var nextInstructionPointer$1 = match$1 ? value2$3 : instructionPointer + 3 | 0;
            _instructionPointer = nextInstructionPointer$1;
            continue ;
        case 6 : 
            var value1$4 = getByMode(program$1, instructionPointer + 1 | 0, Caml_array.caml_array_get(modes, 0));
            var value2$4 = getByMode(program$1, instructionPointer + 2 | 0, Caml_array.caml_array_get(modes, 1));
            var resultAddress$3 = Caml_array.caml_array_get(program$1, instructionPointer + 3 | 0);
            var match$2 = value1$4 < value2$4;
            Caml_array.caml_array_set(program$1, resultAddress$3, match$2 ? 1 : 0);
            _instructionPointer = instructionPointer + 4 | 0;
            continue ;
        case 7 : 
            var value1$5 = getByMode(program$1, instructionPointer + 1 | 0, Caml_array.caml_array_get(modes, 0));
            var value2$5 = getByMode(program$1, instructionPointer + 2 | 0, Caml_array.caml_array_get(modes, 1));
            var resultAddress$4 = Caml_array.caml_array_get(program$1, instructionPointer + 3 | 0);
            var match$3 = value1$5 === value2$5;
            Caml_array.caml_array_set(program$1, resultAddress$4, match$3 ? 1 : 0);
            _instructionPointer = instructionPointer + 4 | 0;
            continue ;
        
      }
    } else {
      return Pervasives.failwith("invalid opcode: " + String(opcode));
    }
  };
}

var result$1 = runProgram$1($$Array.map(Caml_format.caml_int_of_string, input.split(",")));

console.log("Part2 result: ", "TBD");

var Part2 = /* module */[
  /* userInput */5,
  /* runProgram */runProgram$1,
  /* result */result$1
];

exports.inputPath = inputPath;
exports.input = input;
exports.toDigits = toDigits;
exports.padWithZeroes = padWithZeroes;
exports.parseOpcode = parseOpcode;
exports.parseModes = parseModes;
exports.getByMode = getByMode;
exports.Part1 = Part1;
exports.Part2 = Part2;
/* inputPath Not a pure module */