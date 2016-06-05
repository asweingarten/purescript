"use strict";

// module ReadLine

var prompt = require('prompt-sync')();

exports.readLine = function() {
  return prompt('make a move: ', 'Red 1');
};



