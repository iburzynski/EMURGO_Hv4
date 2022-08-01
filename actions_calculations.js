// Session 2 Exercise: Introduction to Functional Thinking

// "Function" is an ambiguous term in programming
// It can refer to both "pure" (mathematical) functions and "impure" procedures that affect the world
// To help us distinguish the two, we introduced two new terms: Calculations and Actions

// A Calculation (pure function):
//   * has no effect on the program or the outside world besides outputting its return value
//   * given the same input values, always returns the same output
//     (because its return value depends only on its input(s))
// Any function that doesn't fulfill this criteria is an Action (impure)

// Readings:
// 1. Grokking Simplicity Chapter 1: https://livebook.manning.com/book/exploring-functional-programming/chapter-1/
// 2. Grokking Simplicity Chapter 2: https://livebook.manning.com/book/exploring-functional-programming/chapter-2/


// Problem Set: Distinguishing Actions & Calculations

// Let's test your understanding of Calculations (pure functions) vs. Actions (functions with effects).
// Classify each of the following JavaScript functions as either Calculation or Action, and explain why.
// Don't worry if you are unfamiliar with JavaScript!
// Just use your intuition and the provided info to reason about what each function is doing.

// 1.
function getDate() {
  return new Date().toDateString();
}
// Date() constructor reference:
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date

// Answer:
/* Explanation:

*/

// 2.
function formatDate (year, month, day) {
  return new Date(year, month, day).toDateString();
}

// Answer:
/* Explanation:

*/

// 3.
function toHex(n) {
  // Converts a decimal value to a hexadecimal value
  var hex = n.toString(16);
  return hex.padStart(2, "0");
}

// Answer:
/* Explanation:

*/

// 4.
function rgbToHex(red, green, blue) {
  return "#" + [toHex(red), toHex(green), toHex(blue)].join('');
}

// Answer:
/* Explanation:

*/

// 5.
function setFontColor(elementID, red, green, blue) {
  // Modifies the font color of an HTML element
  var hex = rgbToHex(red, green, blue);
  var colorMe = document.getElementById(elementID);
  colorMe.setAttribute("style", "color: " + hex);
}

// Answer:
/* Explanation:

*/

// 6.
async function fetchAPIData(url) {
  // Fetches JSON data from the provided URL
  var file = await fetch(url);
  return await file.json();
}

// Answer:
/* Explanation:

*/

// 7.
function writeJsonString(object) {
  // Converts a JavaScript object to a JSON string
  return JSON.stringify(object)
}

// Answer:
/* Explanation:

*/

// 8.
function exclusiveOr(a, b) {
  return (a || b) && !(a && b);
}

// Answer:
/* Explanation:

*/

// 9.
function makeTruthTable(operator) {
  var truthValues = [true, false];
  var table = [];
  for (var a of truthValues) {
    for (var b of truthValues) {
      var value = operator(a, b);
      table.push({ a, b, value });
    }
  }
  return table;
}

// Answer:
/* Explanation:

*/

// 10.
function printTruthTable(operator) {
  console.table(makeTruthTable(operator));
}

// Answer:
/* Explanation:

*/