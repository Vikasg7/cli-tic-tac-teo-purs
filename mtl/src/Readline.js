const readline = require("readline")

exports.createInterface_ = function () {
  return readline.createInterface({
    input: process.stdin,
    output: process.stdout
  })
}

exports.question_ = function (rl) {
  return function (q) {
    return function (cb) {
      return function () {
        rl.question(q, function (a) {
          cb(a)()
        })
      }
    }
  }
}

exports.closeInterface_ = function (rl) {
  return function () {
    rl.close()
  }
}