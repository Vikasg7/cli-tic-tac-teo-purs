const readline = require("readline")

exports.prompt_ = function (q) {
   const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
   })
   return function (cb) {
      return function () {
         rl.question(q, function (answer) {
            rl.close()
            cb(answer)
         })
      }
   }
}