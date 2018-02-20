'use strict'
exports.args =
  { bin : process.argv[0] || ""
  , path : process.argv[1] || ""
  , command : process.argv[2] || ""
  }

exports.logPretty = function (s) {
    return function () {
        try {
            console.log(JSON.stringify(JSON.parse(s), null, 2))
        } catch (e) {
            console.log(s)
        }
    }
}

exports._readJsonFromStdin = function (onError, onSuccess) {
    var data = ""
    process.stdin.on('data', function (chunk) {
        data += chunk.toString()
    })

    process.stdin.on('end', function () {
        var result
        try {
            result = JSON.parse(data)
        } catch (e) {
            return onError(e)
        }
        return onSuccess(result)
    })

    return function canceler (cancelError, cancelerError, cancelerSuccess) {
        return cancelerSuccess()
    }
}
