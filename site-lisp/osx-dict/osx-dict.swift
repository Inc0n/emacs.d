#!/usr/bin/env swift

// go checkout
// https://github.com/odlp/dictionary-osx

import Foundation
import CoreServices

typealias Iter = (idx: Int, acc: String)

let args = CommandLine.arguments
if args.count == 1 {
    exit(1)
}

// func allIndexes(in string: String, where pred:(Character) -> Bool) -> [String.Index] {
//     var indexes: [String.Index] = []
//     while let index = string.firstIndex(where: pred) {
//         indexes.append(index)
//     }
//     return indexes
// }

func split(in string: inout String, by substr: String) -> String {
    if let range = string.range(of: substr) {
        let retstr = string[range.upperBound...]
        string = String(string[..<range.lowerBound])
        return String(retstr)
    } else {
        return ""
    }
}

let word = args[1]
if let output =
     DCSCopyTextDefinition(nil, word as NSString,
                           CFRangeMake(0, word.count)) {
    // how to parse osx dict
    // As simple as it is
    // each line is denoted by '.'.
    // Yes a simple period
    // find start
    // ① 9312 9331 ⑳

    var definition = output.takeRetainedValue() as String
    var phrases = split(in: &definition, by: "PHRASES")
    let origin = split(in: &phrases, by: "ORIGIN")
    let fn = { (iter: Iter, c: Character) -> Iter in
        if c.isWholeNumber && c.wholeNumberValue! == iter.idx {
            return (idx: iter.idx+1, acc: iter.acc + "\n" + String(c))
        } else {
            return (idx: iter.idx, acc: iter.acc + String(c))
        }
    }
    var formatted = definition.reduce((idx: 1, acc: ""), fn).acc
    let regex = try! NSRegularExpression(pattern: "( [^ ]+[\\.。])")
    let repl = "$1\n   "
    formatted = regex.stringByReplacingMatches(in: formatted, options: [], range: NSRange(0..<definition.utf16.count), withTemplate: repl)
    print(formatted)
}
// else {
//     exit(1)
// }