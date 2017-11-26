//
// Parser_Tests.swift
// FootlessParser
//
// Released under the MIT License (MIT), http://opensource.org/licenses/MIT
//
// Copyright (c) 2015 NotTooBad Software. All rights reserved.
//

import FootlessParser
import XCTest

class Parser_Tests: XCTestCase {

	func testSingleTokenParser () {
		let parser = token(1)

		var input = [1]

		assertParseSucceeds(parser, &input, result: 1, consumed: 1)
		XCTAssert(input == [], "Input should be empty")
	}

	func testSeveralTokenParsers () {
		var input = Array("abc")

		for character in "abc" {
			let parser = token(character)
			assertParseSucceeds(parser, &input, result: character, consumed: 1)
		}
		XCTAssert(input == [], "Input should be empty")
	}

	func testFailingParserReturnsError () {
		let parser = token(2)

		assertParseFails(parser, [1])
	}

	func testTokensParser () {
		let parser = tokens([1,2,3,4])

		assertParseSucceeds(parser, [1,2,3,4,5], result: [1,2,3,4], consumed: 4)
		assertParseFails(parser, [])
		assertParseFails(parser, [1,2,3])
		assertParseFails(parser, [1,2,3,5])
	}

	func testStringTokensParser () {
		let parser = string("abc")

		assertParseSucceeds(parser, "abcde", result: "abc", consumed: 3)
		assertParseFails(parser, "")
		assertParseFails(parser, "ab")
		assertParseFails(parser, "abx")
	}

	func testEmptyStringTokensParser () {
		let parser = string("")

		assertParseSucceeds(parser, "abcde", result: "", consumed: 0)
	}

	func testAnyParser () {
		let parser: Parser<Character, Character> = any()

		var input = Array("abc")

		assertParseSucceeds(parser, &input, result: "a", consumed: 1)
		assertParseSucceeds(parser, &input, result: "b", consumed: 1)
	}

	func testOptionalParser () {
		let parser = optional( char("a"), otherwise: "x" )

		var input = Array("abc")

		assertParseSucceeds(parser, &input, result: "a", consumed: 1)
		assertParseSucceeds(parser, &input, result: "x", consumed: 0)
	}

	func testOneOrMoreParser () {
		let parser = oneOrMore(token(1))

		assertParseSucceeds(parser, [1], result: [1], consumed: 1)
		assertParseSucceeds(parser, [1,1,1], result: [1,1,1], consumed: 3)
		assertParseSucceeds(parser, [1,1,1,9], result: [1,1,1], consumed: 3)
	}

	func testZeroOrMoreParser () {
		let parser = zeroOrMore(token(1))

		assertParseSucceeds(parser, [], result: [])
		assertParseSucceeds(parser, [9], result: [], consumed: 0)
		assertParseSucceeds(parser, [1], result: [1])
		assertParseSucceeds(parser, [1,1,1], result: [1,1,1])
		assertParseSucceeds(parser, [1,1,1,9], result: [1,1,1], consumed: 3)
	}

	func testCountParser () {
		let parser = count(3, token(1))

		assertParseSucceeds(parser, [1,1,1,1], result: [1,1,1], consumed: 3)
		assertParseFails(parser, [1,1])
		assertParseFails(parser, [1,2,1])
		assertParseFails(parser, [])
	}

	func testCount1Parser () {
		let parser = count(1, token(1))

		assertParseSucceeds(parser, [1,1,1,1], result: [1], consumed: 1)
		assertParseFails(parser, [2,2])
		assertParseFails(parser, [])
    }
    
	func testCountParser0TimesWithoutConsumingInput () {
		let parser = count(0, token(1))

		assertParseSucceeds(parser, [1,1,1,1], result: [], consumed: 0)
		assertParseSucceeds(parser, [2,2,2,2], result: [], consumed: 0)
		assertParseSucceeds(parser, [], result: [], consumed: 0)
	}

	func testCountRangeOfLength3 () {
		let parser = count(2...4, token(1))

		assertParseFails(parser, [])
		assertParseFails(parser, [1])
		assertParseSucceeds(parser, [1,1], result: [1,1], consumed: 2)
		assertParseSucceeds(parser, [1,1,1,2], result: [1,1,1], consumed: 3)
		assertParseSucceeds(parser, [1,1,1,1,1,1], result: [1,1,1,1], consumed: 4)
	}

	func testCountRangeOfLength2 () {
		let parser = count(2...3, token(1))

		assertParseFails(parser, [])
		assertParseFails(parser, [1,2])
		assertParseSucceeds(parser, [1,1], result: [1,1], consumed: 2)
		assertParseSucceeds(parser, [1,1,1,2], result: [1,1,1], consumed: 3)
		assertParseSucceeds(parser, [1,1,1,1,1,1], result: [1,1,1], consumed: 3)
	}

	func testCountRangeOfLength1 () {
		let parser = count(2...2, token(1))

		assertParseFails(parser, [])
		assertParseFails(parser, [1,2])
		assertParseSucceeds(parser, [1,1], result: [1,1], consumed: 2)
		assertParseSucceeds(parser, [1,1,1], result: [1,1], consumed: 2)
	}

	func testCountRangeFrom0 () {
		let parser = count(0...2, token(1))

		assertParseSucceeds(parser, [])
		assertParseSucceeds(parser, [2,2])
		assertParseSucceeds(parser, [1,2])
		assertParseSucceeds(parser, [1,1], result: [1,1], consumed: 2)
		assertParseSucceeds(parser, [1,1,1,2], result: [1,1], consumed: 2)
	}

	func testOneOfParser () {
		let parser = oneOf("abc")

		assertParseSucceeds(parser, "a", result: "a")
		assertParseSucceeds(parser, "b", result: "b")
		assertParseSucceeds(parser, "c", result: "c")
		assertParseFails(parser, "d")
		assertParseSucceeds(parser, "ax", result: "a", consumed: 1)
	}

	func testNoneOfParser () {
		let parser = noneOf("abc")

		assertParseFails(parser, "a")
		assertParseFails(parser, "b")
		assertParseFails(parser, "c")
		assertParseSucceeds(parser, "d", result: "d")
		assertParseSucceeds(parser, "da", result: "d", consumed: 1)
	}

	func testNotParser () {
		let parser = not("a" as Character)

		assertParseSucceeds(parser, "b", result: "b")
		assertParseSucceeds(parser, "c", result: "c")
		assertParseFails(parser, "a")
	}

	func testEofParser () {
		let parser = token(1) <* eof()

		assertParseSucceeds(parser, [1], result: 1)
		assertParseFails(parser, [1,2])
		assertParseSucceeds(token(1), [1,2])
	}

	func testParsingAString () {
		let parser = zeroOrMore(char("a"))

		XCTAssertEqual(try! parse(parser, "a"), "a")
		XCTAssertEqual(try! parse(parser, "aaaa"), "aaaa")
		XCTempAssertThrowsError { _ = try parse(parser, "aaab") }
	}

	func testParsingAnArray () {
		let parser = zeroOrMore(token(1))

		XCTAssertEqual(try! parse(parser, []), [])
		XCTAssertEqual(try! parse(parser, [1]), [1])
		XCTAssertEqual(try! parse(parser, [1,1,1]), [1,1,1])
		XCTempAssertThrowsError { _ = try parse(parser, [1,2]) }
	}
}

extension Parser_Tests {
	public static var allTests = [
		("testSingleTokenParser", testSingleTokenParser),
		("testSeveralTokenParsers", testSeveralTokenParsers),
		("testFailingParserReturnsError", testFailingParserReturnsError),
		("testTokensParser", testTokensParser),
		("testStringTokensParser", testStringTokensParser),
		("testEmptyStringTokensParser", testEmptyStringTokensParser),
		("testAnyParser", testAnyParser),
		("testOptionalParser", testOptionalParser),
		("testOneOrMoreParser", testOneOrMoreParser),
		("testZeroOrMoreParser", testZeroOrMoreParser),
		("testCountParser", testCountParser),
		("testCount1Parser", testCount1Parser),
		("testCountParser0TimesWithoutConsumingInput", testCountParser0TimesWithoutConsumingInput),
		("testCountRangeOfLength3", testCountRangeOfLength3),
		("testCountRangeOfLength2", testCountRangeOfLength2),
		("testCountRangeOfLength1", testCountRangeOfLength1),
		("testCountRangeFrom0", testCountRangeFrom0),
		("testOneOfParser", testOneOfParser),
		("testNoneOfParser", testNoneOfParser),
		("testNotParser", testNotParser),
		("testEofParser", testEofParser),
		("testParsingAString", testParsingAString),
		("testParsingAnArray", testParsingAnArray),
		]
}
