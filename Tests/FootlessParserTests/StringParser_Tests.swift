//
// StringParser_Tests.swift
// FootlessParser
//
// Released under the MIT License (MIT), http://opensource.org/licenses/MIT
//
// Copyright (c) 2015 NotTooBad Software. All rights reserved.
//

import FootlessParser
import XCTest


enum Token: String {
    case foo, bar, baz

    static let allValues: [Token] = [ .foo, .bar, .baz ]
}

extension Token: CustomStringConvertible, CustomDebugStringConvertible {
    var description: String {
        return rawValue
    }

    var debugDescription: String {
        return self.description
    }
}

class StringParser_Tests: XCTestCase {

	func testCharParser () {
		let parser = char("a")

		var input = Array("a")

		assertParseFails(parser, "b")
		assertParseSucceeds(parser, &input, result: "a")
		XCTAssert(input == [], "Input should be empty")
	}

	func testOffsetForNoneOf () {
		let input = "AB"
		let parser = zeroOrMore(noneOf(["BC"]))
		// fatal error: cannot increment beyond endIndex
                let _ = try! parse(parser, input)
		assertParseSucceeds(parser, input, result: "AB")
	}

	func testOneOrMoreParserForCharacters () {
		let parser = oneOrMore(char("a"))

		assertParseSucceeds(parser, "a", result: "a")
		assertParseSucceeds(parser, "aaa", result: "aaa")
		assertParseSucceeds(parser, "aaab", result: "aaa", consumed: 3)
	}

	func testZeroOrMoreParserForCharacters () {
		let parser = zeroOrMore(char("a"))

		assertParseSucceeds(parser, "", result: "")
		assertParseSucceeds(parser, "b", result: "")
		assertParseSucceeds(parser, "a", result: "a")
		assertParseSucceeds(parser, "aaa", result: "aaa")
		assertParseSucceeds(parser, "aaab", result: "aaa", consumed: 3)
	}

	func testStringCountParser () {
		let parser = count(3, char("a"))

		assertParseSucceeds(parser, "aaaa", result: "aaa", consumed: 3)
		assertParseFails(parser, "aa")
		assertParseFails(parser, "axa")
		assertParseFails(parser, "")
	}

	func testStringCountRangeParser () {
		let parser = count(2...4, char("a"))

		assertParseFails(parser, "")
		assertParseFails(parser, "ab")
		assertParseSucceeds(parser, "aab", result: "aa", consumed: 2)
		assertParseSucceeds(parser, "aaaaa", result: "aaaa", consumed: 4)
	}

    func testNoneOfStrings() {
        let parser = zeroOrMore(noneOf(["foo", "bar"]))

        assertParseSucceeds(parser, "", result: "")
        assertParseSucceeds(parser, "a foo", result: "a ")
        assertParseSucceeds(parser, "a bar", result: "a ")
        assertParseSucceeds(parser, "bar foo", result: "")
    }

    func testString() {
        let parser = string("foo")
        assertParseSucceeds(parser, "foo", result: "foo")
        assertParseSucceeds(parser, "foobar", result: "foo")
        assertParseFails(parser, "barf")
        assertParseFails(parser, "bar")
        assertParseFails(parser, "ba")
        assertParseFails(parser, "b")
        assertParseFails(parser, "")
    }


    func testRepresentable() {
        let parser: Parser<Character,Token> = anyOf( Token.allValues )

        assertParseSucceeds( parser, "foo", result: Token.foo )
        assertParseSucceeds( parser, "bar", result: Token.bar )
        assertParseSucceeds( parser, "baz", result: Token.baz )
        assertParseSucceeds( parser, "fooo", result: Token.foo, consumed: 3 )
        assertParseFails( parser, "boink" )
        assertParseFails( parser, "fo" )
        assertParseFails( parser, "ba" )
    }

    func testLexemes() {

        let parser = lexemes( anyOf( Token.allValues ) )

        var result = try! parse( parser, "\t foo bar baz  ")
        XCTAssertEqual(result, [Token.foo, Token.bar, Token.baz])

        result = try! parse( parser, "foo" )
        XCTAssertEqual(result, [Token.foo])

        result = try! parse( parser, "\n\n\r\n\r   foo    \r\n\r\n\t\r\n" )
        XCTAssertEqual(result, [Token.foo])


        result = try! parse( parser, "" )
        XCTAssertEqual(result, [] as [Token])

        assertParseFails( parser, "foobarbaz" )

        let parser2 = parser <* oneOrMore( digit )

        result = try! parse( parser2, "\t foo bar baz 123")
        XCTAssertEqual(result, [Token.foo, Token.bar, Token.baz])

        assertParseFails( parser2, "foo bar baz123" )

    }
}
extension StringParser_Tests {
	public static var allTests = [
		("testCharParser", testCharParser),
		("testOneOrMoreParserForCharacters", testOneOrMoreParserForCharacters),
		("testZeroOrMoreParserForCharacters", testZeroOrMoreParserForCharacters),
		("testStringCountParser", testStringCountParser),
		("testStringCountRangeParser", testStringCountRangeParser),
		("testNoneOfStrings", testNoneOfStrings),
		("testString", testString),
		("testOffsetForNoneOf", testOffsetForNoneOf)
		]
}
