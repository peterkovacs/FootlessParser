//
// TestHelpers.swift
// FootlessParser
//
// Created by Kåre Morstøl on 09.04.15.
// Copyright (c) 2015 NotTooBad Software. All rights reserved.
//

import Foundation
import FootlessParser
import XCTest

public func == <R:Equatable, I:Equatable, E:Equatable>
    (lhs: ((output: R, remainder: AnyCollection<I>)?, E?), rhs: ((output: R, remainder: AnyCollection<I>)?, E?)) -> Bool {
    if let lhs=lhs.0, let rhs=rhs.0 {
        return lhs.output == rhs.output && lhs.remainder == rhs.remainder
    }
    if let lhs=lhs.1, let rhs=rhs.1 {
        return lhs == rhs
    }
    return false
}

public func != <R:Equatable, I:Equatable, E:Equatable>
	(lhs: ((output: R, remainder: AnyCollection<I>)?, E?), rhs: ((output: R, remainder: AnyCollection<I>)?, E?)) -> Bool {

	return !(lhs == rhs)
}

extension XCTestCase {

	/**
	Verifies that 2 parsers return the same given the same input, whether it be success or failure.

	- parameter shouldSucceed?: Optionally verifies success or failure.
	*/
    func assertParsesEqually <T: Equatable, R: Equatable>
        (_ p1: Parser<T,R>, _ p2: Parser<T,R>, input: [T], shouldSucceed: Bool? = nil, file: StaticString = #file, line: UInt = #line) {

        let parse = { (p: Parser<T, R>) -> (R?, Remainder<T>, String?) in
            var remainder = Remainder(input)
            do {
                return (try p.parse(&remainder), remainder, nil)
            } catch let error as ParseError<T> {
                return (nil, remainder, error.description)
            } catch {
                return (nil, remainder, nil)  // should not happen
            }
        }
        let r1 = parse(p1)
        let r2 = parse(p2)

        if r1.0 != r2.0 || r1.1 != r2.1 || r1.2 != r2.2 {
        	return XCTFail("with input '\(input)': '\(r1)' != '\(r2)", file: file, line: line)
        }
		if let shouldSucceed = shouldSucceed {
			if shouldSucceed && (r1.0 == nil) {
				XCTFail("parsing of '\(input)' failed, shoud have succeeded", file: file, line: line)
			}
			if !shouldSucceed && (r1.0 != nil) {
				XCTFail("parsing of '\(input)' succeeded, shoud have failed", file: file, line: line)
			}
		}
	}

	/** Verifies the parse succeeds, and optionally checks the result and how many tokens were consumed. Updates the provided 'input' parameter to the remaining input. */
	func assertParseSucceeds <T, R: Equatable>
        (_ p: Parser<T,R>, _ input: inout [T], result: R? = nil, consumed: Int? = nil, file: StaticString = #file, line: UInt = #line) {

        do {
            var remainder = Remainder(input)
            let output = try p.parse(&remainder)
            if let result = result {
                if output != result {
                    XCTFail("with input '\(input)': output should be '\(result)', was '\(output)'. ", file: file, line: line)
                }
            }
            if let consumed = consumed {
                let actuallyconsumed = input.count - Int(remainder.count)
                if actuallyconsumed != consumed {
                    XCTFail("should have consumed \(consumed), took \(actuallyconsumed)", file: file, line: line)
                }
            }
            input = Array(remainder)
        } catch let error {
            XCTFail("with input \(input): \(error)", file: file, line: line)
        }
	}

	/** Verifies the parse succeeds, and optionally checks the result and how many tokens were consumed. Updates the provided 'input' parameter to the remaining input. */
	func assertParseSucceeds <T, R: Equatable>
		(_ p: Parser<T,[R]>, _ input: inout [T], result: [R]? = nil, consumed: Int? = nil, file: StaticString = #file, line: UInt = #line) {

        do {
            var remainder = Remainder(input)
            let output = try p.parse(&remainder)
            if let result = result {
                if output != result {
                    XCTFail("with input '\(input)': output should be '\(result)', was '\(output)'. ", file: file, line: line)
                }
            }
            if let consumed = consumed {
                let actuallyconsumed = input.count - Int(remainder.count)
                if actuallyconsumed != consumed {
                    XCTFail("should have consumed \(consumed), took \(actuallyconsumed)", file: file, line: line)
                }
            }
            input = Array(remainder)
        } catch let error {
            XCTFail("with input \(input): \(error)", file: file, line: line)
        }
	}

	/** Verifies the parse succeeds, and optionally checks the result and how many tokens were consumed. */
    func assertParseSucceeds <T, R: Equatable, C: Collection>
		(_ p: Parser<T,R>, _ input: C, result: R? = nil, consumed: Int? = nil, file: StaticString = #file, line: UInt = #line) where C.Iterator.Element == T {

		var parserinput = Array(input)
		assertParseSucceeds(p, &parserinput, result: result, consumed: consumed, file: file, line: line)
	}

	/** Verifies parsing the string succeeds, and optionally checks the result and how many tokens were consumed. */
	func assertParseSucceeds <R: Equatable>
		(_ p: Parser<Character,R>, _ input: String, result: R? = nil, consumed: Int? = nil, file: StaticString = #file, line: UInt = #line) {

        var parserinput = Array(input.characters)
        assertParseSucceeds(p, &parserinput, result: result, consumed: consumed, file: file, line: line)
	}

	/** Verifies the parse succeeds, and optionally checks the result and how many tokens were consumed. */
	func assertParseSucceeds <T, R: Equatable, C: Collection>
		(_ p: Parser<T,[R]>, _ input: C, result: [R]? = nil, consumed: Int? = nil, file: StaticString = #file, line: UInt = #line) where C.Iterator.Element == T {

        var input = Array(input)
		assertParseSucceeds(p, &input, result: result, consumed: consumed, file: file, line: line)
	}


	/** Verifies the parse fails with the given input. */
	func assertParseFails <T, R>
		(_ p: Parser<T,R>, _ input: T, file: StaticString = #file, line: UInt = #line) {

        do {
            var remainder = Remainder([input])
            let output = try p.parse(&remainder)
            XCTFail("Parsing succeeded with output '\(output)', should have failed.", file: file, line: line)
        } catch {}
	}

	/** Verifies the parse fails with the given collection as input. */
	func assertParseFails <T, R, C: Collection>
		(_ p: Parser<T,R>, _ input: C, file: StaticString = #file, line: UInt = #line) where C.Iterator.Element == T {

        do {
            var remainder = Remainder(input)
            let output = try p.parse(&remainder)
            XCTFail("Parsing succeeded with output '\(output)', should have failed. Remainder: \(remainder)", file: file, line: line)
        } catch {}
	}

	/** Verifies the parse fails with the given string as input. */
	func assertParseFails <R>
		(_ p: Parser<Character,R>, _ input: String, file: StaticString = #file, line: UInt = #line) {

        return assertParseFails(p, Array(input.characters), file: file, line: line)
	}
}


import Foundation

extension XCTestCase {

	// from https://forums.developer.apple.com/thread/5824
	func XCTempAssertThrowsError (message: String = "", file: StaticString = #file, line: UInt = #line, _ block: () throws -> ()) {
		do	{
			try block()

			let msg = (message == "") ? "Tested block did not throw error as expected." : message
			XCTFail(msg, file: file, line: line)
		} catch {}
	}

	func XCTempAssertNoThrowError(message: String = "", file: StaticString = #file, line: UInt = #line, _ block: () throws -> ()) {
		do { try block() }
		catch	{
			let msg = (message == "") ? "Tested block threw unexpected error: " : message
			XCTFail(msg + String(describing: error), file: file, line: line)
		}
	}
}
