//
// Parser.swift
// FootlessParser
//
// Released under the MIT License (MIT), http://opensource.org/licenses/MIT
//
// Copyright (c) 2015 NotTooBad Software. All rights reserved.
//

public struct Remainder<Token: Equatable> {
    public typealias Data = Array<Token>
    let original: Data
    var position: Data.Index
    public let endIndex: Data.Index

}

public struct Parser<Token: Equatable, Output> {
    public typealias ParseFunction = (inout Remainder<Token>) throws -> Output
    public let parse: ParseFunction

    public init( parse: @escaping ParseFunction ) {
      self.parse = parse
    }
}

public extension Parser {
    public func map<B>( _ f: @escaping (Output) -> B ) -> Parser<Token,B> {
        return Parser<Token,B> { input in
            let result = try self.parse(&input)
            return f(result)
        }
    }

    public func flatMap<B>( _ f: @escaping (Output) -> Parser<Token,B> ) -> Parser<Token,B> {
        return Parser<Token,B> { input in
            let result = try self.parse(&input)
            return try f(result).parse(&input)
        }
    }

    public func between<P1, P2>( _ p1: Parser<Token, P1>, _ p2: Parser< Token, P2 > ) -> Parser<Token,Output> {
        return ( p1 *> self <* p2 )
    }

    init<P1, P2>( lift f: @escaping (P1, P2) -> Output, _ p1: Parser<Token,P1>, _ p2: Parser<Token, P2> ) {
        self.parse = { input in
            let original = input
            do {
                let a = try p1.parse(&input)
                let b = try p2.parse(&input)

                return f(a, b)
            } catch {
                input = original
                throw error
            }
        }
    }

    init<P1, P2, P3>( lift f: @escaping (P1, P2, P3) -> Output, _ p1: Parser<Token,P1>, _ p2: Parser<Token, P2>, _ p3: Parser<Token, P3> ) {
        self.parse = { input in
            let original = input
            do {
                let a = try p1.parse(&input)
                let b = try p2.parse(&input)
                let c = try p3.parse(&input)

                return f(a, b, c)
            } catch {
                input = original
                throw error
            }
        }
    }

    init<P1, P2, P3, P4>( lift f: @escaping (P1, P2, P3, P4) -> Output, _ p1: Parser<Token,P1>, _ p2: Parser<Token, P2>, _ p3: Parser<Token, P3>, _ p4: Parser<Token, P4>) {
        self.parse = { input in
            let original = input
            do {
                let a = try p1.parse(&input)
                let b = try p2.parse(&input)
                let c = try p3.parse(&input)
                let d = try p4.parse(&input)

                return f(a, b, c, d)
            } catch {
                input = original
                throw error
            }
        }
    }

    init<P1, P2, P3, P4, P5>( lift f: @escaping (P1, P2, P3, P4, P5) -> Output, _ p1: Parser<Token,P1>, _ p2: Parser<Token, P2>, _ p3: Parser<Token, P3>, _ p4: Parser<Token, P4>, _ p5: Parser<Token, P5> ) {
        self.parse = { input in
            let original = input
            do {
                let a = try p1.parse(&input)
                let b = try p2.parse(&input)
                let c = try p3.parse(&input)
                let d = try p4.parse(&input)
                let e = try p5.parse(&input)
                
                return f(a, b, c, d, e)
            } catch {
                input = original
                throw error
            }
        }
    }
}

public extension Remainder {
    public init<T: Collection>( _ collection: T ) where T.Iterator.Element == Token {
        original = Array(collection)
        position = original.startIndex
        endIndex = original.endIndex
    }

     public mutating func scan() -> Token? {
        return scan { _ in true }
    }

    public mutating func peek() -> Token? {
        guard position < endIndex else { return nil }
        return original[position]
    }

    public mutating func scan( condition: (Token) -> Bool ) -> Token? {
        guard let result = peek(), condition(result) else { return nil }
        defer { position = index( after: position ) }
        return result
    }
}

extension Remainder: CustomStringConvertible {
    public var description: String {
        return String(describing: original[position..<endIndex])
    }
}

extension Remainder: Collection {
    public typealias Index = Array<Token>.Index

    public mutating func next() -> Token? {
        guard position < endIndex else { return nil }
        defer { position = original.index( after: position ) }
        return original[position]
    }

    public var startIndex: Index { return position }

    public subscript(index: Index) -> Token {
        return original[index]
    }
    public subscript(index: Range<Index>) -> Remainder {
      return Remainder( original[index] )
    }
    public subscript(index: ClosedRange<Index>) -> Remainder {
      return Remainder( original[index] )
    }

    public func index( after i: Index ) -> Index {
        return original.index(after: i)
    }

    public func index(_ i: Int, offsetBy n: Int, limitedBy limit: Int) -> Int? {
        return original.index(i, offsetBy: n, limitedBy: limit)
    }

    public mutating func advance(by: Int = 1) {
        position = original.index(position, offsetBy: by, limitedBy: endIndex) ?? endIndex
    }
}

extension Remainder: Equatable {
    public static func ==(lhs: Remainder<Token>, rhs: Remainder<Token> ) -> Bool {
        return lhs.count == rhs.count && lhs.original.elementsEqual(rhs.original)
    }
}

public func satisfy<T>
    (expect: @autoclosure @escaping () -> String, condition: @escaping (T) -> Bool) -> Parser<T, T> {
    return Parser { (input: inout Remainder) in
        if let next = input.first {
            if condition(next) {
                input.advance(by: 1)
                return next
            } else {
                throw ParseError.Mismatch(input, expect(), String(describing:next))
            }
        } else {
            throw ParseError.Mismatch(input, expect(), "EOF")
        }
    }
}

public func token<T: Equatable>(_ token: T) -> Parser<T, T> {
    return satisfy(expect: String(describing:token)) { $0 == token }
}

/** Match several tokens in a row. */
public func tokens <T: Equatable, C: Collection> (_ xs: C) -> Parser<T,C> where C.Iterator.Element == T {
    let length = xs.count
    return count(length, any()) >>- { parsedtokens in
        if parsedtokens.elementsEqual(xs) {
            return pure( xs )
        } else {
            return fail( .Mismatch(Remainder(parsedtokens), String(describing:xs), String(describing: parsedtokens)) )
        }
    }
}

/** Return whatever the next token is. */
public func any <T> () -> Parser<T,T> {
    return satisfy(expect: "anything") { _ in true }
}

/** Try parser, if it fails return 'otherwise' without consuming input. */
public func optional <T,A> (_ p: Parser<T,A>, otherwise: A) -> Parser<T,A> {
    return p <|> pure(otherwise)
}

/** Try parser, if it fails return nil without consuming input. */
public func optional <T,A> (_ p: Parser<T, A>) -> Parser<T, A?> {
    return Parser { input in
        do {
            return try p.parse(&input)
        } catch is ParseError<T> {
            return nil
        }
    }
}

/** Delay creation of parser until it is needed. */
public func lazy <T,A> (_ f: @autoclosure @escaping () -> Parser<T,A>) -> Parser<T,A> {
    return Parser { input in try f().parse(&input) }
}

/** Apply parser once, then repeat until it fails. Returns an array of the results. */
public func oneOrMore <T,A> (_ p: Parser<T,A>) -> Parser<T,[A]> {
    return Parser { input in
        let first = try p.parse(&input)
        var result = [first]
        while true {
            do {
                let next = try p.parse(&input)
                result.append(next)
            } catch {
                return result
            }
        }
    }
}

/** Repeat parser until it fails. Returns an array of the results. */
public func zeroOrMore <T,A> (_ p: Parser<T,A>) -> Parser<T,[A]> {
    return optional( oneOrMore(p), otherwise: [] )
}

/** Repeat parser 'n' times. If 'n' == 0 it always succeeds and returns []. */
public func count <T,A> (_ n: Int, _ p: Parser<T,A>) -> Parser<T,[A]> {
    return Parser { input in
        var working = input
        var results = [A]()
        for _ in 0..<n {
            results.append(try p.parse(&working))
        }
        input = working
        return results
    }
}

/**
 Repeat parser as many times as possible within the given range.
 count(2...2, p) is identical to count(2, p)
 - parameter r: A positive closed integer range.
 */
public func count <T,A> (_ r: ClosedRange<Int>, _ p: Parser<T,A>) -> Parser<T,[A]> {
    precondition(r.lowerBound >= 0, "Count must be >= 0")
    return extend <^> count(r.lowerBound, p) <*> ( count(r.count-1, p) <|> zeroOrMore(p) )
}

/**
 Repeat parser as many times as possible within the given range.
 count(2..<3, p) is identical to count(2, p)
 - parameter r: A positive half open integer range.
 */
public func count <T,A> (_ r: Range<Int>, _ p: Parser<T,A>) -> Parser<T,[A]> {
    precondition(r.lowerBound >= 0, "Count must be >= 0")
    return extend <^> count(r.lowerBound, p) <*> ( count(r.count-1, p) <|> zeroOrMore(p) )
}

/** Succeed if the next token is in the provided collection. */
public func oneOf <T: Equatable, C: Collection> (_ collection: C) -> Parser<T,T> where C.Iterator.Element == T {
    return satisfy(expect: "one of '\(String(describing:collection))'") { collection.contains($0) }
}

/** Succeed if the next token is _not_ in the provided collection. */
public func noneOf <T: Equatable, C: Collection> (_ collection: C) -> Parser<T,T> where C.Iterator.Element == T {
    return satisfy(expect: "something not in '\(collection)'") { !collection.contains($0) }
}

/** Match anything but this. */
public func not <T: Equatable> (_ token: T) -> Parser<T,T> {
    return satisfy(expect: "anything but '\(token)'") { $0 != token }
}

/** Verify that input is empty. */
public func eof <T> () -> Parser<T,()> {
    return Parser { input in
        if !input.isEmpty {
            throw ParseError.Mismatch(input, "EOF", String(describing:input))
        }
        return ()
    }
}

/** Fail with the given error message. Ignores input. */
public func fail <T,A> (_ error: ParseError<T>) -> Parser<T,A> {
    return Parser { _ in throw error }
}

/**
 Parse all of input with parser.
 Failure to consume all of input will result in a ParserError.
 - parameter p: A parser.
 - parameter input: A collection, like a string or an array.
 - returns: Output from the parser.
 - throws: ParserError.
 */
public func parse<A,T,C: Collection>(_ p: Parser<T,A>, _ c: C) throws -> A where T == C.Iterator.Element {
    var input = Remainder( c )
    return try ( p <* eof() ).parse(&input)
}

/**
 Parses a single String RawRepresentable into its represented value.

 ```
 enum Foo: String {
 case foo
 }
 let p: Parser<Character,Foo> =
 representable( string( "foo" ) )
 ```

 - Parameter p: A string parser, that can parse a raw value represented by the type `Repr`.
 - Returns: A string parser that returns the type specified by `Repr`.

 [Sourcery](https://github.com/krzysztofzablocki/Sourcery) may be helpful to generate an array of values in the enum.
 */
public func representable<Repr: RawRepresentable, T, A>( _ p: Parser<T,A> ) -> Parser<T,Repr> where A == Repr.RawValue {
    return Parser { input in
        let repr = try p.parse( &input )
        if let value = Repr(rawValue: repr) {
            return value
        } else {
            throw ParseError.Mismatch(input, String(describing:Repr.self), String(describing:input))
        }
    }
}

public func anyOf<Token, Output, S: Collection>( _ s: S ) -> Parser<Token, Output> where S.Iterator.Element == Parser<Token,Output> {
    return s.reduce( fail(.Mismatch(Remainder([]), "", "")), <|> )
}

/** Creates a chain of alternate parsers based on the elements of collection `s`.  I think this is essentially the same as `oneOf`.
 */
public func anyOf<T, S: Collection>( _ s: S ) -> Parser<T,T> where S.Iterator.Element == T {
    if let first = s.first {
        var p: Parser<T, T> = token( first )

        for next in s.dropFirst() {
            p = p <|> token( next )
        }

        return p
    } else {
        return Parser {
            throw ParseError.Mismatch($0, "nothing expected", String( describing: $0.first ))
        }
    }
}
