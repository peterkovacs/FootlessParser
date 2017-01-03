import Foundation

public enum CountError: Error {
    case NegativeCount
}


public enum ParseError<T: Equatable>: Error {
    case Mismatch(Remainder<T>, String, String)
}

extension ParseError: CustomStringConvertible {
    public var description: String {
        switch self {
        case let .Mismatch(remainder, expected, actual): return "Expected \(expected), actual \(actual) at position -\(remainder.count)"
        }
    }
}
