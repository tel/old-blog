func id  <A>(a: A)      -> A { return a }
func fix <A>(f: A -> A) -> A { return f(fix(f)) }
func loop<A>()          -> A { return fix(id) }

protocol Viewl {
    typealias El
    func uncons() -> (El, Self)?
}

func head<X:Viewl>(x: X) -> X.El? { return x.uncons()?.0 }
func tail<X:Viewl>(x: X) -> X?    { return x.uncons()?.1 }

extension String:Viewl {
    typealias El = Character
    func uncons() -> (Character, String)? {
        return isEmpty ? nil : (self[startIndex], dropFirst(self))
    }
}

extension Array:Viewl {
    typealias El = Element
    func uncons() -> (Array.Element, Array)? {
        return isEmpty ? nil : (self[0], Array(dropFirst(self)))
    }
}

class Ptr<T> { let val: T; init(_ aVal:T) { val = aVal } }

// Lazy, potentially-infinite Streams in Swift
struct Stream<A> {
    typealias El      = A
    let project: () -> (A, Ptr<Stream<A>>)?
    
    func append(s: Stream<A>) -> Stream<A> {
        return mkStream {
            if let (a, ss) = self.uncons() {
                return (a, Ptr(ss.append(s)))
            } else { return s.project() }
        }
    }
    
    func then<B>(f: A -> Stream<B>) -> Stream<B> {
        if let (a, s) = uncons() {
            return f(a).append(s.then(f))
        } else { return Stream.empty() }
    }
}

extension Stream {
    static func empty<A>() -> Stream<A> { return mkStream { nil } }
}

extension Stream {
    static func pure(a: A) -> Stream<A> {
        return Stream { return (a, Ptr(Stream.empty())) }
    }
}

extension Stream: Viewl {
    func uncons() -> (El, Stream)? {
        if let (x, a) = project() {
            return (x, a.val)
        } else { return nil }
    }
}

func mkStream<A>(p: () -> (A, Ptr<Stream<A>>)?) -> Stream<A> {
    return Stream(project: p)
}

// func arrayStream<S:Viewl>(s: S) -> Stream<S.El> {
//     if let (x, xs) = s.uncons() {
//         return mkStream { (x, Ptr(arrayStream(xs))) }
//     } else { return Stream.empty() }
// }

func streamN(n: Int) -> Stream<Int> {
    return mkStream {
        return (n, Ptr(streamN(n+1)))
    }
}

func maybe<A,R>(z: R, f: A -> R)(m: A?) -> R {
    if let a = m { return f(a) } else { return z }
}

func viewN<S:Viewl>(n: Int)(s: S) -> [S.El] {
    var res:[S.El] = []
    var s_  = s
    for i in 1..<n {
        if let (a, ss) = s_.uncons() {
            res += a
            s_  = ss
        } else { break }
    }
    return res
}

// Convert a String into a Stream of characters
// func streamString(s: String) -> Stream<Character> {
//     // return Stream(project: { opt(second(streamString))(maybeA: s) })
//     return Stream(project: {
//         if let (c, cs) = s.uncons() {
//             return (c, streamString(cs))
//         } else {
//             return nil
//         }
//     })
// }



//struct Parser<A> {
//    let parse: String -> [(A, String)]
//    
//    static func pure(a: A) -> Parser<A> {
//        return Parser(parse: { s in [(a,s)] })
//    }
//    
//    func then<B>(f: A -> Parser<B>) -> Parser<B> {
//        return Parser(
//            parse: { s in
//                parse(s).then { res in
//                    
//                }
//            }
//        )
//    }
//}

//func satisfy(p: Character -> Bool) -> Parser<Character> {
//    return Parser({ s1 in
//        if let (c, s2) = s1.uncons() {
//            if p(c) {
//                return [(c, s2)]
//            } else {
//                return []
//            }
//        } else {
//            return []
//        }
//    })
//}
//
//satisfy({c in c == "h"}).parse("hoo")
//satisfy({c in c == "h"}).parse("doo")
