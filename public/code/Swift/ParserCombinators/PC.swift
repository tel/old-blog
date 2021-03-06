func id  <A>(a: A)      -> A { return a }
func fix <A>(f: A -> A) -> A { return f(fix(f)) }
func loop<A>()          -> A { return loop() }

protocol Viewl {
    typealias El
    func uncons() -> (El, Self)?
}

func head<X:Viewl>(x: X) -> X.El? { return x.uncons()?.0 }
func tail<X:Viewl>(x: X) -> X?    { return x.uncons()?.1 }

func scott<S:Viewl, R>(z: R, f: (S.El, S) -> R)(subject: S) -> R {
    if let (x, xs) = subject.uncons() {
        return f(x, xs)
    } else { return z }
}

func fold<S:Viewl, R>(z: R, f: (S.El, R) -> R)(subject: S) -> R {
    return scott(z, { (x, xs) in f(x, fold(z, f)(subject: xs)) })(subject: subject)
}

func leftArray<S:Viewl>(n: Int)(s: S) -> ([S.El], S) {
    var res:[S.El] = []
    var s_  = s
    for i in 0..<n {
        if let (a, ss) = s_.uncons() {
            res += a
            s_  = ss
        } else { break }
    }
    return (res, s_)
}

func leftStream<S:Viewl>(s: S) -> Stream<S.El> {
    return fold(
      Stream.empty(),
      { (x,xs) in return Stream { return (x, xs) } }
    )(subject: s)
}

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

extension Int:Viewl {
    typealias El = Int
    func uncons() -> (Int, Int)? {
        return (self, self+1)
    }
}

class Ptr<T> { let val: T; init(_ aVal:T) { val = aVal } }

enum List<A>:Viewl {
    case Empty
    case Cons(A, Ptr<List<A>>)

    typealias El = A
    func uncons() -> (A, List<A>)? {
        switch self {
        case Empty: return nil
        case Cons(let x, let xs): return (x, xs.val)
        }
    }
}

// Lazy, potentially-infinite Streams in Swift
struct Stream<A>:Viewl {

    typealias El = A
    
    // Internal use only---use the initializer to write a more natural
    // unfold which ignores the use of Ptr
    let project: () -> (A, Ptr<Stream<A>>)?

    init(p: () -> (A, Stream<A>)?) {
        project = {
            if let (a, s) = p() {
                return (a, Ptr(s))
            } else { return nil }
        }
    }

    
    // Generated by an unfold
    func uncons() -> (El, Stream)? {
        if let (x, a) = project() {
            return (x, a.val)
        } else { return nil }
    }


    // Standard constructions
    static func empty() -> Stream<A> { return Stream { nil } }

    static func pure(a: A) -> Stream<A> {
        return Stream { return (a, Stream.empty()) }
    }

}

extension Stream {
    // Combining streams one after another. 
    func append(s: Stream) -> Stream {
        return Stream {
            scott(s.uncons(), { (a, ss) in (a, ss.append(s)) })(subject: self)
        }
    }

    // Extending streams element-by-element. (This is Scala's flatMap
    // and Haskell's infamous (>>=))
    func then<B>(f: El -> Stream<B>) -> Stream<B> {
        return scott(
            Stream<B>.empty(),
            { (a, s) in f(a).append(s.then(f)) }
        )(subject: self)
    }

    // Truncate a Stream to a certain length
    func take(n: Int) -> Stream {
        if n == 0 { return Stream.empty() }
        else {
            return scott(
                Stream.empty(),
                { (x, xs) in Stream { return (x, xs.take(n-1)) } }
            )(subject: self)
        }
    }
}

func upFrom(n: Int) -> Stream<Int> {
    return Stream {
        return (n, upFrom(n+1))
    }
}

func maybe<A,R>(z: R, f: A -> R)(m: A?) -> R {
    if let a = m { return f(a) } else { return z }
}

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
