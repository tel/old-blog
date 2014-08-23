protocol Viewl {
    typealias El
    func uncons() -> (El, Self)?
}

func head<X:Viewl>(x: X) -> X.El? { return x.uncons()?.0 }
func tail<X:Viewl>(x: X) -> X?    { return x.uncons()?.1 }

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

extension Array:Viewl {
    typealias El = Element
    func uncons() -> (Array.Element, Array)? {
        return isEmpty ? nil : (self[0], Array(dropFirst(self)))
    }
}

extension String:Viewl {
    typealias El = Character
    func uncons() -> (Character, String)? {
        return isEmpty ? nil : (self[startIndex], dropFirst(self))
    }
}

func leftArray<S:Viewl>(n: Int)(var s: S) -> ([S.El], S) {
    var res:[S.El] = []
    for i in 0..<n {
        if let (a, ss) = s.uncons() {
            res.append(a)
            s = ss
        } else { break }
    }
    return (res, s)
}

func scott<S:Viewl, R>(zero: R, combine: (S.El, S) -> R)(subject: S) -> R {
    if let (x, xs) = subject.uncons() {
        return combine(x, xs)
    } else {
        return zero
    }
}

func fold<S:Viewl, R>(zero: R, combine: (S.El, R) -> R)(subject: S) -> R {
    if let (x, xs) = subject.uncons() {
        return combine(x, fold(zero, combine)(subject: xs))
    } else {
        return zero
    }
}

struct Stream<A>:Viewl {
    let project: () -> (A, Stream<A>)?
    
    typealias El = A
    func uncons() -> (A, Stream)? {
        return project()
    }
}

func stream<A>(projecti: (A, Stream<A>)?) -> Stream<A> {
    return Stream(project: projecti)
}

func leftStream<S:Viewl>(s: S) -> Stream<S.El> {
    return fold( stream( nil ), stream )(subject: s)
}
