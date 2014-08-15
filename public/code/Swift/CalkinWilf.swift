struct Tree<A> {
    let val: A
    let left: () -> Tree<A>
    let right: () -> Tree<A>
    init(
        _ vali: A,
        _ lefti: @autoclosure () -> Tree<A>,
        _ righti: @autoclosure () -> Tree<A>
        ) { (val, left, right) = (vali, lefti, righti) }
}

struct Stream<A> {
    let val: A
    let next: () -> Stream<A>
    init ( _ vali: A, _ nexti: @autoclosure () -> Stream<A> ) { (val, next) = (vali, nexti) }
}

func calkin(n: Int, m: Int) -> Tree<(Int, Int)> {
    return Tree( (n, m), calkin(n+m, m), calkin(n, n+m) )
}

func interleave<A>(sa: Stream<A>, sb: Stream<A>) -> Stream<A> {
    return Stream( sa.val, Stream( sb.val, interleave(sa.next(), sb.next()) ) )
}

func traverse<A>(t: Tree<A>) -> Stream<A> {
    return Stream( t.val, interleave(traverse(t.left()), traverse(t.right())) )
}

let rats: Stream<(Int, Int)> = traverse(calkin(1,1))

func projectStream<A>(s: Stream<A>) -> (A, Stream<A>) {
    return (s.val, s.next())
}

func splitStream<A>(n: Int)(var s: Stream<A>) -> ([A], Stream<A>) {
    var res: [A] = []
    for i in 0..<n {
        let (a, s_) = projectStream(s)
        
        // impure bits
        res.append(a)
        s = s_
    }
    return (res, s)
}

func takeStream<A>(n: Int)(s: Stream<A>) -> [A] {
    let (front, _) = splitStream(n)(s: s)
    return front
}

func chunkStream<A>(n: Int)(s: Stream<A>) -> Stream<[A]> {
    let (front, back) = splitStream(n)(s: s)
    return Stream(front, chunkStream(n)(s: back))
}

println(takeStream(10)(s: chunkStream(3)(s: rats)))
