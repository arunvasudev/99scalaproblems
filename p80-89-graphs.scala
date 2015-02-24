package graphs

abstract class GraphBase[T, U]{
    
    case class Edge(n1: Node, n2: Node, value: U){
        def toTuple = (n1.value, n2.value, value)
    }

    case class Node(value: T){
        var adj: List[Edge] = Nil
        def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
    }

    var nodes: Map[T, Node] = Map()
    var edges: Set[Edge] = Set() 

    // if the edge E connects N to another node, returns the other node
    // otherwise returns None
    def edgeTarget(e: Edge, n: Node): Option[Node]

    override def equals(o: Any) = o match {
        case g: GraphBase[T, U] => (nodes.keySet == g.nodes.keySet &&
                                    edges == g.edges)
        case _ => false
    }
}

class Graph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
        case g: Graph[T, U] => super.equals(g)
        case _ => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] = 
        if (e.n1 == n) Some(e.n2)
        else if (e.n2 == n) Some(e.n1)
        else None

    def addEdge(n1: T, n2: T, value: U) = {
        val e = new Edge(nodes(n1), nodes(n2), value)
        edges = edges + e
        nodes(n1).adj = e :: nodes(n1).adj
        nodes(n2).adj = e :: nodes(n2).adj
    }
}

class Digraph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
        case g: Digraph[T, U] => super.equals(g)
        case _ => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] = 
        if (e.n1 == n) Some(e.n2)
        else None

    def addArc(source: T, dest: T, value: U) = {
        val e = new Edge(nodes(source), nodes(dest), value)
        edges = edges + e
        nodes(source).adj = e :: nodes(source).adj
    }
}
