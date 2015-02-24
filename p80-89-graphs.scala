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

    def addNode(n: T): Node = {
        if (nodes.contains(n))
            return nodes(n)

        val newNode = Node(n)
        nodes = nodes + ((n, newNode))
        return newNode
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
        val node1 = addNode(n1)
        val node2 = addNode(n2)
        val e = new Edge(node1, node2, value)
        edges = edges + e
        nodes(n1).adj = e :: node1.adj
        nodes(n2).adj = e :: node2.adj
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
        val sourceNode = addNode(source)
        val destNode = addNode(dest)
        val e = new Edge(sourceNode, destNode, value)
        edges = edges + e
        nodes(source).adj = e :: sourceNode.adj
    }
}

object Graph {
    def term[T, U](nodes: List[T], edges: List[(T, T)], defLabel: U): Graph[T, U] = {
        val graph = new Graph[T, U]
        nodes.foreach(n => graph.addNode(n))
        edges.foreach({case (n1, n2) => graph.addEdge(n1, n2, defLabel)})
        graph
    }

    def adjacentList[T, U](list: List[(T, List[T])], defLabel: U): Graph[T, U] = {
        val graph = new Graph[T, U]
        list.foreach({ case (n, otherEnds) => 
            graph.addNode(n)
            otherEnds.foreach(n1 => graph.addEdge(n, n1, defLabel))
        })

        graph
    }
}

object Digraph {
    def term[T, U](nodes: List[T], edges: List[(T, T)], defLabel: U): Digraph[T, U] = {
        val graph = new Digraph[T, U]
        nodes.foreach(n => graph.addNode(n))
        edges.foreach({case (n1, n2) => graph.addArc(n1, n2, defLabel)})
        graph
    }

    def adjacentList[T, U](list: List[(T, List[T])], defLabel: U): Digraph[T, U] = {
        val graph = new Digraph[T, U]
        list.foreach({ case (n, otherEnds) => 
            graph.addNode(n)
            otherEnds.foreach(n1 => graph.addArc(n, n1, defLabel))
        })

        graph
    }
}
