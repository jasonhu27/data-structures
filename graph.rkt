#lang dssl2

let eight_principles = ["Know your rights.",
    "Acknowledge your sources.",
    "Protect your work.",
    "Avoid suspicion.",
    "Do your own work.",
    "Never falsify a record or permit another person to do so.",
    "Never fabricate data, citations, or experimental results.",
    "Always tell the truth when discussing your work with your instructor."]

# HW4: Graph

import cons
import sbox_hash
import 'hw4-lib/dictionaries.rkt'


###
### REPRESENTATION
###

# A Vertex is a natural number.
let Vertex? = nat?

# A VertexList is either
#  - None, or
#  - cons(v, vs), where v is a Vertex and vs is a VertexList
let VertexList? = Cons.ListC[Vertex?]

# A Weight is a real number. (It’s a number, but it’s neither infinite
# nor not-a-number.)
let Weight? = AndC(num?, NotC(OrC(inf, -inf, nan)))

# An OptWeight is either
# - a Weight, or
# - None
let OptWeight? = OrC(Weight?, NoneC)

# A WEdge is WEdge(Vertex, Vertex, Weight)
struct WEdge:
    let u: Vertex?
    let v: Vertex?
    let w: Weight?

# A WEdgeList is either
#  - None, or
#  - cons(w, ws), where w is a WEdge and ws is a WEdgeList
let WEdgeList? = Cons.ListC[WEdge?]

# A weighted, undirected graph ADT.
interface WUGRAPH:

    # Returns the number of vertices in the graph. (The vertices
    # are numbered 0, 1, ..., k - 1.)
    def len(self) -> nat?

    # Sets the weight of the edge between u and v to be w. Passing a
    # real number for w updates or adds the edge to have that weight,
    # whereas providing providing None for w removes the edge if
    # present. (In other words, this operation is idempotent.)
    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC

    # Gets the weight of the edge between u and v, or None if there
    # is no such edge.
    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?

    # Gets a list of all vertices adjacent to v. (The order of the
    # list is unspecified.)
    def get_adjacent(self, v: Vertex?) -> VertexList?

    # Gets a list of all edges in the graph, in an unspecified order.
    # This list only includes one direction for each edge. For
    # example, if there is an edge of weight 10 between vertices
    # 1 and 3, then exactly one of WEdge(1, 3, 10) or WEdge(3, 1, 10)
    # will be in the result list, but not both.
    def get_all_edges(self) -> WEdgeList?
    
class WuGraph (WUGRAPH):
    let length
    let adjMatrix

    def __init__(self, size: nat?):
        self.length = size
        self.adjMatrix = [[inf; size] for i in range(size)]
        
    def len(self):
        return self.length
        
    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?):
        if w == None:
            self.adjMatrix[u][v] = inf
            self.adjMatrix[v][u] = inf
        else:
            self.adjMatrix[u][v] = w
            self.adjMatrix[v][u] = w
    
    def get_edge(self, u: Vertex?, v: Vertex?):
        if self.adjMatrix[u][v] == inf:
            return None
        else:
            return self.adjMatrix[u][v]
        
    def get_adjacent(self, v: Vertex?):
        let head = None
        let tail = head
        for i in range(self.length):
            if self.adjMatrix[v][i] != inf:
                if head == None:
                    head = cons(i, None)
                    tail = head
                else:
                    let temp = cons(i, None)
                    tail.next = temp
                    tail = temp
        return head
           
    def get_all_edges(self):
        let head = None
        let tail = head
        let visited = [[False; self.length] for i in range(self.length)]
        for r in range(self.length):
            for c in range(self.length):
                if (self.adjMatrix[r][c] != inf) & (visited[c][r] == False):
                    let e = WEdge(r, c, self.adjMatrix[r][c])
                    visited[r][c] = True
                    visited[c][r] = True
                    if head == None:
                        head = cons(e, None)
                        tail = head
                    else:
                        let temp = cons(e, None)
                        tail.next = temp
                        tail = temp
        return head
        
    def print(self):
        return self.adjMatrix
                    
###
### List helpers
###

# To test methods that return lists with elements in an unspecified
# order, you can use these functions for sorting. Sorting these lists
# will put their elements in a predictable order, order which you can
# use when writing down the expected result part of your tests.

# sort_vertices : ListOf[Vertex] -> ListOf[Vertex]
# Sorts a list of numbers.
def sort_vertices(lst: Cons.list?) -> Cons.list?:
    def vertex_lt?(u, v): return u < v
    return Cons.sort[Vertex?](vertex_lt?, lst)

# sort_edges : ListOf[WEdge] -> ListOf[WEdge]
# Sorts a list of weighted edges, lexicographically
# ASSUMPTION: There's no need to compare weights because
# the same edge can’t appear with different weights.
def sort_edges(lst: Cons.list?) -> Cons.list?:
    def edge_lt?(e1, e2):
        return e1.u < e2.u or (e1.u == e2.u and e1.v < e2.v)
    return Cons.sort[WEdge?](edge_lt?, lst)
    
test 'WU Graph':
    let w = WuGraph(3)
    let b = w.get_adjacent(0)
    let c = w.get_all_edges()
    assert b == None
    assert c == None
    assert w.len() == 3
    w.set_edge(0, 1, 4)
    w.set_edge(0, 2, 5)
    w.set_edge(0, 0, 3)
    assert w.get_edge(0, 1) == 4
    w.set_edge(0, 1, 8)
    assert w.get_edge(0, 1) == 8
    b = w.get_adjacent(0)
    b = sort_vertices(b)
    c = sort_edges(c)
    c = w.get_all_edges()
    assert b == cons(0, cons(1, cons(2, None)))
    assert c == cons(WEdge(0, 0, 3), cons(WEdge(0, 1, 8), cons(WEdge(0, 2, 5), None)))
    
test 'WU Graph v2':
    let g = WuGraph(5)
    assert g.len() == 5
    g.set_edge(0, 1, None)
    assert g.get_edge(0, 1) == None
    g.set_edge(0, 1, 11)
    assert g.get_edge(0, 1) == 11
    g.set_edge(0, 1, 50)
    assert g.get_edge(0, 1) == 50
    g.set_edge(0, 4, 20)
    g.set_edge(0, 3, 23)
    let b = g.get_adjacent(0)
    b = sort_vertices(b)
    assert b == cons(1, cons(3, cons(4, None)))
    g.set_edge(3, 2, 60)
    g.set_edge(3, 4, 99)
    assert g.get_edge(3, 4) == 99
    g.set_edge(3, 4, 101)
    assert g.get_edge(3, 4) == 101
    let c = g.get_adjacent(3)
    c = sort_vertices(c)
    assert c == cons(0, cons(2, cons(4, None)))
    let p = g.get_all_edges()
    p = sort_edges(p)
    assert p == cons(WEdge(0, 1, 50), cons(WEdge(0, 3, 23), cons(WEdge(0, 4, 20), cons(WEdge(2, 3, 60), cons(WEdge(3, 4, 101), None)))))
    

###
### BUILDING GRAPHS
###

def example_graph() -> WuGraph?:
    let result = WuGraph(6) # 6-vertex graph from the assignment
    result.set_edge(0, 1, 12)
    result.set_edge(1, 2, 31)
    result.set_edge(1, 3, 56)
    result.set_edge(2, 4, -2)
    result.set_edge(3, 5, 1)
    result.set_edge(4, 3, 9)
    result.set_edge(5, 2, 7)
    return result

struct CityMap:
    let graph
    let city_name_to_node_id
    let node_id_to_city_name

def my_neck_of_the_woods():
    let nodeToCity = [0 ; 6]
    nodeToCity[0] = 'San Diego'
    nodeToCity[1] = 'Irvine'
    nodeToCity[2] = 'Los Angeles'
    nodeToCity[3] = 'San Marcos'
    nodeToCity[4] = 'Poway'
    nodeToCity[5] = 'San Clemente'
    
    let cityToNode = HashTable(6, make_sbox_hash())
    cityToNode.put('San Diego', 0)
    cityToNode.put('Irvine', 1)
    cityToNode.put('Los Angeles', 2)
    cityToNode.put('San Marcos', 3)
    cityToNode.put('Poway', 4)
    cityToNode.put('San Clemente', 5)
    
    let cityGraph = WuGraph(6)
    cityGraph.set_edge(0, 1, 86.5)
    cityGraph.set_edge(0, 2, 130)
    cityGraph.set_edge(0, 3, 35.3)
    cityGraph.set_edge(0, 4, 24.5)
    cityGraph.set_edge(0, 5, 59.1)
    cityGraph.set_edge(2, 3, 130)
    cityGraph.set_edge(2, 5, 61.5)
    cityGraph.set_edge(4, 5, 54.5)
    cityGraph.set_edge(1, 4, 80.3)
    cityGraph.set_edge(1, 3, 63.5)
  
    return CityMap(cityGraph, cityToNode, nodeToCity)
    
test 'CityMap Testing':
    let city = my_neck_of_the_woods()
    let g = city.graph
    let nameNode = city.city_name_to_node_id
    let nodeName = city.node_id_to_city_name
    let dist = g.get_edge(nameNode.get('San Diego'), nameNode.get('Irvine'))
    assert dist == 86.5
    let d2 = g.get_edge(nameNode.get('San Clemente'), nameNode.get('Los Angeles'))
    assert d2 == 61.5
    let d3 = g.get_edge(nameNode.get(nodeName[0]), nameNode.get(nodeName[2]))
    assert d3 == 130
    let n1 = nodeName[3]
    assert n1 == 'San Marcos'
    let n2 = nameNode.get('San Marcos')
    assert n2 == 3
    let adj = g.get_adjacent(nameNode.get('Los Angeles'))
    assert adj == cons(0, cons(3, cons(5, None)))

###
### DFS
###

# dfs : WUGRAPH Vertex [Vertex -> any] -> None
# Performs a depth-first search starting at `start`, applying `f`
# to each vertex once as it is discovered by the search.
def dfs(graph: WUGRAPH!, start: Vertex?, f: FunC[Vertex?, AnyC]) -> NoneC:
    let seen = [False ; graph.len()]
    def traverse(graph: WUGRAPH!, vert: Vertex?, f: FunC[Vertex?, AnyC]):
        if seen[vert] == False:
            f(vert)
            seen[vert] = True
            let curr = graph.get_adjacent(vert)
            while not curr == None:
                traverse(graph, curr.data, f)
                curr = curr.next
    return traverse(graph, start, f)
                
    
#   ^ YOUR CODE GOES HERE

# dfs_to_list : WUGRAPH Vertex -> ListOf[Vertex]
# Performs a depth-first search starting at `start` and returns a
# list of all reachable vertices.
#
# This function uses your `dfs` function to build a list in the
# order of the search. It will pass the test below if your dfs visits
# each reachable vertex once, regardless of the order in which it calls
# `f` on them. However, you should test it more thoroughly than that
# to make sure it is calling `f` (and thus exploring the graph) in
# a correct order.
def dfs_to_list(graph: WUGRAPH!, start: Vertex?) -> VertexList?:
    let list = None
    # Add to the front when we visit a node
    dfs(graph, start, lambda new: list = cons(new, list))
    # Reverse to the get elements in visiting order.
    return Cons.rev(list)

###
### TESTING
###

## You should test your code thoroughly. Here is one test to get you started:

test 'dfs_to_list(example_graph())':
    # Cons.from_vec is a convenience function from the `cons` library that
    # allows you to write a vector (using the nice vector syntax), and get
    # a linked list with the same elements.
    assert sort_vertices(dfs_to_list(example_graph(), 0)) \
        == Cons.from_vec([0, 1, 2, 3, 4, 5])
    let g = WuGraph(5)
    assert sort_vertices(dfs_to_list(g, 0)) == Cons.from_vec([0])
    g.set_edge(0, 4, 10)
    g.set_edge(0, 3, -5)
    assert sort_vertices(dfs_to_list(g, 0)) == Cons.from_vec([0, 3, 4])
    g.set_edge(3, 2, 2)
    assert sort_vertices(dfs_to_list(g, 0)) == Cons.from_vec([0, 2, 3, 4])
    let id = 0
    dfs(g, 0, lambda new: id = id + new)
    assert id == 9
    
test 'second dfs':
    let g = WuGraph(10)
    assert sort_vertices(dfs_to_list(g, 0)) == Cons.from_vec([0])
    g.set_edge(0, 1, 5)
    g.set_edge(1, 5, 10)
    g.set_edge(5, 8, 15)
    g.set_edge(8, 9, 20)
    assert sort_vertices(dfs_to_list(g, 0)) == Cons.from_vec([0, 1, 5, 8, 9])
    g.set_edge(0, 3, 30)
    g.set_edge(3, 7, 40)
    g.set_edge(0, 0, 80)
    g.set_edge(0, 2, 100)
    assert sort_vertices(dfs_to_list(g, 0)) == Cons.from_vec([0, 1, 2, 3, 5, 7, 8, 9])
    
