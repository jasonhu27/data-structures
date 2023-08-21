#lang dssl2

let eight_principles = ["Know your rights.",
    "Acknowledge your sources.",
    "Protect your work.",
    "Avoid suspicion.",
    "Do your own work.",
    "Never falsify a record or permit another person to do so.",
    "Never fabricate data, citations, or experimental results.",
    "Always tell the truth when discussing your work with your instructor."]

# Final project: Trip Planner

import cons
import sbox_hash
import 'project-lib/stack-queue.rkt' 
import 'project-lib/graph.rkt' 
import 'project-lib/dictionaries.rkt' 
import 'project-lib/binheap.rkt' 

### Basic Types ###

#  - Latitudes and longitudes are numbers:
let Lat?  = num?
let Lon?  = num?

#  - Point-of-interest categories and names are strings:
let Cat?  = str?
let Name? = str?

### Raw Item Types ###

#  - Raw positions are 2-element vectors with a latitude and a longitude
let RawPos? = TupC[Lat?, Lon?]

#  - Raw road segments are 4-element vectors with the latitude and
#    longitude of their first endpoint, then the latitude and longitude
#    of their second endpoint
let RawSeg? = TupC[Lat?, Lon?, Lat?, Lon?]

#  - Raw points-of-interest are 4-element vectors with a latitude, a
#    longitude, a point-of-interest category, and a name
let RawPOI? = TupC[Lat?, Lon?, Cat?, Name?]

### Contract Helpers ###

# ListC[T] is a list of `T`s (linear time):
let ListC = Cons.ListC
# List of unspecified element type (constant time):
let List? = Cons.list?

struct pos:
    let lat
    let lon
    
struct POI:
    let position
    let category
    let name

struct sort:
    let distance
    let ID

interface TRIP_PLANNER:

    # Returns the positions of all the points-of-interest that belong to
    # the given category.
    def locate_all(
            self,
            dst_cat:  Cat?           # point-of-interest category
        )   ->        ListC[RawPos?] # positions of the POIs

    # Returns the shortest route, if any, from the given source position
    # to the point-of-interest with the given name.
    def plan_route(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_name: Name?          # name of goal
        )   ->        ListC[RawPos?] # path to goal

    # Finds no more than `n` points-of-interest of the given category
    # nearest to the source position.
    def find_nearby(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_cat:  Cat?,          # point-of-interest category
            n:        nat?           # maximum number of results
        )   ->        ListC[RawPOI?] # list of nearby POIs


class TripPlanner (TRIP_PLANNER):
    let graph
    let pos_to_id
    let id_to_pos
    let POIs
    let POI_name_to_pos
    
    def __init__(self, rawRoad: VecC[RawSeg?], rawPOI: VecC[RawPOI?]):
        self.pos_to_id = HashTable(rawRoad.len() * 2, make_sbox_hash())
        self.id_to_pos = [None ; rawRoad.len() * 2]
        self.graph = WuGraph(rawRoad.len() * 2)
        self.POI_name_to_pos = HashTable(rawPOI.len(), make_sbox_hash())
        self.POIs = [None ; rawPOI.len()]
        let i = 0
        for v in rawRoad:
            let posn1 = pos(v[0], v[1])
            let posn2 = pos(v[2], v[3])
            let dist = ((posn2.lat - posn1.lat)**2 + (posn2.lon - posn1.lon)**2).sqrt()
            
            if self.pos_to_id.mem?(posn1) == False:
                self.pos_to_id.put(posn1, i)
                self.id_to_pos[i] = posn1
                i = i + 1
            
            if self.pos_to_id.mem?(posn2) == False:
                self.pos_to_id.put(posn2, i)
                self.id_to_pos[i] = posn2
                i = i + 1
            
            self.graph.set_edge(self.pos_to_id.get(posn1), self.pos_to_id.get(posn2), dist)
            
        for w, j in rawPOI:
            let p = [j[0], j[1]]
            let c = j[2]
            let n = j[3]
            self.POIs[w] = POI(p, c, n)
            self.POI_name_to_pos.put(n, p)
            
    def locate_all(self, category: Cat?):
        let head = None
        let curr = head
        let checked = HashTable(self.graph.len(), make_sbox_hash())
        for k in self.POIs:
            if checked.mem?(k.position) == False:
                if k.category == category:
                    checked.put(k.position, 1)
                    if head == None:
                        head = cons(k.position, None)
                        curr = head
                    else:
                        let temp = cons(k.position, None)
                        curr.next = temp
                        curr = temp
        return head 
        
    def plan_route(self, lat: Lat?, lon: Lon?, n: Name?):
        let all = self.dijkstras(lat, lon)
        if self.POI_name_to_pos.mem?(n) == False:
            return None
        else:
            let POI_pos = self.POI_name_to_pos.get(n)
            let POI_position = pos(POI_pos[0], POI_pos[1])
            let POI_ID = self.pos_to_id.get(POI_position)
            let head = None
            let curr = head
            
            if all[0][POI_ID] == inf:
                return None
            else:
                let reverse = ListStack()
                reverse.push([self.id_to_pos[POI_ID].lat, self.id_to_pos[POI_ID].lon])

                POI_ID = all[1][POI_ID]
                while not POI_ID == None:
                    reverse.push([self.id_to_pos[POI_ID].lat, self.id_to_pos[POI_ID].lon])
                    POI_ID = all[1][POI_ID]
                
                let head = None
                let curr = head
                while not reverse.empty?() == True:
                    let el = reverse.pop()
                    if head == None:
                        head = cons(el, None)
                        curr = head
                    else:
                        let temp = cons(el, None)
                        curr.next = temp
                        curr = temp
      
                return head
                 
    def find_nearby(self, lat: Lat?, lon: Lon?, category: Cat?, n: nat?):
        let dijk = self.dijkstras(lat, lon)
        let POI_dist = [inf ; self.POIs.len()]
        for i, v in self.POIs:
            let p = self.POI_name_to_pos.get(v.name)
            let vec = pos(p[0], p[1])
            let vec_pos = self.pos_to_id.get(vec)
            let combo = sort(dijk[0][vec_pos], i)
            POI_dist[i] = combo
        
        let tree = BinHeap(self.POIs.len(), lambda x, y: x.distance < y.distance)
        for p in POI_dist:
            tree.insert(p)
            
        let count = 0
        let head = None
        let curr = head
        if tree.len() == 0:
            return None
        else:
            while not count == n:
                if count == self.POIs.len():
                    break
                if tree.len() == 0:
                    break
                let min = tree.find_min()
                tree.remove_min()
                if self.POIs[min.ID].category == category:
                    if POI_dist[min.ID].distance == inf:
                        pass
                    else:
                        let pLat = self.POIs[min.ID].position[0]
                        let pLon = self.POIs[min.ID].position[1]
                        let name = self.POIs[min.ID].name
                        let ca = self.POIs[min.ID].category
                        if head == None:
                            head = cons([pLat, pLon, ca, name], None)
                            curr = head
                        else:
                            let temp = cons([pLat, pLon, ca, name], None)
                            curr.next = temp
                            curr = temp  
                        count = count + 1
        return head
        
    def dijkstras(self, lat: Lat?, lon: Lon?):
        let dist = [inf ; self.graph.len()]
        let pred = [None ; self.graph.len()]
        let p = pos(lat, lon)
        let n_pred = self.pos_to_id.get(p)
        dist[n_pred] = 0
        let todo = BinHeap(self.graph.len(), lambda x, y: dist[x] < dist[y])
        let done = [False ; self.graph.len()]
        todo.insert(self.pos_to_id.get(p))
        
        while todo.len() != 0:
            let min = todo.find_min()
            todo.remove_min()
            if done[min] == False:
                done[min] = True
                let curr = self.graph.get_adjacent(min)
                while not curr == None:
                    if dist[min] + self.graph.get_edge(min, curr.data) < dist[curr.data]:
                        dist[curr.data] = dist[min] + self.graph.get_edge(min, curr.data)
                        pred[curr.data] = min
                        todo.insert(curr.data)
                    curr = curr.next
                    
        let combined = [dist, pred]
        return combined
        


def my_first_example():
    return TripPlanner([[0,0, 0,1], [0,0, 1,0]],
                       [[0,0, "bar", "The Empty Bottle"],
                        [0,1, "food", "Pierogi"]])
                        
def my_second_example():
    return TripPlanner([[0, 0, 2, 0], [2, 0, 3, 1], [2, 0, 1, 2], [3, 1, 1, 2], [3, 2, 3, 4], [3, 2, 2, 3], [0, 0, 1, 1], [0, 0, 1, 2], [0, 2, 1, 1], [1, 1, 4, 4], [1, 2, 2, 3], [1, 2, 1, 4],
                        [1, 4, 3, 4], [2, 3, 4, 4], [1, 5, 3, 5]], 
                       [[2, 0, "food", "McDonalds"], [2, 0, "sports", "United Center"], [3, 1, "food", "Burger King"], [3, 1, "entertainment", "AMC"], [4, 4, "sports", "Wrigley Field"], 
                        [3, 1, "food", "Subway"], [1, 4, "sports", "Soldier Field"], [3, 4, "entertainment", "Bowling Alley"], [1, 5, "sports", "Guaranteed Rate Field"], [3, 5, "food", "In n Out"]])

test 'My first locate_all test':
    assert my_first_example().locate_all("food") == cons([0, 1], None)
    assert my_first_example().locate_all("bar") == cons([0, 0], None)

test 'Second locate_all test':
    assert my_second_example().locate_all("food") == cons([2, 0], cons([3, 1], cons([3, 5], None)))
    assert my_second_example().locate_all("sports") == cons([2, 0], cons([4, 4], cons([1, 4], cons([1, 5], None))))
    assert my_second_example().locate_all("entertainment") == cons([3, 1], cons([3, 4], None))
        
test 'My first plan_route test':
    assert my_first_example().plan_route(0, 1, "Pierogi") == cons([0, 1], None)
    assert my_first_example().plan_route(0, 0, "Pierogi") == cons([0,0], cons([0,1], None))
    assert my_first_example().plan_route(1, 0, "Pierogi") == cons([1, 0], cons([0, 0], cons([0, 1], None)))
    assert my_first_example().plan_route(0, 0, "The Empty Bottle") == cons([0, 0], None)
    assert my_first_example().plan_route(1, 0, "The Empty Bottle") == cons([1, 0], cons([0, 0], None))
    assert my_first_example().plan_route(0, 1, "The Empty Bottle") == cons([0, 1], cons([0, 0], None))
   
test 'Second plan_route test':
    assert my_second_example().plan_route(3, 2, "Bowling Alley") == cons([3, 2], cons([3, 4], None))
    assert my_second_example().plan_route(0, 0, "Wrigley Field") == cons([0, 0], cons([1, 1], cons([4, 4], None)))
    assert my_second_example().plan_route(1, 1, "Dillo Day") == None
    assert my_second_example().plan_route(1, 1, "In n Out") == None
    assert my_second_example().plan_route(3, 1, "Subway") == cons([3, 1], None)
    assert my_second_example().plan_route(3, 4, "McDonalds") == cons([3, 4], cons([1, 4], cons([1, 2], cons([2, 0], None))))
    assert my_second_example().plan_route(2, 3, "AMC") == cons([2, 3], cons([1, 2], cons([3, 1], None)))

test 'My first find_nearby test':
    assert my_first_example().find_nearby(0, 0, "food", 1) == cons([0,1, "food", "Pierogi"], None)
    assert my_first_example().find_nearby(0, 0, "food", 0) == None
    assert my_first_example().find_nearby(0, 0, "bar", 1) == cons([0, 0, "bar", "The Empty Bottle"], None)
    assert my_first_example().find_nearby(0, 0, "bar", 3) == cons([0, 0, "bar", "The Empty Bottle"], None)
    
test 'My second find_nearby test':
    assert my_second_example().find_nearby(3, 4, "entertainment", 3) == cons([3, 4, "entertainment", "Bowling Alley"], cons([3, 1, "entertainment", "AMC"], None))
    assert my_second_example().find_nearby(0, 2, "sports", 4) == cons([2, 0, "sports", "United Center"], cons([4, 4, "sports", "Wrigley Field"], cons([1, 4, "sports", "Soldier Field"], None)))
    assert my_second_example().find_nearby(1, 5, "entertainment", 3) == None
    assert my_second_example().find_nearby(3, 5, "sports", 2) == cons([1, 5, "sports", "Guaranteed Rate Field"], None)
    assert my_second_example().find_nearby(2, 3, "food", 0) == None
    assert my_second_example().find_nearby(2, 3, "food", 1) == cons([2, 0, "food", "McDonalds"], None)
    assert my_second_example().find_nearby(2, 3, "food", 2) == cons([2, 0, "food", "McDonalds"], cons([3, 1, "food", "Burger King"], None))
    assert my_second_example().find_nearby(2, 3, "food", 3) == cons([2, 0, "food", "McDonalds"], cons([3, 1, "food", "Burger King"], cons([3, 1, "food", "Subway"], None)))
    assert my_second_example().find_nearby(4, 4, "sports", 25) == cons([4, 4, "sports", "Wrigley Field"], cons([1, 4, "sports", "Soldier Field"], cons([2, 0, "sports", "United Center"], None)))