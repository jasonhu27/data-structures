#lang dssl2

let eight_principles = ["Know your rights.",
    "Acknowledge your sources.",
    "Protect your work.",
    "Avoid suspicion.",
    "Do your own work.",
    "Never falsify a record or permit another person to do so.",
    "Never fabricate data, citations, or experimental results.",
    "Always tell the truth when discussing your work with your instructor."]

# HW5: Binary Heaps

interface PRIORITY_QUEUE[X]:
    # Returns the number of elements in the priority queue.
    def len(self) -> nat?
    # Returns the smallest element; error if empty.
    def find_min(self) -> X
    # Removes the smallest element; error if empty.
    def remove_min(self) -> NoneC
    # Inserts an element; error if full.
    def insert(self, element: X) -> NoneC

# Class implementing the PRIORITY_QUEUE ADT as a binary heap.
class BinHeap[X] (PRIORITY_QUEUE):
    let _data: VecC[OrC(X, NoneC)]
    let _size: nat?
    let _lt?:  FunC[X, X, bool?]
    let _length: nat?

    # Constructs a new binary heap with the given capacity and
    # less-than function for type X.
    def __init__(self, capacity, lt?):
        self._data = [None ; capacity]
        self._size = capacity
        self._lt? = lt?
        self._length = 0
        
    def len(self):
        return self._length
    
    def find_min(self):
        if self._length == 0:
            error('The priority queue is empty')
        else:
            return self._data[0]
    
    def remove_min(self):
        if self._length == 0:
            error('The priority queue ie empty')
        else:
            self.percolateDown()
   
    def insert(self, element: X):
        if self._length == self._size:
            error('The priority queue has reached its capacity')
        else:
            if self._length == 0:
                self._data[0] = element
            else:
                self._data[self._length] = element
                self.bubbleUp(self._length)
            self._length = self._length + 1
            
# Other methods you may need can go here.
    def swap(self, idx1: nat?, idx2: nat?):
        let temp = self._data[idx1]
        self._data[idx1] = self._data[idx2]
        self._data[idx2] = temp
    
    def bubbleUp(self, idx: nat?):
        while not idx == 0:
            let parent = ((idx - 1) / 2).floor()
            if self._lt?(self._data[idx], self._data[parent]):
                self.swap(idx, parent)
                idx = parent
            else:
                break
     
    def percolateDown(self):
        if self._length == 1:
            self._data[0] = None
            self._length = self._length - 1
        elif self._length == 2:
            self.swap(0, 1)
            self._data[1] = None
            self._length = self._length - 1
        elif self._length == 3:
            self.swap(0, 2)
            self._data[2] = None
            if self._lt?(self._data[0], self._data[1]):
                pass
            else:
                self.swap(0, 1)  
            self._length = self._length - 1
        else:       
            self._length = self._length - 1
            self.swap(0, self._length)
            self._data[self._length] = None
            let idx = 0
            while True:
                let left = (2 * idx) + 1
                let right = (2 * idx) + 2
                if (left >= self._length) | (right >= self._length):
                    break
                else:
                    let leftChild = self._data[left]
                    let rightChild = self._data[right]
                    if self._lt?(self._data[idx], leftChild) & self._lt?(self._data[idx], rightChild):
                        break
                    if self._lt?(leftChild, rightChild):
                        self.swap(idx, left)
                        idx = left
                    else:
                        self.swap(idx, right)
                        idx = right
                                  
    def print(self):
        return self._data
        
test 'Edge Cases':
    let h = BinHeap[nat?](1, λ x, y: x < y)
    assert_error h.find_min()
    assert_error h.remove_min()
    h.insert(1)
    assert h.len() == 1
    assert h.find_min() == 1
    assert_error h.insert(2)
    assert h.len() == 1
    let b = BinHeap[nat?](5, λ x, y: x < y)
    b.insert(8)
    b.insert(8)
    assert b.len() == 2
    let c = b.print()
    assert c == [8, 8, None, None, None]
    assert b.find_min() == 8
    b.insert(3)
    c = b.print()
    assert c == [3, 8, 8, None, None]
    assert b.find_min() == 3
    b.insert(10)
    assert b.len() == 4
    c = b.print()
    assert c == [3, 8, 8, 10, None]
    assert b.find_min() == 3
    b.insert(8)
    assert b.len() == 5 
    c = b.print()
    assert c == [3, 8, 8, 10, 8]
    assert_error b.insert(10)
    assert b.find_min() == 3
    b.remove_min()
    c = b.print()
    assert c == [8, 8, 8, 10, None]
    assert b.len() == 4
    assert b.find_min() == 8
    b.remove_min()
    c = b.print()
    assert c == [8, 8, 10, None, None]
    assert b.len() == 3
    assert b.find_min() == 8
    b.remove_min()
    c = b.print()
    assert c == [8, 10, None, None, None]
    assert b.len() == 2
    assert b.find_min() == 8
    b.remove_min()
    c = b.print()
    assert c == [10, None, None, None, None]
    assert b.len() == 1
    assert b.find_min() == 10
    b.remove_min()
    c = b.print()
    assert c == [None, None, None, None, None]
    assert b.len() == 0
    assert_error b.find_min()
    assert_error b.remove_min()

test 'Regular Test':
    # The `nat?` here means our elements are restricted to `nat?`s.
    let h = BinHeap[nat?](10, λ x, y: x < y)
    h.insert(5)
    assert h.find_min() == 5
    assert h.len() == 1
    let b = h.print()
    assert b == [5, None, None, None, None, None, None, None, None, None]
    h.insert(3)
    assert h.len() == 2
    assert h.find_min() == 3
    b = h.print()
    assert b == [3, 5, None, None, None, None, None, None, None, None]
    h.insert(4)
    assert h.len() == 3
    assert h.find_min() == 3
    b = h.print()
    assert b == [3, 5, 4, None, None, None, None, None, None, None]
    h.insert(1)
    assert h.len() == 4
    assert h.find_min() == 1
    b = h.print()
    assert b == [1, 3, 4, 5, None, None, None, None, None, None]
    h.insert(2)
    assert h.len() == 5
    assert h.find_min() == 1
    assert b == [1, 2, 4, 5, 3, None, None, None, None, None]
    h.remove_min()
    assert h.len() == 4
    b = h.print()
    assert b == [2, 3, 4, 5, None, None, None, None, None, None]
    assert h.find_min() == 2
    h.remove_min()
    assert h.len() == 3
    b = h.print()
    assert b == [3, 5, 4, None, None, None, None, None, None, None]
    assert h.find_min() == 3
    h.remove_min()
    assert h.len() == 2
    b = h.print()
    assert b == [4, 5, None, None, None, None, None, None, None, None]
    h.remove_min()
    assert h.len() == 1
    b = h.print()
    assert b == [5, None, None, None, None, None, None, None, None, None]
    h.remove_min()
    assert h.len() == 0
    b = h.print()
    assert b == [None, None, None, None, None, None, None, None, None, None]
    assert_error h.remove_min()
    
test 'max heap':
    let h = BinHeap[nat?](10, λ x, y: x > y)
    h.insert(5)
    assert h.find_min() == 5
    assert h.len() == 1
    let b = h.print()
    assert b == [5, None, None, None, None, None, None, None, None, None]
    h.insert(10)
    assert h.find_min() == 10
    assert h.len() == 2
    b = h.print()
    assert b == [10, 5, None, None, None, None, None, None, None, None]
    h.insert(15)
    assert h.find_min() == 15
    assert h.len() == 3
    b = h.print()
    assert b == [15, 5, 10, None, None, None, None, None, None, None]
    h.insert(2)
    assert h.find_min() == 15
    assert h.len() == 4
    b = h.print()
    assert b == [15, 5, 10, 2, None, None, None, None, None, None]
    h.insert(8)
    assert h.find_min() == 15
    assert h.len() == 5
    b = h.print()
    assert b == [15, 8, 10, 2, 5, None, None, None, None, None]
    h.insert(13)
    assert h.find_min() == 15
    assert h.len() == 6
    b = h.print()
    assert b == [15, 8, 13, 2, 5, 10, None, None, None, None]
    h.insert(20)
    assert h.find_min() == 20
    assert h.len() == 7
    b = h.print()
    assert b == [20, 8, 15, 2, 5, 10, 13, None, None, None]
    h.remove_min()
    assert h.len() == 6
    b = h.print()
    assert b == [15, 8, 13, 2, 5, 10, None, None, None, None]
    h.remove_min()
    assert h.len() == 5
    b = h.print()
    assert b == [13, 8, 10, 2, 5, None, None, None, None, None]
    h.remove_min()
    assert h.len() == 4
    b = h.print()
    assert b == [10, 8, 5, 2, None, None, None, None, None, None]
    h.remove_min()
    assert h.len() == 3
    b = h.print()
    assert b == [8, 2, 5, None, None, None, None, None, None, None]
    h.remove_min()
    assert h.len() == 2
    b = h.print()
    assert b == [5, 2, None, None, None, None, None, None, None, None]
    h.remove_min()
    assert h.len() == 1
    b = h.print()
    assert b == [2, None, None, None, None, None, None, None, None, None]
    h.remove_min()
    assert h.len() == 0
    b = h.print()
    assert b == [None, None, None, None, None, None, None, None, None, None]
    assert_error h.remove_min()
    
# Sorts a vector of Xs, given a less-than function for Xs.
#
# This function performs a heap sort by inserting all of the
# elements of v into a fresh heap, then removing them in
# order and placing them back in v.
def heap_sort[X](v: VecC[X], lt?: FunC[X, X, bool?]) -> NoneC:
    let h = BinHeap(v.len(), lt?)
    for i in range(v.len()):
        h.insert(v[i])
        
    for j in range(v.len()):
        let a = h.find_min()
        v[j] = a
        h.remove_min()       
        

test 'heap sort descending':
    let v = [3, 6, 0, 2, 1]
    heap_sort(v, λ x, y: x > y)
    assert v == [6, 3, 2, 1, 0]
    
test 'heap sort ascending':
    let v = [10, 3, 0, 6, 9]
    heap_sort(v, λ x, y: x < y)
    assert v == [0, 3, 6, 9, 10]
    
test 'heap sort additional':
    let v = [1, 2, 3, 4, 5, 6, 7]
    heap_sort(v, λ x, y: x > y)
    assert v == [7, 6, 5, 4, 3, 2, 1]
    
    let v1 = [934, 21, 34, 21, 73, 100, 21]
    heap_sort(v1, λ x, y: x < y)
    assert v1 == [21, 21, 21, 34, 73, 100, 934]
    
    let v2 =[9, 3, 8, 1, 0, 0, 0, 0]
    heap_sort(v2, λ x, y: x > y)
    assert v2 == [9, 8, 3, 1, 0, 0, 0, 0]

# Sorting by birthday.

struct person:
    let name: str?
    let birth_month: nat?
    let birth_day: nat?

def earliest_birthday() -> str?:
    let v = [0 ; 6]
    v[0] = person('Padmavathi', 9, 24)
    v[1] = person('Satyasai', 12, 10)
    v[2] = person('Krishna', 4, 15)
    v[3] = person('Swati', 8, 20)
    v[4] = person('Swaroop', 6, 30)
    v[5] = person('Hemant', 7, 30)
    
    def birthday_compare(x: person?, y: person?):
        if x.birth_month != y.birth_month:
            return x.birth_month < y.birth_month
        else:
            return x.birth_day < y.birth_day
        pass
        
    heap_sort(v, λ x, y: birthday_compare(x, y))
    return v[0].name
    
test 'earliest_birthday':
    let a = earliest_birthday()
    assert a == 'Krishna'
