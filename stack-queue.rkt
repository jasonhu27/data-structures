#lang dssl2

let eight_principles = ["Know your rights.",
    "Acknowledge your sources.",
    "Protect your work.",
    "Avoid suspicion.",
    "Do your own work.",
    "Never falsify a record or permit another person to do so.",
    "Never fabricate data, citations, or experimental results.",
    "Always tell the truth when discussing your work with your instructor."]    

# HW2: Stacks and Queues

import ring_buffer

interface STACK[T]:
    def push(self, element: T) -> NoneC
    def pop(self) -> T
    def empty?(self) -> bool?

# Defined in the `ring_buffer` library; copied here for reference.
# Do not uncomment! or you'll get errors.
# interface QUEUE[T]:
#     def enqueue(self, element: T) -> NoneC
#     def dequeue(self) -> T
#     def empty?(self) -> bool?

# Linked-list node struct (implementation detail):
struct _cons:
    let data
    let next: OrC(_cons?, NoneC)

###
### ListStack
###

class ListStack[T] (STACK):

    # Any fields you may need can go here.
    let head
    let length

    # Constructs an empty ListStack.
    def __init__ (self):
        self.head = None
        self.length = 0

    # Other methods you may need can go here.
    def push(self, element: T):
        self.head = _cons(element, self.head)
        self.length = self.length + 1
    def pop(self):
        let val = self.head.data
        if self.length == 0:
            error('The stack is empty') 
        elif self.length == 1:
            self.head = None
            self.length = 0
        else:
            self.length = self.length - 1
            self.head = self.head.next
        return val
    def empty?(self):
        if self.length == 0:
            return True
        return False
        
test "stack_test1":
    let s = ListStack()
    assert_error s.pop()
    s.push(2)
    assert s.pop() == 2
    assert s.empty?() == True
    s.push(4)
    s.push(5)
    s.push(6)
    assert s.pop() == 6
    assert s.pop() == 5
    assert s.pop() == 4
    s.push(7)
    s.push(12)
    s.push(-1)
    assert s.pop() == -1
    assert s.pop() == 12
    assert s.pop() == 7
    
test "stack_test2":
    let t = ListStack()
    assert_error t.pop()
    t.push(None)
    t.push("hello")
    t.push(4)
    assert t.empty?() == False
    assert t.pop() == 4
    assert t.pop() == "hello"
    assert t.pop() == None
    assert t.empty?() == True
  

###
### ListQueue
###

class ListQueue[T] (QUEUE):

    # Any fields you may need can go here.
    let head
    let tail
    let length

    # Constructs an empty ListQueue.
    def __init__ (self):
        self.head = None
        self.tail = None
        self.length = 0

    def enqueue(self, element: T):         
        if self.length == 0:
            self.head = _cons(element, self.head)
            self.tail = self.head
            pass
        else:
            let temp
            temp = _cons(element, None)
            self.tail.next = temp
            self.tail = temp     
        self.length = self.length + 1        
    def dequeue(self):
        let value
        value = self.head.data
        if self.length == 0:
            error('The queue is empty')
        elif self.length == 1:
            self.head = None
            self.tail = None
            self.length = self.length - 1
        else:
            self.length = self.length - 1 
            self.head = self.head.next
        return value
    def empty?(self):
        if self.length == 0:
            return True
        else:
            return False

test "queue_test":
    let q = ListQueue()
    q.enqueue(2)
    q.enqueue(5)
    q.enqueue(10)
    assert q.dequeue() == 2
    assert q.dequeue() == 5
    assert q.empty?() == False
    assert q.dequeue() == 10
    assert q.empty?() == True
    assert_error q.dequeue()
    q.enqueue(13)
    q.enqueue(None)
    assert q.dequeue() == 13
    assert q.empty?() == False
    q.enqueue("Hello")
    q.enqueue("World")
    assert q.dequeue() == None
    assert q.dequeue() == "Hello"
    assert q.dequeue() == "World"
    assert q.empty?() == True

###
### Playlists
###

struct song:
    let title: str?
    let artist: str?
    let album: str?

# Enqueue six songs of your choice to the given queue, then return the first
# song that should play.
def fill_playlist (q: QUEUE!):
    if q.empty?() == False:
        error('Queue given is not empty')
    else:
        q.enqueue(song("Know Yourself", "Drake", "If You're Reading This It's Too Late"))
        q.enqueue(song("Used to Be", "AJ Mitchell", "SKYVIEW"))
        q.enqueue(song("Neighbors", "J. Cole", "4 Your Eyez Only"))
        q.enqueue(song("One Thing", "One Direction", "Up All Night"))
        q.enqueue(song("ILoveUIHateU", "Playboi Carti", "Whole Lotta Red"))
        q.enqueue(song("Nonstop", "Drake", "Scorpion"))
        q.enqueue(song("TEST DRIVE", "Joji", "BALLADS 1"))
    return q.dequeue()

test "ListQueue playlist":
    let p = ListQueue()
    assert fill_playlist(p) == song("Know Yourself", "Drake", "If You're Reading This It's Too Late")
    let m = ListQueue()
    m.enqueue(5)
    assert_error fill_playlist(m)

# To construct a RingBuffer: RingBuffer(capacity)
test "RingBuffer playlist":
    let r = RingBuffer(7)
    assert fill_playlist(r) == song("Know Yourself", "Drake", "If You're Reading This It's Too Late")
    let small = RingBuffer(6)
    assert_error fill_playlist(small)
    let t = RingBuffer(7)
    t.enqueue(1)
    assert_error fill_playlist(t)
