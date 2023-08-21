#lang dssl2

let eight_principles = ["Know your rights.",
    "Acknowledge your sources.",
    "Protect your work.",
    "Avoid suspicion.",
    "Do your own work.",
    "Never falsify a record or permit another person to do so.",
    "Never fabricate data, citations, or experimental results.",
    "Always tell the truth when discussing your work with your instructor."] 

# HW3: Dictionaries

import sbox_hash

# A signature for the dictionary ADT. The contract parameters `K` and
# `V` are the key and value types of the dictionary, respectively.
interface DICT[K, V]:
    # Returns the number of key-value pairs in the dictionary.
    def len(self) -> nat?
    # Is the given key mapped by the dictionary?
    def mem?(self, key: K) -> bool?
    # Gets the value associated with the given key; calls `error` if the
    # key is not present.
    def get(self, key: K) -> V
    # Modifies the dictionary to associate the given key and value. If the
    # key already exists, its value is replaced.
    def put(self, key: K, value: V) -> NoneC
    # Modifes the dictionary by deleting the association of the given key.
    def del(self, key: K) -> NoneC
    # The following method allows dictionaries to be printed
    def __print__(self, print)

struct _cons:
    let data
    let next: OrC(_cons?, NoneC)

struct kv:
    let key
    let value

class AssociationList[K, V] (DICT):
    let _head
    let _tail
    let length

    def __init__(self):
        self._head = None
        self._tail = None
        self.length = 0   
        
    def len(self):
        return self.length
        
    def mem?(self, key: K):
        if self.existValue(key) == None:
            return False
        else:
            return True
        
    def get(self, key: K):
        if (self.mem?(key)) == False:
            error('This key is not present in the dictionary')
        else:
            return self.existValue(key)
            
    def put(self, key: K, value: V):
        let curr = self._head
        if self.length == 0:
            self._head = _cons(kv(key, value), None)
            self._tail = self._head
            self.length = self.length + 1
        elif (self.mem?(key)) == True:
            while not curr == None:
                if curr.data.key == key:
                    curr.data.value = value
                curr = curr.next
        else:
            let temp = _cons(kv(key, value), None)
            self._tail.next = temp
            self._tail = temp
            self.length = self.length + 1
            
    def del(self, key: K):
        if self.mem?(key) == False:
            pass
        else:
            if self._head.data.key == key:
                self._head = self._head.next
            else:
                let curr = self._head
                let prev = curr
                curr = curr.next
                while not curr == None:
                    if curr.data.key == key:
                        prev.next = curr.next
                    prev = prev.next
                    curr = curr.next
            self.length = self.length - 1

    # See above.
    def __print__(self, print):
        print("#<object:AssociationList head=%p>", self._head)

    # Other methods you may need can go here.
    def existValue(self, key: K):
        let curr = self._head
        while not curr == None:
            if curr.data.key == key:
                return curr.data.value
            curr = curr.next
        return None
        
test 'EmptyAssociationList + Tests':
    let b = AssociationList()
    assert b.len() == 0
    assert b.mem?('Check') == False
    assert_error b.get('Check')
    b.del('Hello')
    assert b.len() == 0
    b.put('hello', 5)
    b.put('World', 'Yes')
    assert b.get('World') == 'Yes'
    assert_error b.get('NO')
    b.put('World', 'NO')
    assert b.len() == 2
    assert b.get('World') == 'NO'
    b.put('Darnell', 'Wright')
    b.put('Justin', 'Fields')
    assert b.put('DJ', 2) == None
    assert b.len() == 5
    assert b.del('Darnell') == None
    assert b.len() == 4
    assert b.get('DJ') == 2
    assert b.get('Justin') == 'Fields'
    assert_error b.get('Phone')
    assert b.del('Aaron') == None
    assert b.len() == 4
    b.put('Justin', 'JF1')
    assert b.get('Justin') == 'JF1'
    assert b.len() == 4
    
test 'Extra Test':
    let a = AssociationList()
    assert a.mem?('hello') == False
    assert a.put('hello', 5) == None
    assert a.len() == 1
    assert a.mem?('hello') == True
    assert a.get('hello') == 5
    assert_error a.get('John')
    a.put('world', 28)
    a.put('jason', 14)
    assert a.len() == 3
    a.del('roddy')
    assert a.len() == 3
    a.del('world')
    assert a.len() == 2
    assert a.mem?('world') == False
    assert a.get('hello') == 5
    assert a.get('jason') == 14
    a.put('hello', 10)
    assert a.get('hello') == 10

class HashTable[K, V] (DICT):
    let _hash
    let _size
    let _data
    let _numBuckets

    def __init__(self, nbuckets: nat?, hash: FunC[AnyC, nat?]):
        self._hash = hash
        self._size = 0
        self._numBuckets = nbuckets
        self._data = [None ; nbuckets]
        
    def len(self):
        return self._size
        
    def mem?(self, key: K):
        let index = self.hash_value(key)
        if self.findValue(key, index) == None:
            return False
        else:
            return True
        
    def get(self, key: K):
        let index = self.hash_value(key)
        if self.mem?(key) == False:
            error('This key is not present in the dictionary')
        else:
            return self.findValue(key, index)
        
    def put(self, key: K, value: V):
        let index = self.hash_value(key)
        let curr = self._data[index]
        if curr == None:
            self._data[index] = _cons(kv(key, value), None)
            self._size = self._size + 1
        elif self.mem?(key) == False:
            while not curr == None:
                let temp = curr.next
                if temp == None:
                    curr.next = _cons(kv(key, value), None)
                    self._size = self._size + 1
                    break
                curr = curr.next
        else:
            while not curr == None:
                if curr.data.key == key:
                    curr.data.value = value
                    break
                curr = curr.next
            
    def del(self, key: K):
        let index = self.hash_value(key)
        if self.mem?(key) == False:
            pass
        else:
            if self._data[index].data.key == key:
                self._data[index] = self._data[index].next
                self._size = self._size - 1
                pass
            else:
                let curr = self._data[index]
                if curr.data.key == key:
                    self._data[index] = None
                    self._size = self._size - 1
                else:
                    let temp = curr.next
                    while not temp == None:
                        if temp.data.key == key:
                            curr.next = temp.next
                            self._size = self._size - 1
                            break
                        curr = curr.next
                        temp = temp.next
            
    # This avoids trying to print the hash function, since it's not really
    # printable and isn’t useful to see anyway:
    def __print__(self, print):
        print("#<object:HashTable  _hash=... _size=%p _data=%p>",
              self._size, self._data)

    # Other methods you may need can go here.
    def findValue(self, key: K, index):
        let curr = self._data[index]
        while not curr == None:
            if curr.data.key == key:
                return curr.data.value
            curr = curr.next
        return None
        
    def hash_value(self, key: K):
        return self._hash(key) % self._numBuckets


# first_char_hasher(String) -> Natural
# A simple and bad hash function that just returns the ASCII code
# of the first character.
# Useful for debugging because it's easily predictable.
def first_char_hasher(s: str?) -> int?:
    if s.len() == 0:
        return 0
    else:
        return int(s[0])

test 'empty + sbox hash test':
    let h = HashTable(10, make_sbox_hash())
    assert h.len() == 0
    assert h.mem?('No') == False
    assert_error h.get('check')
    assert h.del('Ryan') == None
    assert h.len() == 0
    assert h.put('hello', 5) == None
    assert h.mem?('hello') == True
    assert h.len() == 1
    assert h.get('hello') == 5
    h.put('Jason', 23)
    assert h.len() == 2
    assert h.mem?('Jason') == True
    assert h.get('Jason') == 23
    h.put('Jason', 'Lebron')
    assert h.get('Jason') == 'Lebron'
    assert h.len() == 2
    h.del('Jason')
    assert h.len() == 1
    assert h.mem?('Jason') == False
    h.put(24, 'Jordan')
    h.put('Boston', 'Celtics')
    h.put('Anakin', 'Skywalker')
    assert h.len() == 4
    assert h.get('Anakin') == 'Skywalker'
    assert h.mem?('Skywalker') == False
    assert h.del('Boston') == None
    assert h.len() == 3
    assert h.mem?('Boston') == False
    
test 'Collisions':
    let h = HashTable(5, first_char_hasher)
    h.put('hello', 5)
    assert h.len() == 1
    h.put('help', 10)
    h.put('hop', 15)
    h.put('han', 'solo')
    assert h.len() == 4
    h.del('help')
    assert_error h.get('help')
    assert h.len() == 3
    assert h.mem?('help') == False
    assert h.mem?('hello') == True
    assert h.mem?('hop') == True
    assert h.mem?('han') == True
    assert h.get('hello') == 5
    assert h.get('hop') == 15
    assert h.get('han') == 'solo'
    h.del('hop')
    assert h.mem?('hop') == False
    assert h.len() == 2
    assert_error h.get('hop')
    assert h.mem?('hello') == True
    assert h.get('hello') == 5
    assert h.mem?('han') == True
    assert h.get('han') == 'solo'
    assert h.put('han', 'ben') == None
    assert h.len() == 2
    assert h.get('han') == 'ben'
    
test 'Invalid Test':
    let o = HashTable(10, int)
    o.put(5, 'five')
    o.put(15, 'fifteen')
    o.put(25, 'twenty-five')
    assert o.len() == 3
    o.del(5)
    assert o.len() == 2
    assert o.mem?(5) is False
    assert o.mem?(15) is True
    assert o.mem?(25) is True
    
def compose_menu(d: DICT!) -> DICT?:
    d.put('Sara', ['Channa masala', 'Indian'])
    d.put('Branden', ['Apple pie', 'American'])
    d.put('Krishna', ['Koshary', 'Egyptian'])
    d.put('Aymeric', ['Risotto', 'Italian'])
    d.put('Sadhana', ['Utthapam & sambar', 'Indian'])
    d.put('Jason', ['Sushi', 'Japanese'])
    return d

test "AssociationList menu":
    let a = AssociationList()
    a.put('Test', 'Result')
    a = compose_menu(a)
    assert a.get('Sara')[1] == 'Indian'
    assert a.get('Jason')[1] == 'Japanese'
    
test "HashTable menu":
    let h = HashTable(7, make_sbox_hash())
    h.put('Test', 'Result')
    h = compose_menu(h)
    assert h.get('Branden')[1] == 'American'
    assert h.get('Krishna')[1] == 'Egyptian'
    