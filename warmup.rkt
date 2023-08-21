#lang dssl2

let eight_principles = ["Know your rights.",
     "Acknowledge your sources.",
     "Protect your work.",
     "Avoid suspicion.",
     "Do your own work.",
     "Never falsify a record or permit another person to do so.",
     "Never fabricate data, citations, or experimental results.",
     "Always tell the truth when discussing your work with your instructor."]
     

# HW1: DSSL2 Warmup


###
### ACCOUNTS
###

# an Account is either a checking or a saving account
let account_type? = OrC("checking", "savings")

class Account:
    let id
    let type
    let balance

    # Constructs an account with the given ID number, account type, and
    # balance. The balance cannot be negative.
    # Note: nat = natural number (0, 1, 2, ...)
    def __init__(self, id, type, balance):
        if balance < 0: error('Account: negative balance')
        if not account_type?(type): error('Account: unknown type')
        self.id = id
        self.type = type
        self.balance = balance

    # .get_balance() -> num?
    def get_balance(self): return self.balance

    # .get_id() -> nat?
    def get_id(self): return self.id

    # .get_type() -> account_type?
    def get_type(self): return self.type

    # .deposit(num?) -> NoneC
    # Deposits `amount` in the account. `amount` must be non-negative.
    def deposit(self, amount):
        if amount < 0: 
            error('Amount to be deposited must be non-negative')
        else: 
            self.balance = self.balance + amount

    # .withdraw(num?) -> NoneC
    # Withdraws `amount` from the account. `amount` must be non-negative
    # and must not exceed the balance.
    def withdraw(self, amount):
        if amount > self.get_balance(): 
            error('Withdraw amount exceeds balance')
        elif amount < 0: 
            error('Amount to be withdrawn must be non-negative') 
        else: 
            self.balance = self.balance - amount

    # .transfer(num?, Account?) -> NoneC
    # Transfers the specified amount from this account to another. That is,
    # it subtracts `amount` from this account's balance and adds `amount`
    # to the `to` account's balance. `amount` must be non-negative.
    def transfer(self, amount, to):
        if amount < 0:
            error('Amount to be transferred must be non-negative')
        elif amount > self.get_balance():
            error('Amount to be transferred exceeds first account balance')
        else:
            self.withdraw(amount)
            to.deposit(amount)

test 'Account#deposit':
    let acc1 = Account(5, "checking", 30)
    assert acc1.get_balance() == 30
    acc1.deposit(15)
    assert acc1.get_balance() == 45
    acc1.deposit(0)
    assert acc1.get_balance() == 45
    assert_error acc1.deposit(-40)
    acc1.deposit(20)
    assert acc1.get_balance() == 65
    
test 'Account#withdraw':
    let account = Account(2, "checking", 32)
    assert account.get_balance() == 32
    account.withdraw(10)
    assert account.get_balance() == 22
    assert_error account.withdraw(-10)
    account.withdraw(0)
    assert account.get_balance() == 22
    account.withdraw(22)
    assert account.get_balance() == 0
    assert_error account.withdraw(5)
    
test 'Account#transfer':
    let a1 = Account(1, "checking", 50)
    let a2 = Account(2, "checking", 0)
    assert a1.get_balance() == 50
    assert a2.get_balance() == 0
    a1.transfer(20, a2)
    assert a1.get_balance() == 30
    assert a2.get_balance() == 20
    assert_error a1.transfer(-10, a2)
    assert_error a1.transfer(40, a2)
    a1.transfer(0, a2)
    assert a1.get_balance() == 30
    assert a2.get_balance() == 20
    a2.transfer(10, a1)
    assert a1.get_balance() == 40
    assert a2.get_balance() == 10
    

###
### CUSTOMERS
###

# Customers have names and bank accounts.
struct customer:
    let name
    let bank_account

# max_account_id(VecC[customer?]) -> nat?
# Find the largest account id used by any of the given customers' accounts.
# Raise an error if no customers are provided.
def max_account_id(customers):
    if customers.len() == 0: 
        error('No Customers are given')
    else: 
        let max = customers[0].bank_account.get_id()
        for i in customers:
            if i.bank_account.get_id() > max:
                max = i.bank_account.get_id()  
        return max

# open_account(str?, account_type?, VecC[customer?]) -> VecC[customer?]
# Produce a new vector of customers, with a new customer added. That new
# customer has the provided name, and their new account has the given type and
# a balance of 0. The id of the new account should be one more than the current
# maximum, or 1 for the first account created.
def open_account(name, type, customers):
    if customers.len() == 0:
        let ans = [0]
        ans[0] = customer(name, Account(1, type, 0))
        return ans
    else:
        let ans2 = [0; customers.len() + 1]
        for c in range(len(customers)):
            ans2[c] = customers[c]
        ans2[customers.len()] = customer(name, Account(max_account_id(customers) + 1, type, 0))
        return ans2

# check_sharing(VecC[customer?]) -> bool?
# Checks whether any of the given customers share an account number.
def check_sharing(customers):
    if customers.len() == 0:
        return False
    else:
        for r in range(len(customers)):
            for c in range(len(customers)):
                if r == c:
                    pass
                elif customers[r].bank_account.get_id() == customers[c].bank_account.get_id():
                    return True
    return False
        
#   ^ YOUR CODE GOES HERE

test 'Customer#max_account_id':
    let customers = [customer("Jason", Account(5, "checking", 25)), customer("Ronald", Account(23, "checking", 10)), customer("LeBron", Account(18, "savings", 50)), customer("Marcus", Account(72, "savings", 21))]
    assert max_account_id(customers) == 72
    let c1 = []
    assert_error max_account_id(c1)
    let c2 = [customer("Dansby", Account(7, "checking", 100))]
    assert max_account_id(c2) == 7
    
test 'Customer#open_account':
    let customers = []
    assert open_account("Jason", "savings", customers) == [customer("Jason", Account(1, "savings", 0))]
    let c1 = [customer("Matt", Account(5, "savings", 20)), customer("Alex", Account(10, "checking", 3)), customer("Rodney", Account(8, "savings", 5))]
    let opened = open_account("Brian", "checking", c1)
    assert opened[0] == customer("Matt", Account(5, "savings", 20))
    assert opened[1] == customer("Alex", Account(10, "checking", 3))
    assert opened[2] == customer("Rodney", Account(8, "savings", 5))
    assert opened[3] == customer("Brian", Account(11, "checking", 0))
    
test 'Customer#check_sharing':
    let customers = []
    let b1 = check_sharing(customers)
    assert b1 == False
    let c1 = [customer("Jordan", Account(10, "checking", 15)), customer("Dan", Account(3, "savings", 10)), customer("Reggie", Account(10, "checking", 100)), customer("Jax", Account(98, "savings", 5))]
    let b2 = check_sharing(c1)
    assert b2 == True
    let c2 = [customer("Anthony", Account(3, "checking", 40)), customer("Steph", Account(30, "checking", 20)), customer("Giannis", Account(34, "savings", 30))]
    let b3 = check_sharing(c2)
    assert b3 == False