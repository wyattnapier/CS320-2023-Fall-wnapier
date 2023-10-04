# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python

# import sys 
# sys.path.append("./../../../../classlib/Python/")
# from MyPython import *

class MyList:
    def __init__(self, value=None, next=None):
        self.value = value
        self.next = next

# Define constructors for MyNil, MyCons, MySnoc, MyReverse, and MyAppend2

def mylist_nil():
    return MyList()

def mylist_cons(val, mylist):
    return MyList(val, mylist)

def mylist_snoc(mylist, val):
    head = mylist;
    while(next != None):

def mylist_reverse(mylist):
    # Implement this function to reverse the elements in mylist
    pass

def mylist_append2(mylist1, mylist2):
    # Implement this function to append mylist2 to mylist1
    pass

# Example usage
xs0 = mylist_nil()
xs1 = mylist_cons(1, xs0)
xs2 = mylist_snoc(xs0, 2)
xs3 = mylist_append2(xs1, xs2)
# Continue defining and using other functions as needed

# xs1 = mylist_cons(1, xs0)
# xs2 = mylist_snoc(xs0, 2)
# xs3 = mylist_append2(xs1, xs2) # length = 2
# xs4 = mylist_append2(xs3, xs3) # length = 4
# xs5 = mylist_reverse(xs4)
# xs6 = mylist_append2(xs5, xs5) # length = 8
# xs7 = mylist_append2
