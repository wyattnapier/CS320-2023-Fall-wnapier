# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python
########################################################################
# class MyList:
#     def __init__(self) -> None:
#         id_tag= -1
#     get(tag):
#         return self.ctag

# mylist_nil():
#     set tag to 0

# mylist_cons:
#     set tag to 1

# tags represent what type of list it is

# cons1 (first element) cons2 rest of it:
#     need getter functions where you can get cons 1 or cons 2 elemnts

# same idea for snoc

# reverse you need to pass in reverse list
# in getter function you need to return head or tail

# my list for each and my list rforeach
#     use if else and elif and shit
#     could see if there is patten matching potentially

# define nil, snoc, cons, and append which all inherit initial mylist class
########################################################################
def mylist_foreach(lst, work_func):
    if (lst.get_id_tag()==0):
        return None
    else:
        work_func(lst.get_itm1())
        mylist_foreach(lst.get_itm2(), work_func)

def mylist_rforeach(lst, work_func):
    if (lst.get_id_tag()==0):
        return None
    else:
        mylist_rforeach(lst.get_itm2(), work_func)
        work_func(lst.get_itm1())

class mylist:
    id_tag = -1
    def get_id_tag(self):
        return self.id_tag
    
class mylist_nil(mylist):
    def __init__(self):
        self.id_tag = 0

class mylist_cons(mylist):
    def __init__(self, itm1, itm2):
        self.id_tag = 1
        self.itm1 = itm1
        self.itm2 = itm2
    def get_itm1(self):
        return self.itm1
    def get_itm2(self):
        return self.itm2

def mylist_sing(itm):
    return mylist_cons(itm, mylist_nil())

def mylist_reverse(lst):
    if (lst.get_id_tag()==0):
        return mylist_nil()
    else:
        return mylist_snoc(mylist_reverse(lst.get_itm2()), lst.get_itm1())

def mylist_snoc(lst, itm):
    if (lst.get_id_tag()==0):
        return mylist_sing(itm)
    else:
        return mylist_cons(lst.get_itm1(), mylist_snoc(lst.get_itm2(), itm))

def mylist_print(lst):
    if (lst.get_id_tag()==0):
        print("[]")
    else:
        print("[", end="")
        mylist_print_aux(lst)
        print("]")

def mylist_print_aux(lst):
    if (lst.get_id_tag()==0):
        return None
    elif (lst.get_itm2().get_id_tag()==0):
        print(lst.get_itm1(), end="")
    else:
        print(lst.get_itm1(), end=", ")
        mylist_print_aux(lst.get_itm2())

def mylist_append2(lst1, lst2):
    if (lst1.get_id_tag()==0):
        return lst2
    else:
        return mylist_cons(lst1.get_itm1(), mylist_append2(lst1.get_itm2(), lst2))

###########################################################################