# Assign2-6: 20 points
# Please translate the following code
# into Python and then implement all the
# functions called in the code to make the
# translation (written in Python) works correctly

# Assign2-6: 20 points
# Please translate the following code
# into Python and then implement all the
# functions called in the code to make the
# translation (written in Python) works correctly
#
# let
# string_merge
# (cs1: string)(cs2: string): string =
# let n1 = string_length(cs1)
# and n2 = string_length(cs2) in

# let rec foreach(i1: int)(i2:int)(work) =
# if i1 < n1 then (
#   if i2 < n2 then let c1 = string_get_at(cs1)(i1) and c2 = string_get_at(cs2)(i2) in
  #   if c1 <= c2 then (work(c1); foreach(i1+1)(i2+0)(work))
  #   else (work(c2); foreach(i1+0)(i2+1)(work))
#   else
#   int1_foreach(n1-i1)
#     (fun i -> work(string_get_at(cs1)(i1+i)))
#) 
# else (
#   int1_foreach(n2-i2)
#     (fun i -> work(string_get_at(cs2)(i2+i)))
# )
# in
#   string_make_fwork(foreach(0)(0))
# ;;


import sys 
sys.path.append("./../../../../classlib/Python/")
from MyPython import *
sys.path.append("./../../")
from assign2.py import *

def string_length(s):
    return len(s)

def string_get_at(s, index):
    return s[index]

def fnlist_make_fwork(fwork):
    res = fnlist_nil()

    def work(x0):
        nonlocal res
        res = fnlist_cons(x0, res)

    fwork(work)
    return fnlist_reverse(res)

def string_merge(cs1, cs2):
    n1 = string_length(cs1)
    n2 = string_length(cs2)

    def foreach(i1, i2, work):
        if i1 < n1:
            if i2 < n2:
                c1 = string_get_at(cs1, i1)
                c2 = string_get_at(cs2, i2)
                if c1 <= c2:
                    work(c1)
                    return foreach(i1 + 1, i2, work)
                else:
                    work(c2)
                    return foreach(i1, i2 + 1, work)
            else:
                int1_foreach(n1 - i1, work)
        else:
            int1_foreach(n2 - i2, work)

    def work(x0):
        nonlocal result
        result = fnlist_cons(x0, result)

    result = fnlist_nil()
    foreach(0, 0, work)
    result = fnlist_reverse(result)

    return string_make_fwork(result)  # Convert the result to a string
