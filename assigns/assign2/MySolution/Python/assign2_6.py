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

def string_length(str):
  return len(str)

def string_get_at(str, index):
  str[index]

def fnlist_make_fwork(fwork):
  res = fnlist_nil() # initialize list empty
  # define work function to add list element to list
  def work(x0):
    nonlocal res # get scope of outer res
    res = fnlist_cons(x0, res)
    # return res # return or not??
  fwork(work) #call work // not sure if u reassign or not
  return fnlist_reverse(res) #reverse list because work adds to front // might not need to rev now because we rev at end

# def work(): # might need to implement this?

def string_merge(str1, str2):
  n1 = len(str1)
  n2 = len(str2)

  def foreach(i1, i2, work):
    if(i1 < n1): 
      if (i2 < n2):
        c1 = string_get_at(str1, i1)
        c2 = string_get_at(str2, i2)
        if c1 <= c2: #do work yessir
          return fnlist_make_fwork(lambda work: work(c1), foreach(i1+1, i2, fnlist_make_fwork)))
        else: 
          # return string_make(c2, foreach(i1, i2 + 1, work))
          return fnlist_make_fwork(lambda work: work(c2), foreach(i1, i2+1, fnlist_make_fwork)))
      else:
        return int1_foreach(n1-i1, work) #figure out work function
        # simply append the rest of str1
    else:
      return int1_foreach(n2-i2, work) #figure out work function

  out = foreach(str1, str2, fnlist_make_fwork)
  return fnlist_reverse(out) # might need to just print string instead?