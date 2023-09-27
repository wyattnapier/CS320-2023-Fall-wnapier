# Assign2-5: 20 points
# Please implement in Python a function
# of the name fnlist_make_fwork that corresponds
# to the function list_make_fwork in the library
# MyOCaml.ml

# let list_make_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
#   let res = ref([]) in
#     let work(x0) = (res := (x0 :: !res))
#     in(*let*)(fwork(work); list_reverse(!res) )
# ;;

import sys 
sys.path.append("./../../../../classlib/Python/")
from MyPython import *

def fnlist_make_fwork(fwork):
  res = fnlist_nil() # initialize list empty
  # define work function to add list element to list
  # when res isn't passed as param got 8/20
  def work(x0, res):
    return fnlist_cons(x0, res)
    # return res # return or not??
  res = fwork(work, res) #call work // not sure if u reassign or not
  res = fnlist_reverse(res) #reverse list because work adds to front
  return res
