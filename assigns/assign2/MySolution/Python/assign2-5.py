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
sys.path.append("./../../")
from assign2 import *

def fnlist_make_fwork(fwork):
  print("I like camels")
