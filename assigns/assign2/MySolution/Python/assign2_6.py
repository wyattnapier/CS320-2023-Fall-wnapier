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


# def string_length(str):
#   return len(str)

# def string_get_at(str, index):
#   str[index]

# def fnlist_make_fwork(fwork):
#   res = fnlist_nil() # initialize list empty
#   # define work function to add list element to list
#   def work(x0):
#     nonlocal res # get scope of outer res
#     res = fnlist_cons(x0, res)
#     # return res # return or not??
#   fwork(work) #call work // not sure if u reassign or not
#   return fnlist_reverse(res) #reverse list because work adds to front // might not need to rev now because we rev at end

# # def work(): # might need to implement this?
# # def like_work(x0):
# #   fnlist_cons(x0, built_list)

# def string_merge(str1, str2):
#   n1 = len(str1)
#   n2 = len(str2)

#   def foreach(i1 , i2, work):
#     if(i1 < n1): 
#       if (i2 < n2):
#         c1 = string_get_at(str1, i1)
#         c2 = string_get_at(str2, i2)
#         if c1 <= c2: #do work yessir
#           print(c1)
#           return fnlist_make_fwork(lambda work: work(c1), foreach(i1+1, i2, fnlist_make_fwork))
#         else: 
#           print(c2)
#           # return string_make(c2, foreach(i1, i2 + 1, work))
#           return fnlist_make_fwork(lambda work: work(c2), foreach(i1, i2+1, fnlist_make_fwork))
#       else:
#         return "a"
#         return int1_foreach(n1-i1, lambda work: work(string_get_at(str1, i1))) #figure out work function
#         # simply append the rest of str1
#     else:
#       return "b"
#       return int1_foreach(n2-i2, lambda work: work(string_get_at(str2, i2))) #figure out work function

#   out = foreach(0, 0, fnlist_make_fwork)
#   #does this need to be reversed?
#   return out # might need to just print string instead?

# print(string_merge("135", "2468"))


#################################

#### helper ####
def string_length(str):
  return len(str)

def string_get_at(str, index):
  return str[index]
#### main ####
def string_merge(str1, str2):
  n1 = len(str1)
  n2 = len(str2)
  out = ""
  def foreach(i1 , i2):
    if(i1 < n1 and i2 < n2): 
      c1 = string_get_at(str1, i1)
      c2 = string_get_at(str2, i2)
      if c1 <= c2: #do work yessir
        return c1 + foreach(i1+1, i2)
      else: 
        return c2 + foreach(i1, i2+1)
    else:
      if (i1 < n1):
        # print("i1:", i1)
        # print("n1:", n1)
        s = ""
        for i in range(i1, n1):
          s = s + string_get_at(str1, i)
        return s
      elif (i2 < n2):
        # print("i2:", i2)
        # print("n2:", n2)
        s = ""
        for i in range(i2, n2):
          s = s + string_get_at(str2, i)
        return s

  out = foreach(0, 0)
  #does this need to be reversed?
  return out # might need to just print string instead?

print(string_merge("abcde", "1234"))