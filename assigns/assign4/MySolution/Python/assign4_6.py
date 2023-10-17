# Assign4-6:
#
# HX-2023-10-06: 30 points (bonus)
#
# (*
# //
# Please implement the following function
# that enumerates all the pairs (i, j) of natural
# numbers satisfying $i <= j$; a pair (i1, j1) must
# be enumerated ahead of another pair (i2, j2) if the
# following condition holds:
#   i1*i1*i1 + j1*j1*j1 < i2*i2*i2 + j2*j2*j2
# //
# let
# theNatPairs_cubesum(): (int * int) stream = fn () =>
# //
# *)
#
# Your implementation of theNatPairs_cubesum should
# return a generator corresponding to the OCaml stream
# If you have already implement a stream in Python, you
# turn it into a generator by calling the following fun:
#
# def generator_of_stream(fxs):
#     while True:
#         cxs = fxs()
#         if cxs.ctag == 0:
#             break
#         else:
#             fxs = cxs.cons2
#             yield cxs.cons1
#     raise StopIteration
#
# def theNatPairs_cubesum(): # please give your implementation
#
################################################

###########################################################################

# class strcon:
#   ctag = -1
#   def get_ctag(self):
#       return self.ctag
#   def __iter__(self):
#       return strcon_iter(self)
# # end-of-class(strcon)

# class strcon_nil(strcon):
#   def __init__(self):
#       self.ctag = 0
#       return None
# # end-of-class(strcon_nil)

# class strcon_cons(strcon):
#   def __init__(self, cons1, cons2):
#       self.ctag = 1
#       self.cons1 = cons1
#       self.cons2 = cons2
#       return None
#   def get_cons1(self):
#       return self.cons1
#   def get_cons2(self):
#       return self.cons2
# # end-of-class(strcon_cons)

# class strcon_iter:
#     def __iter__(self):
#         return self
#     def __init__(self, itms):
#         self.itms = itms
#     def __next__(self):
#         if (self.itms.ctag==0):
#             raise StopIteration
#         else:
#             itm1 = self.itms.cons1
#             self.itms = self.itms.cons2()
#             return itm1

###########################################################################

import sys 
sys.path.append("./../../../../classlib/Python/")
from MyPython import *

def cube_sum(ij):
  i = ij[0]
  j = ij[1]
  return i*i*i + j*j*j

def cub_sum_lte(ij1, ij2):
  return (cube_sum(ij1) <= cube_sum(ij2))

def strcon_merge2(cxs1, cxs2, lte3):
  cx1 = cxs1.cons1
  cx2 = cxs2.cons1
  if lte3(cx1, cx2):
    return strcon_cons(cx1, lambda: strcon_merge2((cxs1.cons2)(), cxs2, lte3))
  else:
    return strcon_cons(cx2, lambda: strcon_merge2(cxs1, (cxs2.cons2)(), lte3))
  
def stream_merge2(fxs1, fxs2, lte3):
  return lambda: strcon_merge2(fxs1(), fxs2(), lte3)

def generator_of_stream(fxs):
  while True:
    cxs = fxs()
    if cxs.ctag == 0:
      break
    else:
      fxs = cxs.cons2
      yield cxs.cons1
  raise StopIteration

def theNatPairs_cubesum():
  def helper(i0):
    return strcons_cons((i0, i0), stream_merge2(stream_tabulate(-1, lambda j0: (i0, i0+1+j0)), lambda: helper(i0+1), cube_sum_lte))
  return generator_of_stream(lambda: helper(0))