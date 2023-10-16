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
import sys
sys.path.append("../../../../classlib/Python")
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

def generator_of_streaam(fxs):
  while True:
    cxs = fxs()
    if cxss.ctag ==0:
      break
    else:
      fxs = cxs.cons2
      yield cxs.cons1
  raise StopIteration

def theNatPairs_cubesum():
  def helper(i0):
    return strcons_cons((i0, i0), stream_merge2(stream_tabulate(-1, lambda j0: (i0, i0+1+j0)), lambda: helper(i0+1), cube_sum_lte))
  return generator_of_stream(lambda: helper(0))