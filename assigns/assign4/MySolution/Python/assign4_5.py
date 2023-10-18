# Assign4-5:
#
# HX-2023-10-06: 30 points
# Please translate the following code from OCaml
# into Python:
#
# DONE
# let string_fset_at (cs: string)(i0: int)(c0: char) =
# string_tabulate (string_length(cs)) (fun i ->
#   if i <> i0 then string_get_at(cs)(i) else c0)
# ;;
# (* ****** ****** *)
#
# DONE
# let alphabet = string_tabulate(26)(fun i -> chr(ord('a') + i));;
#
# (* ****** ****** *)
#
# let list_of_buddies (word: string): string list =
# let n0 = string_length(word) in
# list_make_fwork (fun work ->
#    int1_foreach(n0) (fun i0 ->
#       let c0 = string_get_at(word)(i0) in
#       string_foreach(alphabet)(fun c1 -> if c1 <> c0 then work(string_fset_at(word)(i0)(c1)))))
# ;; 
# (* ****** ****** *)
#
#
################################################

def string_length(str):
  return len(str)

def string_get_at(str, i):
  return str[i]

def string_tabulate(n, fun):
  str = ""
  for i in range(n):
    str = str + fun(i)
  return str


def string_fset_at(cs, i0, c0): 
  return string_tabulate(string_length(cs), lambda i: string_get_at(cs, i) if i != i0 else c0)

def alphabet():
  return string_tabulate(26, lambda i: chr(ord('a') + i))

#delete this later its in the library
def int1_foreach(n0, work_func):
    i0 = 0
    while(i0 < n0):
        work_func(i0)
        i0 = (i0 + 1)
    return None

def fnlist_make_fwork(fwork):
 res1 = []
 fwork(lambda x: res1.append(x) if x != None else None)
 return res1

def string_foreach(str, fun):
  int1_foreach(len(str), lambda i: fun(str[i]))


def list_of_buddies(word):
  n0 = string_length(word)
  return fnlist_make_fwork(lambda work:\
                           int1_foreach(n0, lambda i0:\
                                string_foreach(alphabet(), lambda c1: \
                                               work(string_fset_at(word, i0, c1) if c1 != string_get_at(word, i0) else None))))





# def string_fset_at(cs, i0, c0):
#   def tabulate(length):
#     return [c0 if i==i0 else cs[i] for i in range(length)]
#   return tabulate(len(cs))

# #alphabet = "abcdefghijklmnopqrstuvwxyz"
# alphabet = [chr(ord('a') + i) for i in range(26)]

# def list_of_buddies(word):
#   def make_work(i0, c0):
#     return lambda string: string_fset_at(string, i0, c0)
  
#   def buddy_work(i0, c0):
#     return [work(word) for c1 in alphabet if c1 != c0 for work in [make_work(i0, c1)]]
  
#   return [buddy for i0, c0 in enumerate(word) for buddy in buddy_work(i0, c0)]