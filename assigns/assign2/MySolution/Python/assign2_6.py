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