def int cond(bool b)
{
  int x;
  if (b)
    x = 42;
  else
    x = 17;
  return x;
}

def int main()
{
 print(cond(true));
 print(cond(false));
 return 0;
}
