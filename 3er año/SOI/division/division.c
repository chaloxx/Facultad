

struct CR {
  unsigned c, r;
};

struct CR divmod(unsigned m, unsigned n)
{
  struct CR res;
  int i; /* int de 32 bits */
  
  res.c = res.r = 0;
  for (i = 0; i < 32; i++) {
    res.r <<= 1;
    res.c <<= 1;
    if (m & 0x80000000)
      res.r += 1;
    m <<= 1;
    if (res.r >= m) {
      res.c += 1;
      res.r -= m;
    }
  }
  return res;
}

