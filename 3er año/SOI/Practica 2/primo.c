#include <stdio.h>

int es_primo=1;

int main(int argc, char **argv)
{
  long long i,n;
  if (argc<2) 
    return -1;
  sscanf(argv[1], "%lld", &n);
  for (i=2;i<n;i++) {
    if (n % i == 0)
    {
      es_primo=0;
      break;
    }
  }
  printf("El numero %lld %s primo.\n",n, es_primo ? "es" : "no es");
  return 0;
}