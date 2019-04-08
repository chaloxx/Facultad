#include <stdio.h>
#include <pthread.h>

void *f(void *arg)
{
  static int quien;
  quien=*(int*)arg;
  printf("holo mundo! %d\n", quien);
  return NULL;
}

int main()
{
  pthread_t t[50];
  int arg[50], i;
  for(i=0; i<50; i++) {
	arg[i] = i;
	pthread_create(&t[i], 0, f, &arg[i]);
  }
  for(i=0; i<50; i++) pthread_join(t[i], NULL);
  return 0;
}
