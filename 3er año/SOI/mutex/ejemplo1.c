#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

int n;

void *f(void *arg)
{
  int quien = *(int*)arg;
  for(;;){
	n=quien;
	sleep(1);
	printf("%d:%s\n", quien, n==quien?"bien":"auch!");
  }
  return NULL;
}

int main()
{
  pthread_t t1, t2;
  int arg1=1, arg2=2;
  pthread_create(&t1, 0, f, &arg1);
  pthread_create(&t2, 0, f, &arg2);
  pthread_join(t1, NULL);
  pthread_join(t2, NULL);
  return 0;
}
