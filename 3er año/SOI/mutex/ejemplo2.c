#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

int n;
pthread_mutex_t s=PTHREAD_MUTEX_INITIALIZER;

void *f(void *arg)
{
  int quien=*(int*)arg;
  for(;;){
	pthread_mutex_lock(&s);
	n=quien;
	sleep(1);
	printf("%d:%s\n", quien, n==quien?"bien":"auch!");
	pthread_mutex_unlock(&s);
	sleep(1);
  }
  return 0;
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
