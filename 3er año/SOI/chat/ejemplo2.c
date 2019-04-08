/* compilar con
 * gcc ....c -lpthread
 *
 * sale
 * hola mundo! 1
 * hola mundo! 2
 * o
 * hola mundo! 2
 * hola mundo! 1
 */

#include <stdio.h>
#include <pthread.h>

void *f(void *arg)
{
  int quien=*(int*)arg;
  printf("hola mundo!%d\n", quien);
  return NULL;
}

int main()
{
  pthread_t t1, t2;
  int arg1, arg2;
  arg1=1; arg2=2;
  pthread_create(&t1,0,f,&arg1);
  pthread_create(&t2,0,f,&arg2);
  pthread_join(t1,NULL);
  pthread_join(t2,NULL);
  return 0;
}


