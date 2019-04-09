/// Simple test case for the threads assignment.
///
/// Create several threads, and have them context switch back and forth
/// between themselves by calling `Thread::Yield`, to illustrate the inner
/// workings of the thread system.
///
/// Copyright (c) 1992-1993 The Regents of the University of California.
///               2007-2009 Universidad de Las Palmas de Gran Canaria.
///               2016-2017 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "system.hh"
#include "synch.hh"
#include <unistd.h>
#include "port.hh"

#define N 10
#define MAX 2 /* segundos */
#define NUM_THREADS 20


Port* p;

void prod(void *arg)
{
  int i = *(int*)arg;
  sleep(random()%MAX);
  p-> Send(i);
}

void cons(void *arg)
{
  int* i = new int;
  usleep(random()%MAX);
  p-> Receive(i);
}


/// Set up a ping-pong between several threads.
///
/// Do it by launching ten threads which call `SimpleThread`, and finally
/// calling `SimpleThread` ourselves.
void ThreadTest(){
  DEBUG('t', "Entering thread test\n");
  p = new Port();
  char * names[NUM_THREADS];
  Thread *threads[NUM_THREADS];
  char v[10];
  int *mjes = new int[NUM_THREADS];

  for(int i = 0;i < NUM_THREADS;i++){
     mjes[i] = i;
  }

  for (int i = 0 ; i < NUM_THREADS; i++){
    sprintf(v,"hilo::%d",i);
    names[i] = new char [64];
    strcpy(names[i],v);
    threads[i] = new Thread(names[i]);
    if (i%2 == 0){
      threads[i]->Fork(prod,(void *) (mjes+i));
    }
    else{
      threads[i]->Fork(cons,(void *) names[i]);
    }
  }


 //sleep(MAX);
}
