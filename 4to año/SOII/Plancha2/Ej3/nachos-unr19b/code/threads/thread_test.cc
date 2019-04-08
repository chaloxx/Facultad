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
#define MAX 10 /* segundos */


Port* p;

void prod(void *arg)
{
  sleep(random()%MAX);
  printf("El productor produce\n");
  p-> Send(1);
}

void cons(void *arg)
{
  int* i = new int;
  usleep(random()%MAX);
  p-> Receive(i);
  printf("El consumidor consume %d\n",*p);
}










/// Set up a ping-pong between several threads.
///
/// Do it by launching ten threads which call `SimpleThread`, and finally
/// calling `SimpleThread` ourselves.
void
ThreadTest()
{
    DEBUG('t', "Entering thread test\n");


    p = new Port();
    char *name = new char [64];
    strncpy(name, "2nd", 64);
    Thread *newThread = new Thread(name);
    newThread->Fork(prod, NULL);




    cons((void *) "1st");
}
