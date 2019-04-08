#include <stdio.h>
#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>

#define N_VISITANTES 1000000
#define N 2 //Cantidad de procesos
#define max(a,b) (a > b ? a : b)
int flag[N], turno[N];
int visitantes = 0;



void unlock(int proc){
	turno[proc]=0;	
	}

void lock(int proc){
	flag[proc] = 1;//Avisar que se quiere un turno
    turno[proc] = 1+max(turno[0],turno[1]);//Tomar un turno
    flag[proc] = 0;
    for(int j=0; j<N; j++){
      while(flag[j]);//Esperar si se esta pidiendo un turno
      while(turno[j] && turno[proc]>turno[j] || (turno[proc]==turno[j] && proc>j));
	}
	
}





void *molinete(void *arg)
{
  int i, proc = omp_get_thread_num() % N;
  for (i=0;i<N_VISITANTES;i++)
  {
      lock(proc);     
      fflush(stdout);
      visitantes++;//RC      
      unlock(proc);
  }
}

int main()
{
  #pragma omp parallel sections num_threads(2)
  {
  #pragma omp section
	  {
		molinete(NULL);
	  }
  #pragma omp section
	  {
	   
		molinete(NULL);  
	  }
  }
  printf("Hoy hubo %d visitantes!\n", visitantes);    
  return 0;
} 
