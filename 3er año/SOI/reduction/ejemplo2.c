
#include <stdio.h>
#include <omp.h>


int main(){
	int i;
	
	
	#pragma omp parallel for
	
	for(i = 0; i< 100; i++)
		printf("i = %d, thr = %d\n", i, omp_get_thread_num());
	
	
	
	
	}
