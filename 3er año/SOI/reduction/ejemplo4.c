

#include <stdio.h>
#include <omp.h>


int main(){
	int i;
	
	
	#pragma omp parallel
	#pragma omp sections
	{
		#pragma omp section
		printf("hola mundo %d \n",omp_get_thread_num());
		#pragma omp section
		printf("chau mundo %d \n",omp_get_thread_num());
		#pragma omp section
		printf("daba daba q??? %d \n",omp_get_thread_num());
	
	
	}
	return 0;
	
}
