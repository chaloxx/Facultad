#include <stdio.h>
#include <unistd.h>

int main(){
	printf("Soy el proceso padre y mi id es %d\n",getpid());
	if(fork() == 0){
	sleep(2);
	printf("Soy un pobre huerfano y fui adoptado por %d",getppid());
	return 0;
	}
	return 0;
}

