#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>



int main(){
	if(fork() != 0){
		printf("Yo soy tu padre y mi pid es %d!\n",getpid());
		int status = wait(&status);
		printf("Mi hijo salio con %d",status);
		}
	else{
		printf("Yo soy tu hijo y mi pid es %d!\n",getpid());
		}
	exit(1);
	}
