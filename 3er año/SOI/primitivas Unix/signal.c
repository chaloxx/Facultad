
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
volatile int flag;



void handler(int sig){
	wait(NULL);
	flag = 1;
}


int main(){
	signal(SIGCHLD,handler);
	if(fork()){
		while(flag == 0);
	}
	else{
		sleep(1);
		printf(".");
	}
	return 0;
}

