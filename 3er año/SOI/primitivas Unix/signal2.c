
#include <stdio.h>
#include <signal.h>

void handler(int sig){
	static int clas;
	if(clas < 3){
	printf("Osooooo!\n");
	clas++;
	}
	else{
	signal(SIGINT,SIG_DFL);
	}
}


int main(){
	signal(SIGINT, handler);
	for(;;);


}

