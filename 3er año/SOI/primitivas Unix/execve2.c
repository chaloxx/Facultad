#include <unistd.h>
#include <stdlib.h>



int main(){
	execve("func",NULL,NULL);
	exit(1);
	}
