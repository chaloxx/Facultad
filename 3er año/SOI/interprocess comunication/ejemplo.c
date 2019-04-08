#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>



int main(){
	int pip[2];
	pipe(pip);
	if(fork()){
		write(pip[1], "hola mundo\n", 11);
		close(pip[0]); close(pip[1]);
		wait(NULL);
		}
	else{
		/*char linea[1024];
		int cto = read(pip[0], linea, 1024);
		linea[cto] = 0;
		printf("%s",linea);
		close(pip[0]); close(pip[1]);*/
		char c;
		close(pip[1]);
		while(read(pip[0], &c,1) == 1)
			putchar(c);
		close(pip[0]);
		}
	return 0;
	}








