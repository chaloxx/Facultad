#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>




char *arr[] = {
	"esta",
	"es",
	"una",
	"puta",
	"prueba",
	"MAS",
	"para",
	"ordenar",
	"jejeje",
	"jojojo",
	NULL,
	};


int main(){
	int ph[2], hp[2];
	pipe(ph);
	pipe(hp);
	if(fork()){
		int i; char c;
		close(ph[0]);//Cierra la lectura
		close(hp[1]);//Cierra la escritura
		for(i = 0; arr[i]; i++){
			write(ph[1], arr[i], strlen(arr[i]));
			write(ph[1], "\n", 1);
			}
		close(ph[1]);// Cierra la escritura
		while(read(hp[0], &c,1) == 1)
			putchar(c);
		close(hp[0]);
		wait(NULL);
		}
	else{
		close(ph[1]); close(hp[0]);
		close(0); dup(ph[0]); close(ph[0]);
		close(1); dup(hp[1]); close(hp[1]);
		execl("/usr/bin/sort", "sort", NULL);
		perror("execl");
		exit(-1);
		}
	return 0;
	}






