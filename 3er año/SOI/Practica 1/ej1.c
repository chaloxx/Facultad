#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <sys/wait.h>
#include <sys/types.h>

int main(){
	int pip[2];
	char comando[1024], comando2[1024];
	for(;;){
		printf("Primer comando:");
		gets(comando);
		comando[strlen(comando)] = 0;
		printf("Segundo comando:");
		gets(comando2);
		comando[strlen(comando2)] = 0;
		if(strcmp(comando2,"fin") == 0){
			break;
		pipe(pip);
		//Crear un hijo
		if(fork() == 0){
			close(1);close(pip[0]);
			dup(pip[1]);
			execl(comando,comando,NULL);//Ejecutar primer comando
			close(pip[1]);
		}
		wait(NULL); //El padre debe esperar por el primer hijo
		//Crear otro hijo
		if(fork() == 0){
			close(0);close(pip[1]);
			dup(pip[0]);
			execl(comando2,comando2,"ped",NULL);//Ejecutar segundo comando
			close(pip[0]);
		} 
		//Cerrar pipes del padre
		close(pip[0]);close(pip[1]);
		wait(NULL);
	}

}
