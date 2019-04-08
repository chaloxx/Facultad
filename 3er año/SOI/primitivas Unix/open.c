#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int main(){
	char words[] = "perro";
	char words2[] = "gato";
	printf("%d",sizeof(words));
	int arch,i;
	arch = open("newtxt2.txt",O_WRONLY|O_CREAT,S_IRWXU);
	lseek(arch,0,SEEK_END);
	write(arch, words2, sizeof(words2)-1);
	write(arch, words, sizeof(words)-1);
	/*Leerlo al revés*/
	/*for(i = 0; i < 3; i++){
		read(arch,words2[i],sizeof(words[i]));
		printf("%s\n",words2[i]);
	}*/
	close(arch);
}
