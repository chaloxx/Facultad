/* server tcp */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>



#define PORT 5000 /*Port usados: /etc/services*/

typedef struct sockaddr *sad;

void error(char *s){
	exit((perror(s),-1));
	}



int main(){
	int sock, sock1;
	struct  sockaddr_in sin, sin1;
	char linea[1024];
	int cto;
	socklen_t l;
	/*creamos el socket de conexión*/
	if((sock=socket( PF_INET, SOCK_STREAM,0)) < 0)
		error("socket");
	/*dirección */
	sin.sin_family = AF_INET;
	sin.sin_port = htons(PORT);
	sin.sin_addr.s_addr = INADDR_ANY;
	/*Asignamos la dirección al socket*/
	if(bind(sock,(sad)&sin, sizeof(sin)) < 0)
		error("bind");
	/*avisaremos que escucharemos con socket*/
	if( listen(sock,5) < 0) error("listen");
	/*Esperamos conexion*/
	for (;;){
		l = sizeof(sin1);
		if( (sock1 = accept(sock, (sad)&sin1, &l)) < 0)
			error("accept");
		printf("De (%s, %d)\n", inet_ntoa(sin1.sin_addr), ntohs(sin1.sin_port));
		/*Tratamos la sesión*/
		if( (cto = read(sock1, linea, sizeof(linea))) < 0)
			error("read");
		linea[cto] = 0;
		printf("[%s]\n", linea);
		linea[0]++;
		if (( write(sock1,linea,cto) < 0))
			error("write");
	}
	/* cerramos sesion */
	close (sock1);
	/*Cerramos conexión*/
	close(sock);
	return 0;
	}
