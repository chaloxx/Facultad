/* client udp */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>



#define PORT 5000 /*Port usados: /etc/services*/

typedef struct sockaddr *sad;

void error(char *s){
	exit((perror(s),-1));
	}


int main(int argc, char **argv){
	int sock;
	struct sockaddr_in sin;
	char linea[1024];
	int cto;
	
	/*crea el socket*/
	if( (sock = socket(PF_INET, SOCK_DGRAM,0)) < 0)
		error("socket");
	
	/*direccion*/
	sin.sin_family = AF_INET;
	sin.sin_port = htons(PORT);
	inet_aton(argv[1], &sin.sin_addr);
	
	/* mandamos y recibimos */
	if (sendto(sock, argv[2], strlen(argv[2]), 0, (sad)&sin, sizeof sin) < 0)
		error ("sendto");
	if ((cto=recvfrom(sock, linea, sizeof linea, 0, NULL, NULL)) < 0)
		error ("recvfrom");
	linea[cto] = 0;
	printf  ("[%s]\n", linea);
	/*cerramos */
	close (sock);
	return 0;
}
	
