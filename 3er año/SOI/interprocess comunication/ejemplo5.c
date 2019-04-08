/* server upd */

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

void error(char *s)
{
	exit((perror(s),-1));
}

int main()
{
	int sock;
	struct sockaddr_in sin;
	socklen_t l;
	char linea[1024];
	int cto;
	
	/* crea socket */
	if ((sock=socket (AF_INET, SOCK_DGRAM, 0)) < 0)
		error("socket");
	/* direccion */
	sin.sin_family= AF_INET;//Dominio de direcciones de internet
	sin.sin_port = htons(PORT);//Conversion
	sin.sin_addr.s_addr = INADDR_ANY;//IP del host
	/* asociar el socket a esta dirección */
	if (bind(sock,(sad)&sin, sizeof (sin)) < 0)
		error("bind");
	l = sizeof sin;
	if ((cto=recvfrom(sock, linea, sizeof(linea), 0, (sad)&sin, &l)) < 0)
		error ("recvfrom");
	linea[cto]=0;	
	printf ("de (%d, %s) llega [%s]\n", ntohs(sin.sin_port), inet_ntoa(sin.sin_addr), linea);
	linea[0]++;
	if (sendto(sock, linea, cto, 0, (sad)&sin, l) < 0)
		error("sendto");
	/* cerramos */
	close (sock);
	return 0;
}
