/* chat */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/select.h>
#include <time.h>

typedef struct sockaddr *sad;

void error(char *s)
{
  exit((perror(s), -1));
}

#define MAX(x,y) ((x)>(y)?(x):(y))

int main(int argc, char **argv)
{
  int sock, maxm1, cto;
  char linea[1024];
  struct sockaddr_in sin;
  socklen_t l;
  fd_set in, in_orig;
  if(argc != 2) {
	fprintf(stderr, "Uso: %s port\n", argv[0]);
	exit(-1);
  }
  if((sock=socket(PF_INET, SOCK_DGRAM, 0)) < 0)
	error("socket");
  sin.sin_family=AF_INET;
  sin.sin_port=htons(atoi(argv[1]));
  sin.sin_addr.s_addr=INADDR_ANY;
  if(bind(sock,(sad)&sin,sizeof sin) < 0)
	  error("bind");
  FD_ZERO(&in_orig);
  FD_SET(0, &in_orig);FD_SET(sock, &in_orig);
  maxm1=MAX(0, sock)+1;
  for(;;){ /* ciclo vital */
	printf("yes, master..."); fflush(stdout);
	memcpy(&in, &in_orig, sizeof in);
	if(select(maxm1, &in, NULL, NULL, NULL)<0)
	  error("select");
	if(FD_ISSET(0, &in)) {
	  char *p;
	  fgets(linea, sizeof linea, stdin);
	  linea[strlen(linea)-1] = 0;
	  /*suponemos que tiene la forma
	   * port:ipaddr:mensaje */
	  p=strtok(linea, ":");
	  sin.sin_family=AF_INET;
	  sin.sin_port=htons(atoi(p));
	  p=strtok(NULL, ":");
	  inet_aton(p, &sin.sin_addr);
	  p=strtok(NULL, ":");
	  if(sendto(sock, p, strlen(p), 0, (sad)&sin, sizeof sin)<0)
		error("sendto");
	}
	if(FD_ISSET(sock, &in)) {
	  l=sizeof sin;
	  if((cto=recvfrom(sock, linea, sizeof linea, 0, (sad)&sin, &l))<0)
		error("recvfrom");
	  linea[cto]=0;
	  printf("(%d, %s):[%s]\n", ntohs(sin.sin_port), inet_ntoa(sin.sin_addr), linea);
	}
  }
  return 0;
}
