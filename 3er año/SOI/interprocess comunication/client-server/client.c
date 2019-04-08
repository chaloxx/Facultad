
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h> 
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
void error(char *msg)
{
    perror(msg);
    exit(0);
}

int main(int argc, char *argv[])
{
    int sockfd, portno, n;

    struct sockaddr_in serv_addr;
    struct hostent *server;

    char buffer[256];
    if (argc < 3) {
       fprintf(stderr,"usage %s hostname port\n", argv[0]);
       exit(0);
    }
    portno = atoi(argv[2]);
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) 
        error("ERROR opening socket");
    server = gethostbyname(argv[1]);//Obtiene información acerca del host
    if (server == NULL) {
        fprintf(stderr,"ERROR, no such host\n");
        exit(0);
    }
    bzero((char *) &serv_addr, sizeof(serv_addr));//Setear serv_addr a ceros
    serv_addr.sin_family = AF_INET;//Address family
    bcopy((char *)server->h_addr, 
         (char *)&serv_addr.sin_addr.s_addr,
         server->h_length);//Copiar h_addr  en s_addr
    serv_addr.sin_port = htons(portno);//Numero de puerto
    if (connect(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr)) < 0) 
        error("ERROR connecting");//Conectar a la dirección del host
    printf("Please enter the message: ");
    for(;;){
	    bzero(buffer,256);
	    fgets(buffer,255,stdin);
	    n = write(sockfd,buffer,strlen(buffer));//Escribir de buffer
	    if (n < 0) 
        	 error("ERROR writing to socket");
	    bzero(buffer,256);//Limpiar buffer
	    n = read(sockfd,buffer,255);
	    if (n < 0) 
	         error("ERROR reading from socket");
	    printf("Servidor: %s",buffer);
	    printf("Respuesta:");
     }
    return 0;
}
