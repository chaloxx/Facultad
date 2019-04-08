
/* A simple server in the internet domain using TCP
   The port number is passed as an argument */
#include <stdio.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
void error(char *msg)
{
    perror(msg);
    exit(1);
}

int main(int argc, char *argv[])
{
     int sockfd, newsockfd, portno, clilen;
     char buffer[256];
     struct sockaddr_in serv_addr, cli_addr;
     int n;
     if (argc < 2) {
         fprintf(stderr,"ERROR, no port provided\n");
         exit(1);
     }
     sockfd = socket(AF_INET, SOCK_STREAM, 0);
     if (sockfd < 0) 
        error("ERROR opening socket");
     bzero((char *) &serv_addr, sizeof(serv_addr));//Setea serv_addr a ceros
     portno = atoi(argv[1]);//Este es el puerto desde donde escucha el servidor
     serv_addr.sin_family = AF_INET;//Address family
     serv_addr.sin_addr.s_addr = INADDR_ANY;// Dirección IP del host
     printf("%d",INADDR_ANY);
     serv_addr.sin_port = htons(portno);//Convertir un número de puerto en orden de bytes de host a un número de puerto en orden de bytes de red.
     if (bind(sockfd, (struct sockaddr *) &serv_addr,
              sizeof(serv_addr)) < 0) //Enlaza el socket a una dirección
              error("ERROR on binding");
     listen(sockfd,5);//Escuchar desde el socket por conexiones, el segundo argumento es por la cantidad de conexiones que esperan mientras este proceso maneja una en particular
     clilen = sizeof(cli_addr);
     newsockfd = accept(sockfd, (struct sockaddr *) &cli_addr, &clilen);
     if (newsockfd < 0) 
          error("ERROR on accept");
     for(;;){
	     bzero(buffer,256);//Setear buffer a ceros
	     n = read(newsockfd,buffer,255);//Leer desde el descriptor a buffer
	     if (n < 0) error("ERROR reading from socket");
	     printf("Cliente: %s",buffer);
	     printf("Respuesta:");
	     bzero(buffer,256);
	     fgets(buffer,255,stdin);
	     n = write(newsockfd,buffer,strlen(buffer));
	     if (n < 0) error("ERROR writing to socket");
	}
     return 0; 
}
