inc#lude <stdio.h>
#include <sys/socket.h>       
#include <sys/types.h>       
#include <arpa/inet.h>      
#include <unistd.h>        
#include <string.h>        
#include <time.h>
#include <errno.h>

#define BUFF_SIZE 1024
#define N 10000

int main()
{
    int sock[N], cto, i;
    struct sockaddr_in servaddr;
    struct sockaddr_in clientaddr;
    char buffer[BUFF_SIZE], buffer2[BUFF_SIZE];
    char ipaddr[1024];
    struct timespec tsp;
    
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family      = AF_INET;
    servaddr.sin_port        = htons(8000);
    inet_pton(AF_INET, "127.0.0.1", &servaddr.sin_addr);
    tsp.tv_sec=0;
    tsp.tv_nsec=10000000;
    
    for (i=0; i<N; i++) {
        int portnum = i%10000 + 10000;
        int ipaddrnum = i/10000+1;

        memset(&clientaddr, 0, sizeof(clientaddr));
        clientaddr.sin_family      = AF_INET;
        clientaddr.sin_port        = htons(portnum);
        sprintf(ipaddr, "127.0.0.%d", ipaddrnum);
        inet_pton(AF_INET, ipaddr, &clientaddr.sin_addr);

        printf("Conectando cliente %d...\n", i);
        if ( (sock[i] = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
            perror("ECHOCLNT: Error creating socket");
            return -1;
        }
        
        if ( bind(sock[i], (struct sockaddr *) &clientaddr, sizeof(clientaddr)) < 0 ) {
            perror("ECHOCLNT: Error calling bind()");
            return -1;
        }

        if (connect(sock[i], (struct sockaddr *)&servaddr, sizeof(servaddr)) < 0) {
            perror("ECHOCLNT: error connecting");
            return -1;
        }
        nanosleep(&tsp, NULL);
    }
    printf("%d clients connected\n",i);
    
    for (i=0; i<N; i++) {
        sprintf(buffer2, "Hola mundo %d", i);
        cto = strlen(buffer2);
        if (write(sock[i], buffer2, cto)<0) {
            perror("ECHOCLNT: Error writing");
            return -1;
        }
        
        if ((cto=read(sock[i], buffer, sizeof(buffer)-1))<0) {
            perror("ECHOCLNT: Error reading");
            return -1;
        }
        
        buffer[cto] = '\0';
        printf("%s\n",buffer);
        printf("A [%s] devuelve [%s]\n", buffer2, buffer);
    }

    printf("%d requests\n",i);
    for (i=0; i<N; i++)
        close(sock[i]);
    printf("%d clients disconnected\n",i);

    return 0;
}