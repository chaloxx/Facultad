#include <stdio.h>
#include <sys/socket.h>       
#include <sys/types.h>       
#include <arpa/inet.h>      
#include <unistd.h>        
#include <string.h>        
#include <pthread.h>
#define BUFF_SIZE 1024
#define MAXCLIENT 10000

int nroClient;
pthread_mutex_t bloq;
pthread_t t[MAXCLIENT];



void* handle_client(void* arg)
{
    
    pthread_mutex_lock(&bloq);
    int id = nroClient; // Nuevo cliente
    nroClient++;
    pthread_mutex_unlock(&bloq);
    char buffer[BUFF_SIZE],buffer2[BUFF_SIZE];
    int res,conn_s = *(int*)arg;
    fprintf(stderr,"New client %d connected\n",id);
    while(1) {
        res=read(conn_s,buffer,BUFF_SIZE);
        printf("%s\n",buffer); 
        if (res<=0) {
            printf("Client %d disconnected \n",id );
            close(conn_s); 
            break;
        }
        buffer[res]='\0';
        sprintf(buffer2,"Response to client %d: %s",id,buffer);
        write(conn_s,buffer2,strlen(buffer2));
    }
    return NULL;
}

int main()
{
    int list_s,conn_s,res, arg[MAXCLIENT];
    struct sockaddr_in servaddr;
    char buffer[BUFF_SIZE],buffer2[BUFF_SIZE];
    if ( (list_s = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
        fprintf(stderr, "ECHOSERV: Error creating listening socket.\n");
        return -1;
    }
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family      = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port        = htons(8000);

    if ( bind(list_s, (struct sockaddr *) &servaddr, sizeof(servaddr)) < 0 ) {
        fprintf(stderr, "ECHOSERV: Error calling bind()\n");
        return -1; 
    }

    if ( listen(list_s, 10) < 0 ) {
        fprintf(stderr, "ECHOSERV: Error calling listen()\n");
        return -1;                          
    }

    while (nroClient < MAXCLIENT) {
        if ((conn_s = accept(list_s, NULL, NULL) ) < 0 ) {
            fprintf(stderr, "ECHOSERV: Error calling accept()\n");
            return -1;
        }
        
        arg[nroClient] = conn_s;
        pthread_create(&t[nroClient],NULL,handle_client,&arg[nroClient]);
    }
    pthread_join(t,NULL);
    return 0;
}