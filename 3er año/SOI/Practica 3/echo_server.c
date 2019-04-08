#include <stdio.h>
#include <stdlib.h>

#include <sys/socket.h>       
#include <sys/types.h>       
#include <arpa/inet.h>      
#include <semaphore.h>      
#include <pthread.h>      
#include <errno.h>      
#include <unistd.h>        
#include <string.h>        

#define BUFF_SIZE 1024

int id=-1;

pthread_mutex_t RC = PTHREAD_MUTEX_INITIALIZER;

sem_t sync_copy_of_arg_handle_client;

void *handle_client(void *arg)
{
    pthread_detach(pthread_self());
    int conn_s = *((int*)arg);
    sem_post(&sync_copy_of_arg_handle_client);
    char buffer[BUFF_SIZE],buffer2[BUFF_SIZE];
    int res;
    pthread_mutex_lock(&RC);
    id++; // Nuevo cliente
    fflush(stdout);
    pthread_mutex_unlock(&RC);
    fprintf(stderr,"New client %d connected\n",id);
    while(1) {
        res=read(conn_s,buffer,BUFF_SIZE);
        for(int k = 0 ; k < res ; k++)
          buffer[k]++;
        if (res<=0) {
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
    sem_init(&sync_copy_of_arg_handle_client, 0, 0);
    int list_s,conn_s=-1,res;
    struct sockaddr_in servaddr;
    char buffer[BUFF_SIZE],buffer2[BUFF_SIZE];
    if ( (list_s = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
        fprintf(stderr, "ECHOSERV: Error creating listening socket.\n%s\n",strerror(errno));
        return -1;
    }
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family      = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port        = htons(8000);

    if ( bind(list_s, (struct sockaddr *) &servaddr, sizeof(servaddr)) < 0 ) {
        fprintf(stderr, "ECHOSERV: Error calling bind()\n%s\n",strerror(errno));
        return -1; 
    }

    if ( listen(list_s, 10) < 0 ) {
        fprintf(stderr, "ECHOSERV: Error calling listen()\n%s\n",strerror(errno));
        return -1;                          
    }

    while (1) {
        if ( (conn_s = accept(list_s, NULL, NULL) ) < 0 ) {
            fprintf(stderr, "ECHOSERV: Error calling accept()\n%s\n",strerror(errno));
            return -1;
        }
        pthread_t handle_client_thread;
        int error;
        if(error = pthread_create(&handle_client_thread, NULL, handle_client, &conn_s)){
          fprintf(stderr, "Error (%d) lanzando el thread para manejar una conexion de un cliente al servidor.\n",error);
          exit(1);
          }
        sem_wait(&sync_copy_of_arg_handle_client);
    }
    return 0;
}
