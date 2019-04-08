#include <semaphore.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#define DELAY(a, b) sleep(a + rand() % (b-a+1))
#define SILLAS 4

sem_t sillas, corte, terminocortando, terminome_cortan;

pthread_mutex_t salaespera = PTHREAD_MUTEX_INITIALIZER;

pthread_cond_t alarma = PTHREAD_COND_INITIALIZER;

void error(char *errortext){
  puts(errortext);
  exit(1);
}

void me_cortan(){
  puts("[CLIENTE] Me estan cortando el pelo");
  DELAY(2, 6);
}

void cortando(){
  puts("[BARBERO] Estoy cortando el pelo");
  DELAY(2, 6);
}

void pagando(){
  puts("[CLIENTE] Estoy pagando -$");
  DELAY(1, 3);
}

void me_pagan(){
  puts("[BARBERO] Me estan pagando $$$");
  DELAY(1, 3);
}

void dormir(){
  puts("[BARBERO] Me fui a dormir ~ZZzzZzzzzZZ~");
}

void despertar(){
  puts("[BARBERO] Me desperte! Alta siesta pegue");
}

void ocupar(){
  pthread_mutex_lock(&salaespera);
  if(sem_trywait(&sillas)){ // no se pudo decrementar semaforo
    if(errno == EAGAIN){ // no hay sillas disponibles
      puts("[SALA DE ESPERA] Cliente se fue, la barberia esta llena!");
      pthread_mutex_unlock(&salaespera);
      pthread_exit(NULL); // termino el thread
    } else error("Problema con semaforo sillas");
  } else { // Habia, al menos, una silla disponible.
    int sillasdisponibles;
    sem_getvalue(&sillas, &sillasdisponibles);
    if(sillasdisponibles == SILLAS-1) //La barberia estaba vacia, el barbero duerme
      if(pthread_cond_signal(&alarma)) error("error signaling alarma"); //Despierto barbero
    printf("[SALA DE ESPERA] Se sento cliente. %d sillas libres\n", sillasdisponibles);
    pthread_mutex_unlock(&salaespera);
  }
}

void desocupar(){
  pthread_mutex_lock(&salaespera);
  if(sem_post(&sillas)) error("error posting semaforo sillas"); // libero silla de espera
  pthread_mutex_unlock(&salaespera);
}

void intentardormir(){
  pthread_mutex_lock(&salaespera);
  int sillasdisponibles;
  sem_getvalue(&sillas, &sillasdisponibles);
  if(sillasdisponibles == SILLAS){ //La barberia esta vacia
    dormir();
    if(pthread_cond_wait(&alarma, &salaespera)) error("error signaling alarma"); //Despierto barbero
    despertar();
    }
  pthread_mutex_unlock(&salaespera);
}

void *barbero(void *arg){
  for(;;){
  intentardormir();
  if(sem_post(&corte)) error("error posting semaforo corte"); // le corto a 1
  cortando();
  if(sem_post(&terminocortando)) error("error posting semaforo terminocortando"); // aviso termino cortando()
  if(sem_wait(&terminome_cortan)) error("error waiting semaforo terminome_cortan"); // espero termine me_cortan()
  me_pagan();
  }
}

void *cliente(void *arg){
  if(pthread_detach(pthread_self())) error("detaching client thread"); // para no tener que hacerle join
  ocupar();
  if(sem_wait(&corte)) error("error waiting semaforo corte");
  desocupar();
  me_cortan();
  sem_post(&terminome_cortan);
  sem_wait(&terminocortando);
  pagando();
}

void *lanzadorClientes(void *arg){
  for(;;){
  DELAY(4, 10);
  pthread_t client;
  pthread_create(&client, NULL, cliente, NULL);
  }
}

int main(){
  pthread_t barb, lanzaClientes;
  
  sem_init(&sillas, 0, SILLAS);
  sem_init(&corte, 0, 0);
  sem_init(&terminocortando, 0, 0);
  sem_init(&terminome_cortan, 0, 0);
  
  pthread_create(&barb, NULL, barbero, NULL);
  pthread_create(&lanzaClientes, NULL, lanzadorClientes, NULL);
  
  pthread_join(barb, NULL);

  return 0;
}
