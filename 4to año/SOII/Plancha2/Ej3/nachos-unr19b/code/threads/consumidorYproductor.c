 #include "synch.hh"
#define MAX_BUFFER 1024 /* tamanio del buffer */
#define DATOS_A_PRODUCIR 100000 /* datos que se desean producir */
int n_elementos = 0; /* numero de elementos en el buffer */
int buffer[MAX_BUFFER]; /* buffer comun */

Lock* l;
Condition* no_lleno;
Condition* no_vacio;




void Productor(void*) { /* codigo del productor */
 int dato,i ,pos = 0;
 for(i=0; i < DATOS_A_PRODUCIR; i++ ) {
  dato = i; /* producir dato */
  //l->Acquire(); /* acceder al buffer */
  while (n_elementos == MAX_BUFFER){ /* si buffer lleno */
   //no_lleno->Wait(); /* se bloquea */
  }
  buffer[pos] = i;
  pos = (pos + 1) % MAX_BUFFER;/*calculamos la posicion del siguiente elemento en el buffer*/
  n_elementos ++;
  no_vacio->Signal(); /* si hemos creado un dato el buffer no estará vacio*/
  l->Release();/*liberamos el mutex*/
 }
}

void Consumidor(void*){
/* codigo del sonsumidor */

 int dato, i ,pos = 0;
 for(i=0; i < DATOS_A_PRODUCIR; i++ ) {
  l->Acquire(); /* acceder al buffer */
  while (n_elementos == 0){ /* si buffer vacio */
   no_vacio->Wait(); /* se bloquea */
  }
  dato = buffer[pos];
  pos = (pos + 1) % MAX_BUFFER;/*calculamos la posicion del siguiente elemento en el buffer*/
  n_elementos --;
  no_lleno->Signal(); /* buffer no lleno */
  l->Release();
  printf("Consume %d \n", dato); /* consume dato */
 }
}
