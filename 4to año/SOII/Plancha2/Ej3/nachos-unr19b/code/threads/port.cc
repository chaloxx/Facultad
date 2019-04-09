#include "port.hh"
#include "synch_list.hh"

Port::Port(){
 lSend = new Lock(NULL);
 lReceive = new Lock(NULL);
 llegoMensaje = new Semaphore(NULL,0);
 tomoMensaje = new Semaphore(NULL,0);

}

Port::~Port(){
  delete lSend;
  delete lReceive;
  delete llegoMensaje;
  delete tomoMensaje;
}



void Port::Send(int message){
 DEBUG('p',"Hilo %s esta esperando para enviar %d \n",currentThread-> GetName(),message);
 lSend -> Acquire(); // Proteger el recurso compartido msg
 msg = message;
 DEBUG('p',"Hilo %s envio %d \n",currentThread-> GetName(),message);
 llegoMensaje -> V();// Avisa que llego un mensaje
 tomoMensaje -> P(); // Espera que se tome el mensaje
 lSend -> Release(); // Liberar recurso
 DEBUG('p',"Hilo %s se va \n",currentThread-> GetName());
 currentThread-> Yield();
}

void Port::Receive(int* message){
 lReceive -> Acquire(); // Evitar que un thread se adelante a levantar el semaforo
 DEBUG('p',"Hilo %s esperando mensaje \n",currentThread-> GetName());
 llegoMensaje -> P(); // Espera la llegada de algun mensaje
 *message = msg; // Toma el mensaje
 DEBUG('p',"Hilo %s recibio %d \n",currentThread-> GetName(),*message);
 tomoMensaje -> V(); // Avisa que el mensaje fue tomado
 lReceive -> Release();
 DEBUG('p',"Hilo %s se va \n",currentThread-> GetName());
 currentThread-> Yield();
}
