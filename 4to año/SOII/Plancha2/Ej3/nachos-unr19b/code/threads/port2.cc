#include "port.hh"
#include "synch_list.hh"

Port::Port(){
 queu = new SynchList<int>; // Crear mailbox
 l1 = new Lock(NULL);
 l2 = new Lock(NULL);
 s = new Semaphore(NULL,0);// Semáforo para los threads que llaman a Send

}

Port::~Port(){
  delete queu;
  delete s;
  delete l1;
  delete l2;
}



void Port::Send(int message){
 l1 -> Acquire();
 DEBUG('s',"Hilo %s intentando enviar %d \n",currentThread-> GetName(), message);
 queu-> Append(message); // Agregar contenido al mailbox
 s -> P();// Duerme hasta que termine Receive
 l1 -> Release();
}

void Port::Receive(int* message){
 l2 -> Acquire();
 DEBUG('s',"Hilo %s intentando recibir \n",currentThread-> GetName(), message);
*message = queu-> Pop(); // Se bloquea hasta que llega algo, equivale a decir que se bloquea hasta que se llame a Send
 s -> V(); // Despierta
 l2 -> Release();
}
