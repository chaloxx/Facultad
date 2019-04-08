#include "synch.hh"

void Port::Port(){
 queu = new SynchList<Int>; // Crear mailbox
 s = new Semaphore(NULL,0);// Semáforo para los threads que llaman a Send
}

void Port::~Port{
  delete queu;
}



void Port::Send(int message){
 queu-> Append(message); // Agregar contenido al mailbox
 s -> V(); // Espera hasta que termine un Receive

}

void Port::Receive(int* message){
*message = queu-> Pop(); // Se bloquea hasta que llega algo, equivale a decir que se bloquea hasta que se llame a Send
s -> P(); // Deja pasar a un thread que haya llamado
}
