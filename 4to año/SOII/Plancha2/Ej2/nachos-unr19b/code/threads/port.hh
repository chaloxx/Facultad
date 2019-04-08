#include "synch_list.hh"
#include "system.hh"


//La clase Port simula comunicación entre procesos


class Port {
  public:
  // Inicializar Port
  Port();

  // Destruir Port
  ~Port ();

  // Enviar mensaje (bloqueante hasta que otro thread haga Receive)
  void Send (int);

  // Recive mensaje (bloqueante hasta que otro thread haga Send)
  void Receive (int*);

  private :
  int msg;
  Lock *lSend,*lReceive;
  Semaphore *llegoMensaje,*tomoMensaje;

};
