#include <stdlib.h>


typedef int dni ;
typedef int dinero;
typedef struct cta{
       dni id;
       dinero monto;
  } Cta;
  
typedef Cta *Banco;
   
int cantCtas(Banco *b){
  return (sizeof (*b) / sizeof (Cta));
  }



int NuevoClienteOK (dni d, Banco *b){
  int ultimo =  cantCtas (b);
  (*b)[ultimo].id = d;
  (*b)[ultimo].monto = 0;  
  return 1;
  }

int ClienteExiste(dni id2, Banco *b){
  int cant = cantCtas (b);
  for(int i = 0; i <= cant ;i++){
    if ((*b)[i].id == id2){
      return 1;
      }
    }
  return 0;
  }

// Retorna 0 si el cliente existe y 1 si lo agrega
int NuevoCliente (dni d,Banco *b){
  if (ClienteExiste (d,b)) return 0;
  NuevoClienteOK (d,b);
  }

int main(){
  return 0;
  }
