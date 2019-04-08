/* N procesos */
int flag[N], turno[N];
int max;
/* proc: pid del proceso */
flag[proc]=1;
for(i=0; i<N; i++)
  while(flag[i] != 0);//Todos los procesos deben tener su bandera seteada
turno=++max;//Se incrementa el turno
for(i=0; i<N; i++)
  while(turno[proc]>turno[i] || (turno[proc]==turno[i] && proc>i));

RC;
