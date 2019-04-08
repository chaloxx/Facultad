#include <stdio.h>
#include <sys/time.h>
#include <sys/resource.h>

void printRlimit(const char *msg, int resource){
   struct rlimit rlim;
   getrlimit(resource, &rlim);
   printf("\n%s ", msg);
   printf("soft=");

   if (rlim.rlim_cur == RLIM_INFINITY)
       printf("infinite");
   else if (rlim.rlim_cur == RLIM_SAVED_CUR)
       printf("unrepresentable");
   else
       printf("%lld", (long long) rlim.rlim_cur);

   printf(" hard=");
   if (rlim.rlim_max == RLIM_INFINITY)
     printf("infinite\n");
   else if (rlim.rlim_max == RLIM_SAVED_MAX)
       printf("unrepresentable");
   else
       printf("%lld\n", (long long) rlim.rlim_max);
}

int main(){
   printRlimit("RLIMIT_AS", RLIMIT_STACK);
   printRlimit("RLIMIT_CORE", RLIMIT_CORE);
   printRlimit("RLIMIT_CPU", RLIMIT_CPU);
   printRlimit("RLIMIT_DATA", RLIMIT_DATA);
   printRlimit("RLIMIT_FSIZE", RLIMIT_FSIZE);
   printRlimit("RLIMIT_MEMLOCK", RLIMIT_MEMLOCK);
   printRlimit("RLIMIT_MSGQUEUE", RLIMIT_MSGQUEUE);
   printRlimit("RLIMIT_NPROC", RLIMIT_NPROC);
   printRlimit("RLIMIT_NICE", RLIMIT_NICE);
   printRlimit("RLIMIT_NOFILE", RLIMIT_NOFILE);
   printRlimit("RLIMIT_RSS", RLIMIT_RSS);
   printRlimit("RLIMIT_RTPRIO", RLIMIT_RTPRIO);
   printRlimit("RLIMIT_RTTIME", RLIMIT_RTTIME);
   printRlimit("RLIMIT_SIGPENDING", RLIMIT_SIGPENDING);
   printRlimit("RLIMIT_STACK", RLIMIT_STACK);
   return 0;
}