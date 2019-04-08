/*#include <string.h>
#include <stdio.h>


main (){
 char val[5] = "hola"; //to_string(3);
 printf("%s \n", val );

}*/



#include <stdio.h>

int main()
{
    int v = 43;
    char number[5];
    sprintf(number,"%d",v);
    printf("%s\n",number);
}
