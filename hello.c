#include "hello.h"

typedef struct _fooStruct{
  int field1;
  int field2;
} fooStruct;
typedef void (*fn)(void);
typedef enum {START, STOP} running;
enum {ONE, TWO} numbers;

static int global1;
static int global2;
static int global3;
running state = START;


void hello_init(){
  global1 = 0;
  //global3 = 0;
  global2 = 0;
  numbers = TWO;
  state = START;
}

void fn1(void){
}

int main(int argc, char *argv[]){
  int local1, local2;
  char localchar;
  short localshort;
  fn indirectcall = &fn1;

  hello_init();

  global1 = global1 + 1;
  (*indirectcall)();

  printf("Hello world!");

  bar(1+42, 3);
  local1 = 84 + 42;

  local1 = foo(112);

  foo(state);

  bar(foo(11), 333); 

  while(local1 < 123){
    local1++;
  }

  return 0;
}
