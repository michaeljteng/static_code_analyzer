#include "hello.h"

// Globals
int y;

int public1(){
}

int foo(int a){
  bar(a, 0);
  return a++;
}

void bar(int a, int b){
  if(a > b){
    foo(a - b);
  }
  else if (a < b){
    foo(b-a);
  }
  else return;
}

void testScope(){
  int x;
  x = 1;

  if(x > 9){
    int x;
    x = 2;
  }
}

