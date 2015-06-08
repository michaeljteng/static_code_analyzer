/*
  Sequence of calls:

  A() // two messages in bQ using queue_send in bQ

  B_receive() // receives first message, one message left in bQ
    fun1() called by B_receive
      queue_send in fun1 results in message in aQ 
  
  A_receive() // picks up message in aQ
              // reply is null

  B_receive() // receivese second message, no messages left
    


 */


typedef int token;
typedef int queue;

typedef void (*someFunc)(void);

typedef struct _reply {
  someFunc fptr;
} reply;

#define NULL 0

queue aQ;
queue bQ;

token tokens[] = {0, 1, 2, 3};

void queue_send(queue q, token t, reply* r){
}

void send_reply(reply* r){
  // if(r == NULL) return;
  if(r != NULL){
    (r->fptr)();
  }
}

/*reply* make_reply2(someFunc f){
  reply* r;
  r = (reply*)malloc(sizeof(reply));
  r->fptr = f;
  return r;
  }*/

void queue_receive(queue q){

}

void A_receive(){
  queue_receive(aQ);
}

void B_receive(){
  queue_receive(bQ);
}

void fun1(void){
  queue_send(aQ, tokens[3], NULL); 
}

void fun2(void){
  queue_send(aQ, tokens[0], NULL); 
}

reply make_reply(someFunc f){
  reply r;
  r.fptr = f;
  return r;
}

void A(){
  reply r;
  r = make_reply(fun1);
  queue_send(bQ, tokens[1], &r);
  queue_send(bQ, 42, NULL);
}

