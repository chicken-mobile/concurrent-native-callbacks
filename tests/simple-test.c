/* simple test for CNCB
 */


#include <stdio.h>
#include <pthread.h>
#include <chicken.h>


pthread_t chicken_thread;


extern void foo(int x);	/* callback */
extern int bar(int t, int x);	/* callback */
extern void voidresult(int t);	/* callback */


static void *
start_chicken(void *arg)
{
  CHICKEN_run(C_toplevel);
  printf("chicken returned.\n");
  return NULL;
}


static void *
start1(void *arg)
{
  foo(42);
  printf("foo done.\n");
  return NULL;
}


static void *
start2(void *arg)
{
  int r = bar(0, 43);
  printf("bar done: %d\n", r);
  voidresult(0);
  return NULL;
}


int
main(int argc, char *argv[])
{
  pthread_t t1, t2;
  pthread_addr_t a;

  pthread_attr_init(&a);
  pthread_attr_setstacksize(&a, 4000000);
  pthread_create(&chicken_thread, &a, start_chicken, NULL);
  sleep(2);			/* give it some time to get running */
  printf("creating threads ...\n");
  pthread_create(&t1, NULL, start1, NULL);
  sleep(1);
  pthread_create(&t2, NULL, start2, NULL);
  pthread_join(t1, NULL);
  pthread_join(t2, NULL);
  sleep(3);
  printf("done.\n");
  return 0;
}
