/* simple test for CNCB
 */


#include <stdio.h>
#include <pthread.h>
#include <chicken.h>


pthread_t chicken_thread;


extern void foo(int x);	/* callback */
extern void bar(int x, int *r);	/* callback */


static void *
start_chicken(void *arg)
{
  CHICKEN_run(C_toplevel);
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
  int r;

  bar(42, &r);
  printf("bar done: %d\n", r);
  return NULL;
}


int
main(int argc, char *argv[])
{
  pthread_t t1, t2;

  pthread_create(&chicken_thread, NULL, start_chicken, NULL);
  sleep(2);			/* give it some time to get running */

  pthread_create(&t1, NULL, start1, NULL);
  pthread_create(&t2, NULL, start2, NULL);
  pthread_join(t1, NULL);
  pthread_join(t2, NULL);
  printf("done.\n");
  return 0;
}
