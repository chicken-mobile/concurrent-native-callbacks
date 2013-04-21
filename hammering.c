/* stress-test for CNCB

   usage: hammering [NTHREADS [NCALLS]]

 */


#include <stdio.h>
#include <pthread.h>
#include <chicken.h>


#define MAX_THREADS         1000
#define DEFAULT_THREADS     10
#define DEFAULT_COUNT       1000


int counters[ MAX_THREADS ];
pthread_t threads[ MAX_THREADS ];
pthread_t chicken_thread;
pthread_mutex_t cmutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cvar = PTHREAD_COND_INITIALIZER;
int count = DEFAULT_COUNT;


extern int bar(int x);	/* callback */


static void *
start_chicken(void *arg)
{
  CHICKEN_run(C_toplevel);
  return NULL;
}


static void *
start(void *arg)
{
  long i = (long)arg;
  int j, r;

  printf("thread %ld waiting...\n", i);
  pthread_mutex_lock(&cmutex);
  pthread_cond_wait(&cvar, &cmutex);
  printf("thread %ld running ...\n", i);

  for(j = 0; j < count; ++j) {
    int r = bar(i + j);
    int rok = (i + j) * (i + j);

    if(r != rok) {
      printf("ERROR: thread %ld expected %d but got %d\n", i, rok, r);
      exit(EXIT_FAILURE);
    }
  }

  printf("thread %ld done.\n", i);
  return NULL;
}


int
main(int argc, char *argv[])
{
  int i;
  int n = DEFAULT_THREADS;

  pthread_create(&chicken_thread, NULL, start_chicken, NULL);
  sleep(2);			/* give it some time to get running */

  if(argc > 1)
    n = atoi(argv[ 1 ]);

  if(argc > 2)
    count = atoi(argv[ 2 ]);

  printf("creating %d threads ...\n", n);

  for(i = 0; i < n; ++i)
    pthread_create(&threads[ i ], NULL, start, (void *)(long)i);

  pthread_cond_broadcast(&cvar);

  for(i = 0; i < n; ++i) {
    printf("waiting for thread %d ...\n", i);
    pthread_join(threads[ i ], NULL);
  }

  printf("done.\n");
  return 0;
}
