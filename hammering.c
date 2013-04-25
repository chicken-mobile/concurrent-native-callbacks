/* stress-test for CNCB

   usage: hammering [NTHREADS [NCALLS]]

 */


#include <stdio.h>
#include <pthread.h>
#include <chicken.h>
#include <unistd.h>


#define MAX_THREADS         100
#define DEFAULT_THREADS     10
#define DEFAULT_COUNT       100


pthread_t threads[ MAX_THREADS ];
pthread_t chicken_thread;
pthread_mutex_t cmutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cvar = PTHREAD_COND_INITIALIZER;
int count = DEFAULT_COUNT;
int condition = 0;


extern int bar(int t, int x);	/* callback */


static void *
start_chicken(void *arg)
{
  CHICKEN_run(C_toplevel);
  printf("chicken returned.\n");
  return NULL;
}


static void *
start(void *arg)
{
  long i = (long)arg;
  int j, r;

  printf("thread %ld waiting...\n", i);
  pthread_mutex_lock(&cmutex);

  while(!condition)
    pthread_cond_wait(&cvar, &cmutex);

  printf("thread %ld running ...\n", i);
  pthread_mutex_unlock(&cmutex);

  for(j = 1; j <= count; ++j) {
    int r, rok;

    usleep(1000 * 250);		/* 250ms */

    printf("thread %ld calling (%ld) ...\n", i, i + j);
    r = bar((int)i, i + j);
    printf("thread %ld call returned: %d\n", i, r);
    rok = (i + j) * (i + j);

    if(r != rok) {
      printf("ERROR: thread %ld expected %d but got %d\n", i, rok, r);
      exit(EXIT_FAILURE);
    }
  }

  printf("thread %ld done.\n", i);
  return NULL;
}


/* XXX something is seriously wrong here. Some thread will forever wait and thus
   not be joined.
 */

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

  printf("starting threads ...\n");
  pthread_mutex_lock(&cmutex);
  condition = 1;
  pthread_cond_broadcast(&cvar);
  pthread_mutex_unlock(&cmutex);

  for(i = 0; i < n; ++i) {
    printf("waiting for thread %d ...\n", i);
    pthread_join(threads[ i ], NULL);
  }

  printf("done.\n");
  return 0;
}
