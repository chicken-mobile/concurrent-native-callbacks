/* lowlevel hackery - accessed via "bind" */


static void **
extract_argument_ptr(void *buf)
{
  void **pptr = (void **)malloc(sizeof(void *));
  
  assert(pptr);
  *pptr = (char *)buf + sizeof(2); /* skip cb-name and cvar */
  return pptr;
}


static void
free_argument_ptr(void **ptr)
{
  free(ptr);
}


static ___symbol
extract_callback_name(void *buf)
{
  return *((char **)buf);
}


static void
trigger_return(void *buf)
{
  pthread_cond_signal(*((pthread_cond_t **)buf + 1));
}


static void *
read_message(int fd)
{
  void *val;
  int c, n;

  for(c = sizeof(val); c > 0; c -= n) {
    printf("start read (fd %d)\n", fd);	/* XXX */
    n = read(fd, &val, sizeof(void *));
    printf("read: %d\n", n);	/* XXX */

    if(n == 0) return NULL;	/* EOF */
  }

  return val;
}


static void
send_termination_message(int fd)
{
  void *n = NULL;
  
  write(fd, &n, sizeof(void *));
}
