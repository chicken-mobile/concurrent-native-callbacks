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
  free(pptr);
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
    n = read(fd, &val, sizeof(void *));

    if(n == 0) return NULL;	/* EOF */
  }

  return val;
}
