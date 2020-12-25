/* alloca.c -- allocate automatically reclaimed memory
   (Mostly) portable public-domain implementation -- D A Gwyn

   Taken from GNU binutils 2.10.1.
   Minor changes
   Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>

   This implementation of the PWB library alloca function,
   which is used to allocate space off the run-time stack so
   that it is automatically reclaimed upon procedure exit,
   was inspired by discussions with J. Q. Johnson of Cornell.

   There are some preprocessor constants that can
   be defined when compiling for your specific system, for
   improved efficiency; however, the defaults should be okay.

   The general concept of this implementation is to keep
   track of all alloca-allocated blocks, and reclaim any
   that are found to be deeper in the stack than the current
   invocation.  This heuristic does not reclaim storage as
   soon as it becomes invalid, but it will do so eventually.

   As a special case, alloca(0) reclaims storage without
   allocating any.  It is a good idea to use alloca(0) in
   your main control loop, etc. to force garbage collection.  */

#include <stdlib.h>
#include <unistd.h>

#define ALIGN_SIZE 4
#define ADDRESS_FUNCTION(arg) &(arg)
#define STACK_DIR -1

union alloca_header
{
  char align[ALIGN_SIZE];       /* To force sizeof(union alloca_header).  */
  struct
  {
    union alloca_header *next;  /* For chaining headers.  */
    char *deep;                 /* For stack depth measure.  */
  } h;
};

static union alloca_header *last_alloca_header = NULL;  /* -> last alloca header.  */

/* Return a void * to at least SIZE bytes of storage,
   which will be automatically reclaimed upon exit from
   the procedure that called alloca.  Originally, this space
   was supposed to be taken from the current stack frame of the
   caller, but that method cannot be made to work for some
   implementations of C, for example under Gould's UTX/32.  */

void *
alloca (size_t size)
{
  char probe;                   /* Probes stack depth: */
  char *depth = ADDRESS_FUNCTION (probe);

  /* Reclaim garbage, defined as all alloca'd storage that
     was allocated from deeper in the stack than currently.  */

  {
    union alloca_header *hp;    /* Traverses linked list.  */

    for (hp = last_alloca_header; hp != NULL;)
      if ((STACK_DIR > 0 && hp->h.deep > depth) || (STACK_DIR < 0 && hp->h.deep < depth))
        {
          union alloca_header *np = hp->h.next;

          free ((void *) hp);   /* Collect garbage.  */

          hp = np;              /* -> next header.  */
        }
      else
        break;                  /* Rest are not deeper.  */

    last_alloca_header = hp;    /* -> last valid storage.  */

  }

  if (size == 0)
    return NULL;                /* No allocation required.  */

  /* Allocate combined header + user data storage.  */

  {
    void *new = malloc (sizeof (union alloca_header) + size);
    /* Address of header.  */

    if (new == 0)
      abort ();

    ((union alloca_header *) new)->h.next = last_alloca_header;
    ((union alloca_header *) new)->h.deep = depth;

    last_alloca_header = (union alloca_header *) new;

    /* User storage begins just after header.  */

    return (void *) ((char *) new + sizeof (union alloca_header));
  }
}
