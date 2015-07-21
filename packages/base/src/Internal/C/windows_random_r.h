#if defined(_WIN32) || defined(WIN32)

#ifndef WINDOWS_RANDOM_R_H
#define WINDOWS_RANDOM_R_H

#include <stddef.h>

#define EINVAL 23

typedef int int32_t;

struct random_data
{
    int *fptr;          /* Front pointer.  */
    int *rptr;          /* Rear pointer.  */
    int *state;         /* Array of state values.  */
    int rand_type;      /* Type of random number generator.  */
    int rand_deg;       /* Degree of random number generator.  */
    int rand_sep;       /* Distance between front and rear.  */
    int *end_ptr;       /* Pointer behind state table.  */
};

extern int windows_random_r_errno;

static inline void __set_errno(int err) { windows_random_r_errno = err; };

int random_r (struct random_data *buf, int32_t *result);

int initstate_r (unsigned int seed,
        char *arg_state,
        /*unsigned long*/ size_t n,
        struct random_data *buf);

#endif
#endif