/* OpenMP device memory routines for a runtime that has no offload support.
 *
 * The classic Intel Fortran runtime on macOS (libiomp5) declares these in
 * omp_lib but does not define them, so device_registry.f90 does not link.
 * With no target device every "device" is the host, and the OpenMP rules for
 * the host device apply: allocation is ordinary memory, a host pointer is
 * always present, and nothing can be associated with a second address.
 * device_registry.f90 then takes its "already mapped: no residency" path.
 *
 * Built only by Makefile.mac.
 */
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

void *omp_target_alloc(size_t size, int device_num)
{
    (void)device_num;
    return malloc(size);
}

void omp_target_free(void *device_ptr, int device_num)
{
    (void)device_num;
    free(device_ptr);
}

int omp_target_is_present(const void *ptr, int device_num)
{
    (void)ptr; (void)device_num;
    return 1;
}

int omp_target_memcpy(void *dst, const void *src, size_t length, size_t dst_offset,
                      size_t src_offset, int dst_device_num, int src_device_num)
{
    (void)dst_device_num; (void)src_device_num;
    if (!dst || !src) return 1;
    memmove((char *)dst + dst_offset, (const char *)src + src_offset, length);
    return 0;
}

int omp_target_associate_ptr(const void *host_ptr, const void *device_ptr, size_t size,
                             size_t device_offset, int device_num)
{
    (void)host_ptr; (void)device_ptr; (void)size; (void)device_offset; (void)device_num;
    return 1;   /* not supported on the host device */
}

int omp_target_disassociate_ptr(const void *ptr, int device_num)
{
    (void)ptr; (void)device_num;
    return 1;
}
