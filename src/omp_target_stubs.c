#include <stddef.h>
/* Stubs for OpenMP target runtime API when libomptarget is not linked.
 * Avoids pulling libomptarget (which can SIGSEGV on systems without GPU offload). */
int omp_target_disassociate_ptr(const void *host_ptr, int device_num) { (void)host_ptr; (void)device_num; return 0; }
void *omp_target_alloc(size_t size, int device_num) { (void)size; (void)device_num; return (void*)0; }
void omp_target_free(void *device_ptr, int device_num) { (void)device_ptr; (void)device_num; }
int omp_target_memcpy(void *dst, const void *src, size_t length, size_t dst_offset, size_t src_offset, int dst_device_num, int src_device_num) { (void)dst; (void)src; (void)length; (void)dst_offset; (void)src_offset; (void)dst_device_num; (void)src_device_num; return 0; }
int omp_target_is_present(const void *ptr, int device_num) { (void)ptr; (void)device_num; return 0; }
int omp_target_associate_ptr(const void *host_ptr, const void *device_ptr, size_t size, size_t device_offset, int device_num) { (void)host_ptr; (void)device_ptr; (void)size; (void)device_offset; (void)device_num; return 0; }
