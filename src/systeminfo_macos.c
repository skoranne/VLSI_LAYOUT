#include <stdint.h>
#include <unistd.h>

#if defined(__APPLE__)
#include <mach/mach.h>
#include <sys/sysctl.h>

// Replaces reading VmRSS and VmSize from /proc/self/status (returns bytes)
void get_macos_memory(int64_t *rss_bytes, int64_t *virtual_bytes) {
    struct mach_task_basic_info info;
    mach_msg_type_number_t count = MACH_TASK_BASIC_INFO_COUNT;
    if (task_info(mach_task_self(), MACH_TASK_BASIC_INFO, (task_info_t)&info, &count) == KERN_SUCCESS) {
        *rss_bytes = (int64_t)info.resident_size;
        *virtual_bytes = (int64_t)info.virtual_size;
    } else {
        *rss_bytes = -1;
        *virtual_bytes = -1;
    }
}

// Replaces reading Pid from /proc/self/status
int32_t get_macos_pid(void) {
    return (int32_t)getpid();
}

// Replaces reading TracerPid from /proc/self/status (returns 1 if traced, 0 if not)
int32_t is_macos_traced(void) {
    int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID, getpid()};
    struct kinfo_proc info;
    size_t size = sizeof(info);
    info.kp_proc.p_flag = 0;
    if (sysctl(mib, 4, &info, &size, NULL, 0) != 0) return 0;
    return (info.kp_proc.p_flag & P_TRACED) ? 1 : 0;
}
#else
void get_macos_memory(int64_t *rss_bytes, int64_t *virtual_bytes) {
    (void)rss_bytes; (void)virtual_bytes;
    if (rss_bytes) *rss_bytes = -1;
    if (virtual_bytes) *virtual_bytes = -1;
}
int32_t get_macos_pid(void) { return (int32_t)getpid(); }
int32_t is_macos_traced(void) { return 0; }
#endif
