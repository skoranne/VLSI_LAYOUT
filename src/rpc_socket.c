/* File   : rpc_socket.c
 * Purpose: Minimal POSIX TCP client used by MAGPARSER's RPC server mode
 *          (rpc_server.f90).  MAGPARSER connects to the local Julia cluster
 *          worker on 127.0.0.1:<port>; all cross-machine traffic is carried by
 *          Julia (Distributed.jl), so this file only needs a blocking client.
 * Linux, macOS (no Windows support needed).
 */
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

/* Connect to host:port; returns fd >= 0 or -1. */
int rpc_connect(const char *host, int port)
{
    char service[16];
    struct addrinfo hints, *res = NULL, *p;
    int fd = -1, one = 1;
    snprintf(service, sizeof service, "%d", port);
    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    if (getaddrinfo(host, service, &hints, &res) != 0) return -1;
    for (p = res; p != NULL; p = p->ai_next) {
        fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
        if (fd < 0) continue;
        if (connect(fd, p->ai_addr, p->ai_addrlen) == 0) break;
        close(fd);
        fd = -1;
    }
    freeaddrinfo(res);
    if (fd >= 0) setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one);
    return fd;
}

/* Send exactly n bytes; 0 on success, -1 on error/EOF. */
int rpc_send_all(int fd, const void *buf, int64_t n)
{
    const char *p = (const char *)buf;
    while (n > 0) {
        ssize_t k = send(fd, p, (size_t)(n > (1 << 30) ? (1 << 30) : n), 0);
        if (k < 0 && errno == EINTR) continue;
        if (k <= 0) return -1;
        p += k;
        n -= k;
    }
    return 0;
}

/* Receive exactly n bytes; 0 on success, -1 on error/EOF. */
int rpc_recv_all(int fd, void *buf, int64_t n)
{
    char *p = (char *)buf;
    while (n > 0) {
        ssize_t k = recv(fd, p, (size_t)(n > (1 << 30) ? (1 << 30) : n), 0);
        if (k < 0 && errno == EINTR) continue;
        if (k <= 0) return -1;
        p += k;
        n -= k;
    }
    return 0;
}

void rpc_close(int fd)
{
    if (fd >= 0) close(fd);
}

/* 1 on little-endian hosts (the wire format is little-endian). */
int rpc_little_endian(void)
{
    const uint16_t one = 1;
    return *(const uint8_t *)&one == 1;
}
