/* viewer_gl.c: small C helpers for MAGVIEWER (Fortran keeps the application logic).
 *
 *  - Input: GLFW key / character / scroll / mouse-button callbacks push events into a ring
 *    buffer that Fortran drains once per frame (viewer_input_next). This avoids Fortran
 *    procedure-pointer callbacks and makes edge-triggered keys reliable ('?', '1'..'0', ...).
 *  - Text: 8x13 bitmap glyphs (table from viewer_font.f90, passed once) drawn with glBitmap
 *    at a raster position in the current (pixel) projection.
 *  - Screenshot: glReadPixels of the back buffer to a binary PPM.
 * GL is the system libGL (compatibility profile); prototypes are declared here because this
 * box has no GL development headers.
 */
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#include <stdio.h>
#include <stdlib.h>

typedef unsigned int GLenum;
typedef int GLint;
typedef int GLsizei;
typedef float GLfloat;
typedef unsigned char GLubyte;
extern void glRasterPos2f(GLfloat x, GLfloat y);
extern void glBitmap(GLsizei w, GLsizei h, GLfloat xo, GLfloat yo, GLfloat xm, GLfloat ym, const GLubyte* b);
extern void glPixelStorei(GLenum p, GLint v);
extern void glReadPixels(GLint x, GLint y, GLsizei w, GLsizei h, GLenum f, GLenum t, void* d);
extern void glReadBuffer(GLenum m);
extern void glFinish(void);
#define GL_UNPACK_ALIGNMENT 0x0CF5
#define GL_PACK_ALIGNMENT 0x0D05
#define GL_RGB 0x1907
#define GL_UNSIGNED_BYTE 0x1401
#define GL_BACK 0x0405

/* ---------------- input queue ---------------- */
typedef struct { int type, code, mods, action; double x, y; } ViewerEvent;
#define QN 4096
static ViewerEvent q[QN];
static int qhead = 0, qtail = 0;

static void push(int type, int code, int mods, int action, double x, double y) {
    int nt = (qtail + 1) % QN;
    if (nt == qhead) return;                       /* full: drop */
    q[qtail].type = type; q[qtail].code = code; q[qtail].mods = mods;
    q[qtail].action = action; q[qtail].x = x; q[qtail].y = y;
    qtail = nt;
}
static void key_cb(GLFWwindow* w, int key, int sc, int action, int mods) {
    (void)w; (void)sc;
    if (action == GLFW_RELEASE) return;
    push(1, key, mods, action, 0, 0);
}
static void char_cb(GLFWwindow* w, unsigned int cp) { (void)w; push(2, (int)cp, 0, 1, 0, 0); }
static void scroll_cb(GLFWwindow* w, double dx, double dy) {
    double cx, cy; glfwGetCursorPos(w, &cx, &cy);
    push(3, dy > 0 ? 1 : (dy < 0 ? -1 : 0), 0, 1, cx, cy);
    (void)dx;
}
static void mouse_cb(GLFWwindow* w, int button, int action, int mods) {
    double cx, cy; glfwGetCursorPos(w, &cx, &cy);
    push(4, button, mods, action, cx, cy);
}

/* event types: 1 key (code = GLFW key, action 1 press / 2 repeat), 2 character (code = Unicode),
   3 scroll (code = +1/-1, x/y = cursor), 4 mouse button (action 1 press / 0 release, x/y = cursor) */
void viewer_input_install(GLFWwindow* w) {
    glfwSetKeyCallback(w, key_cb);
    glfwSetCharCallback(w, char_cb);
    glfwSetScrollCallback(w, scroll_cb);
    glfwSetMouseButtonCallback(w, mouse_cb);
}
int viewer_input_next(int* type, int* code, int* mods, int* action, double* x, double* y) {
    if (qhead == qtail) return 0;
    ViewerEvent* e = &q[qhead];
    *type = e->type; *code = e->code; *mods = e->mods; *action = e->action; *x = e->x; *y = e->y;
    qhead = (qhead + 1) % QN;
    return 1;
}
static void err_cb(int code, const char* desc) { fprintf(stderr, "GLFW error %d: %s\n", code, desc); }
void viewer_glfw_errors(void) { glfwSetErrorCallback(err_cb); }
/* Before window creation: MAGVIEWER_EGL=1 creates the context through EGL instead of GLX
   (works on X servers without GLX, e.g. a VNC server, with Mesa's software renderer). */
void viewer_context_hints(void) {
    const char* e = getenv("MAGVIEWER_EGL");
    if (e && *e && *e != '0') glfwWindowHint(GLFW_CONTEXT_CREATION_API, GLFW_EGL_CONTEXT_API);
}
/* Inject key presses from a test script: space-separated tokens. A single letter or digit is
   a key press (its GLFW key code is the upper-case ASCII); '?' and '/' are characters;
   PgUp / PgDn / Esc are keys; "S-x" adds Shift. Returns the number of events queued. */
int viewer_input_script(const char* s, int n) {
    int k = 0, i = 0;
    while (i < n) {
        while (i < n && s[i] == ' ') ++i;
        int j = i;
        while (j < n && s[j] != ' ') ++j;
        if (j > i) {
            const char* t = s + i; int len = j - i, mods = 0;
            if (len > 2 && t[0] == 'S' && t[1] == '-') { mods = GLFW_MOD_SHIFT; t += 2; len -= 2; }
            if (len == 1) {
                char c = t[0];
                if (c == '?' || c == '/') push(2, c, 0, 1, 0, 0);
                else {
                    if (c >= 'a' && c <= 'z') c = (char)(c - 32);
                    push(1, c, mods, 1, 0, 0);
                }
                ++k;
            } else if (len == 4 && t[0] == 'P' && t[2] == 'U') { push(1, GLFW_KEY_PAGE_UP, mods, 1, 0, 0); ++k; }
            else if (len == 4 && t[0] == 'P' && t[2] == 'D') { push(1, GLFW_KEY_PAGE_DOWN, mods, 1, 0, 0); ++k; }
            else if (len == 3 && t[0] == 'E') { push(1, GLFW_KEY_ESCAPE, mods, 1, 0, 0); ++k; }
        }
        i = j;
    }
    return k;
}
void viewer_swap_interval(int n) { glfwSwapInterval(n); }
double viewer_time(void) { return glfwGetTime(); }

/* ---------------- text ---------------- */
static const signed char* font_bits = NULL;     /* 13 rows x 95 glyphs, bottom row first */
void viewer_text_set_font(const signed char* bits) { font_bits = bits; }

/* Draw s[0..n) with the lower-left of the first glyph cell at (x, y) in the current projection
   (callers use a pixel projection). Characters outside 32..126 draw as '?'. */
void viewer_draw_text(float x, float y, const char* s, int n) {
    if (!font_bits || n <= 0) return;
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glRasterPos2f(x, y);
    for (int i = 0; i < n; ++i) {
        int c = (unsigned char)s[i];
        if (c < 32 || c > 126) c = '?';
        glBitmap(8, 13, 0.0f, 0.0f, 8.0f, 0.0f, (const GLubyte*)(font_bits + (c - 32) * 13));
    }
}

/* ---------------- screenshot ---------------- */
int viewer_screenshot_ppm(const char* path, int w, int h) {
    if (w <= 0 || h <= 0) return 1;
    unsigned char* px = (unsigned char*)malloc((size_t)w * h * 3);
    if (!px) return 1;
    glFinish();
    glReadBuffer(GL_BACK);
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
    glReadPixels(0, 0, w, h, GL_RGB, GL_UNSIGNED_BYTE, px);
    FILE* f = fopen(path, "wb");
    if (!f) { free(px); return 1; }
    fprintf(f, "P6\n%d %d\n255\n", w, h);
    for (int r = h - 1; r >= 0; --r) fwrite(px + (size_t)r * w * 3, 1, (size_t)w * 3, f);
    fclose(f);
    free(px);
    return 0;
}
void viewer_finish(void) { glFinish(); }
