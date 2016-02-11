#define DOFILE_ENV "DOFILE"
#define DOFILE_DEFAULT "Dofile"
#define DOFILE_PREFIX "do_"

#define PATH_SEP "/"

#define MIME_SH "text/x-shellscript"
#define MIME_PY "text/x-python"

#define DIE_IF(cnd, msg) \
    if (cnd) { \
        fprintf(stderr, "Error: %s\n", msg); \
        exit(EXIT_FAILURE); \
    }

#define FREE_NULL(p) { free(p); p = NULL; }


enum Dotype {UNKNOWN, PY, SH};
typedef enum Dotype Dotype;

Dotype get_type(char* path);
void apply_type(Dotype dotype);
char* get_dofile_name();
char* get_dofile_rec();
void get_args(int argc, char *argv[], char** func, char** args);
bool has_func(char* dofile, char* func);
void run_func(char* dofile, char* func, char* args);
