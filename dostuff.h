#define DOFILE_ENV "DOFILE"
#define DOFILE_DEFAULT "Dofile"
#define DOFILE_PREFIX "do_"

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
char* get_dofile_name();
char* get_dofile_rec();
void apply_type(Dotype dotype);
void get_func(int argc, char *argv[], char** func, char** args);
void prep_func(char* dofile, char* func);
void print_funcs(char* dofile);
void run_func(char* dofile, char* func, char* args);
void set_cwd(char* dofile, char* func_line);
