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


char** get_dofiles();
char* get_dofile_name();

Dotype get_type(char* path);
void apply_type(Dotype dotype);

void print_funcs(char** dofiles);

void execute(int argc, char* argv[], char** dofiles);
void get_func(int argc, char* argv[], char** func, char** args);
bool prep_func(char* dofile, char* func);
void set_cwd(char* dofile, char* func_line);
void execute_func(char* dofile, char* func, char* args);
