#define DOFILE_ENV "DOFILE"
#define DOFILE_DEFAULT "Dofile"
#define DOFILE_PREFIX "do_"

#define PATH_SEP "/"

#define MIME_SH "text/x-shellscript"
#define MIME_PY "text/x-python"

#define RE_SH "^ *%s *() *{ *$"
#define RE_PY "^def *%s *("

#define EX_SH "source %s && %s"
#define EX_PY "python -B -c \"exec(open('%s').read()); %s()\""

#define DIE_IF(cnd, msg) \
    if (cnd) { \
        fprintf(stderr, "Error: %s\n", msg); \
        exit(EXIT_FAILURE); \
    }

enum Dotype {UNKNOWN, PY, SH};
typedef enum Dotype Dotype;

Dotype get_type(char* path);
char* get_dofile_name();
char* get_dofile();
char* get_cmd(int argc, char *argv[]);
void run_cmd(char* dofile, Dotype dotype, char* cmd);
int main(int argc, char *argv[]);
