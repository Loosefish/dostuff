#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <magic.h>
#include <regex.h>

#define DOFILE_DEFAULT "Dofile"
#define DOFILE_ENV "DOFILE"
#define DOFILE_PREFIX "do_"
#define DOFILE_PREFIX_N 3

#define PATH_SEP "/"

#define MIME_SH "text/x-shellscript"
#define MIME_SH_N 18
#define MIME_PY "text/x-python"
#define MIME_PY_N 13

#define RE_SH "^ *%s *() *{ *$"
#define RE_PY "^def *%s *("

#define DIE_IF(cnd, msg) \
    if (cnd) { \
        fprintf(stderr, "Error: %s\n", msg); \
        exit(EXIT_FAILURE); \
    }


enum Dotype {UNKNOWN, PY, SH};
typedef enum Dotype Dotype;


Dotype get_type(char* path) {
	const char* magic_type;
	magic_t magic_cookie = magic_open(MAGIC_MIME);
	DIE_IF(!magic_cookie, "Failed to initialize magic library");
	DIE_IF(magic_load(magic_cookie, NULL), "Failed to load magic database");

	magic_type = magic_file(magic_cookie, path);
	Dotype dotype = UNKNOWN;
	if (magic_type) {
		if (strncmp(magic_type, MIME_SH, MIME_SH_N) == 0) {
			dotype = SH;
		}
		else if (strncmp(magic_type, MIME_PY, MIME_PY_N) == 0) {
			dotype = PY;
		}
	}
	magic_close(magic_cookie);
	return dotype;
}


char* get_dofile_name() {
	char* name = getenv(DOFILE_ENV);
	return name ? name : DOFILE_DEFAULT;
}

char* get_dofile() {
	// get working dir and Dofile name
	char* cwd = getcwd(NULL, 0);
	DIE_IF(!cwd, "Can't locate current dir");
	char* dofile = get_dofile_name();

	// build expected path for Dofile
	char* dopath;
	asprintf(&dopath, "%s%s%s", cwd, PATH_SEP, dofile);

	free(cwd);
	// check if Dofile is readable
	DIE_IF(access(dopath, R_OK), "Can't read Dofile");
	return dopath;
}


char* get_cmd(int argc, char *argv[]) {
	char *cmd;
	if (argc > 1) {
		size_t arg_n = strlen(argv[1]);
		cmd = calloc(DOFILE_PREFIX_N + 1 + arg_n, sizeof(char));
		strncpy(cmd, DOFILE_PREFIX, DOFILE_PREFIX_N);
		strncpy(cmd + DOFILE_PREFIX_N, argv[1], arg_n);
	}
	else {
		cmd = calloc(DOFILE_PREFIX_N + 1, sizeof(char));
		strncpy(cmd, DOFILE_PREFIX, DOFILE_PREFIX_N);
	}
	return cmd;
}


bool has_cmd(char* dofile, Dotype dotype, char* cmd) {
	char *pattern;
	regex_t cmd_re;
	switch (dotype) {
		case PY:
			asprintf(&pattern, RE_PY, cmd); 
			break;
		case SH:
		default:
			asprintf(&pattern, RE_SH, cmd); 
			break;
	}

	// compile regex
	int r = regcomp(&cmd_re, pattern, REG_NOSUB|REG_NEWLINE);
	free(pattern);
	DIE_IF(r, "Can't compile regex.");

	// check lines in dofile against regex
	FILE* fp = fopen(dofile, "r");
	DIE_IF(!fp, "Can't open Dofile");

	char buffer[1024];
	while (fgets(buffer, 1024, fp)) {
		r = regexec(&cmd_re, buffer, 0, NULL, 0);
		if (r == 0) {
			break;
		}
	}
	fclose(fp);

	regfree(&cmd_re);
	return r == 0 ? true : false;
}


void run_cmd(char* dofile, Dotype dotype, char* cmd) {
}


int main(int argc, char *argv[]) {
	// if Dofile exists
		// if arg == '-' -> list commands
		// else if arg in dofile -> execute command
		// else "Unknown command!"
	// else if arg is path -> init Dofile in path
	// else "No Dofile!"
	char *dofile = get_dofile();
	if (dofile) {
		Dotype dotype = get_type(dofile);
		DIE_IF(dotype == UNKNOWN, "Dofile has unknown type");

		char *cmd = get_cmd(argc, argv);
		printf("DOFILE: %s\n", dofile);
		printf("CMD: %s\n", cmd);
		DIE_IF(!has_cmd(dofile, dotype, cmd), "Unknown command");

		printf("Found cmd\n");

		free(cmd);
		free(dofile);
	}
	else {
		printf("No Dofile!\n");
	}
}
