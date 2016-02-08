#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <magic.h>
#include <regex.h>

#include "dostuff.h"

char *FUNC_PATTERN = NULL;
char *FUNC_ARG_SEP = NULL;
char *EXEC_FMT = NULL;
char *EXEC_ARGS_FMT = NULL;

void apply_type(Dotype dotype) {
	switch (dotype) {
		case PY:
			FUNC_PATTERN = "^def *%s *(";
			FUNC_ARG_SEP = ", ";
			EXEC_FMT = "python -B -c \"exec(open('%s').read()); %s()\"";
			EXEC_ARGS_FMT = "python -B -c \"exec(open('%s').read()); %s(%s)\"";
			break;
		case SH:
		default:
			FUNC_PATTERN = "^ *%s *() *{ *$";
			FUNC_ARG_SEP = " ";
			EXEC_FMT = "source %s && %s";
			EXEC_ARGS_FMT = "source %s && %s %s";
			break;
	}
}

Dotype get_type(char* path) {
	const char* magic_type;
	magic_t magic_cookie = magic_open(MAGIC_MIME);
	DIE_IF(!magic_cookie, "Failed to initialize magic library");
	DIE_IF(magic_load(magic_cookie, NULL), "Failed to load magic database");

	magic_type = magic_file(magic_cookie, path);
	Dotype dotype = UNKNOWN;
	if (magic_type) {
		if (strncmp(magic_type, MIME_SH, strlen(MIME_SH)) == 0) {
			dotype = SH;
		}
		else if (strncmp(magic_type, MIME_PY, strlen(MIME_PY)) == 0) {
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
		asprintf(&cmd, "%s%s", DOFILE_PREFIX, argv[1]);
	}
	else {
		asprintf(&cmd, "%s", DOFILE_PREFIX);
	}
	return cmd;
}


void get_cmd_args(Dotype dotype, int argc, char *argv[], char** cmd, char** args) {
	if (argc > 2) {
		size_t total = 1;
		for (size_t i = 2; i < argc; ++i) {
			total += strlen(argv[i]);
		}
		*args = calloc(total + 1 + ((argc - 3) * strlen(FUNC_ARG_SEP)), 1);
		strcpy(*args, argv[2]);
		for (size_t i = 3; i < argc; ++i) {
			strcat(*args, FUNC_ARG_SEP);
			strcat(*args, argv[i]);
		}
	}

	if (argc > 1) {
		asprintf(cmd, "%s%s", DOFILE_PREFIX, argv[1]);
	}
	else {
		asprintf(cmd, "%s", DOFILE_PREFIX);
	}
}


bool has_cmd(char* dofile, Dotype dotype, char* cmd) {
	char *pattern;
	regex_t cmd_re;
	asprintf(&pattern, FUNC_PATTERN, cmd); 

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
	// TODO: ARGS
	char* cmd_full;
	asprintf(&cmd_full, EXEC_FMT, dofile, cmd);
	system(cmd_full);
	free(cmd_full);
}


void run_cmd_args(char* dofile, Dotype dotype, char* cmd_name, char* cmd_args) {
	char* cmd_full;
	asprintf(&cmd_full, EXEC_ARGS_FMT, dofile, cmd_name, cmd_args);
	system(cmd_full);
	free(cmd_full);
}


int main(int argc, char *argv[]) {
	char *dofile = get_dofile();
	if (dofile) {
		Dotype dotype = get_type(dofile);
		apply_type(dotype);
		DIE_IF(dotype == UNKNOWN, "Dofile has unknown type");

		char *cmd_name = NULL;
		char *cmd_args = NULL;
		get_cmd_args(dotype, argc, argv, &cmd_name, &cmd_args);
		/* printf("DOFILE: %s\n", dofile); */
		/* printf("CMD: %s\n", cmd_name); */
		/* printf("CMD_ARGS: %s\n", cmd_args); */
		DIE_IF(!has_cmd(dofile, dotype, cmd_name), "Unknown command");
		// if arg is path -> init Dofile in path

		if (cmd_args) {
			run_cmd_args(dofile, dotype, cmd_name, cmd_args);
		}
		else {
			run_cmd(dofile, dotype, cmd_name);
		}

		free(cmd_name);
		free(cmd_args);
		free(dofile);
	}
	else {
		printf("No Dofile!\n");
	}
}
