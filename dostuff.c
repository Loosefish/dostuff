// TODO: list functions
// TODO: DOFILE as list of filenames?
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


char* get_dofile_name() {
	char* name = getenv(DOFILE_ENV);
	return name ? name : DOFILE_DEFAULT;
}


char* get_dofile_rec() {
	// TODO: portability?
	char* dofile;
	char* wd;
	for (;;) {
		wd = getcwd(NULL, 0);
		asprintf(&dofile, "%s%s%s", wd, PATH_SEP, get_dofile_name());
		if (access(dofile, R_OK) == 0) {
			free(wd);
			return dofile;
		}
		DIE_IF(strcmp("/", wd) == 0, "Can't locate Dofile");

		free(wd);
		free(dofile);
		DIE_IF(chdir("..") != 0, "Can't locate Dofile");
	}
}


void get_args(int argc, char *argv[], char** func, char** args) {
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
		asprintf(func, "%s%s", DOFILE_PREFIX, argv[1]);
	}
	else {
		asprintf(func, "%s", DOFILE_PREFIX);
	}
}


bool has_func(char* dofile, char* func) {
	regex_t func_re;
	char *pattern;
	asprintf(&pattern, FUNC_PATTERN, func);

	// compile regex
	int r = regcomp(&func_re, pattern, REG_NOSUB|REG_NEWLINE);
	free(pattern);
	DIE_IF(r, "Can't compile regex.");

	// check lines in dofile against regex
	FILE* fp = fopen(dofile, "r");
	DIE_IF(!fp, "Can't open Dofile");

	char buffer[1024];
	while (fgets(buffer, 1024, fp)) {
		r = regexec(&func_re, buffer, 0, NULL, 0);
		if (r == 0) {
			break;
		}
	}
	fclose(fp);

	regfree(&func_re);
	return r == 0 ? true : false;
}


void run_func(char* dofile, char* func, char* args) {
	char* func_full;
	if (args) {
		asprintf(&func_full, EXEC_ARGS_FMT, dofile, func, args);
	}
	else {
		asprintf(&func_full, EXEC_FMT, dofile, func);
	}
	system(func_full);
	free(func_full);
}


int main(int argc, char *argv[]) {
	char *dofile = get_dofile_rec();
	if (dofile) {
		// determine filetype and apply settings
		Dotype dotype = get_type(dofile);
		apply_type(dotype);
		DIE_IF(dotype == UNKNOWN, "Dofile has unknown type");

		if (argc > 1 && strcmp(argv[1], "-f") == 0) {
			// print dofile path and exit
			printf("%s\n", dofile);
		}
		else {
			// build function name and args
			char *func = NULL;
			char *args = NULL;
			get_args(argc, argv, &func, &args);

			DIE_IF(!has_func(dofile, func), "Unknown command");
			// if arg is path -> init Dofile in path

			run_func(dofile, func, args);

			free(func);
			free(args);
		}
		free(dofile);
	}
	else {
		printf("No Dofile!\n");
	}
}
