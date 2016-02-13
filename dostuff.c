// TODO: Magic comments for declaring "local" functions (flag also?)
// TODO: DOFILE as list of filenames?
#define _GNU_SOURCE
#include <libgen.h>
#include <magic.h>
#include <regex.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "dostuff.h"

char *EXEC_ARGS_FMT = NULL;
char *EXEC_FMT = NULL;
char *FUNC_ARG_SEP = NULL;
char *FUNC_LOCAL_SUF = NULL;
char *FUNC_PATTERN = NULL;
char *NAME_PATTERN = NULL;


Dotype get_type(char* path) {
	const char* magic_type;
	magic_t magic_cookie = magic_open(MAGIC_MIME);
	DIE_IF(!magic_cookie, "Can't initialize magic library");
	DIE_IF(magic_load(magic_cookie, NULL), "Can't load magic database");

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
			EXEC_ARGS_FMT = "python -B -c \"exec(open('%s').read()); %s(%s)\"";
			EXEC_FMT = "python -B -c \"exec(open('%s').read()); %s()\"";
			FUNC_ARG_SEP = ", ";
			FUNC_LOCAL_SUF = "do_local";
			FUNC_PATTERN = "^def *%s *(";
			NAME_PATTERN = "^def *%s\\([[:alnum:]]*\\) *(";
			break;
		case SH:
		default:
			EXEC_ARGS_FMT = "source %s && %s %s";
			EXEC_FMT = "source %s && %s";
			FUNC_ARG_SEP = " ";
			FUNC_LOCAL_SUF = "do_local";
			FUNC_PATTERN = "^ *%s *() *{ *";
			NAME_PATTERN = "^ *%s\\([[:alnum:]]*\\) *() *{ *";
			break;
	}
}


char* get_dofile_name() {
	char* name = getenv(DOFILE_ENV);
	return name ? name : DOFILE_DEFAULT;
}


char* get_dofile_rec() {
	char *dir = getcwd(NULL, 0);
	char *buf = calloc(strlen(dir) + strlen(get_dofile_name()) + 2, 1);

	for (;;) {
		sprintf(buf, "%s/%s", dir, get_dofile_name());
		if (access(buf, R_OK) == 0) {
			FREE_NULL(dir);
			return buf;
		}
		DIE_IF(strcmp(dir, "/") == 0, "Can't locate dofile");
		dir = dirname(dir);
	}
}


void print_funcs(char* dofile) {
	regex_t func_re;
	char *pattern;
	size_t nmatch = 2;
	regmatch_t pmatch[2];
	asprintf(&pattern, NAME_PATTERN, DOFILE_PREFIX);
	DIE_IF(!pattern, "Can't allocate memory");

	// compile regex
	int r = regcomp(&func_re, pattern, REG_NEWLINE);
	FREE_NULL(pattern);
	DIE_IF(r, "Can't compile regex.");

	// check lines in dofile against regex
	FILE* fp = fopen(dofile, "r");
	DIE_IF(!fp, "Can't open dofile");

	char buffer[1024];
	while (fgets(buffer, 1024, fp)) {
		r = regexec(&func_re, buffer, nmatch, pmatch, 0);
		if (r == 0) {
			buffer[pmatch[1].rm_eo] = 0;
			printf("%s\n", buffer + pmatch[1].rm_so);
		}
	}
	fclose(fp);
	regfree(&func_re);
}


void get_func(int argc, char *argv[], char** func, char** args) {
	if (argc > 2) {
		size_t total = 1;
		for (size_t i = 2; i < argc; ++i) {
			total += strlen(argv[i]);
		}
		*args = calloc(total + 1 + ((argc - 3) * strlen(FUNC_ARG_SEP)), 1);
		DIE_IF(!args, "Can't allocate memory");
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
	DIE_IF(!func, "Can't allocate memory");
}


void prep_func(char* dofile, char* func) {
	regex_t func_re;
	char *pattern;
	asprintf(&pattern, FUNC_PATTERN, func);
	DIE_IF(!pattern, "Can't allocate memory");

	// compile regex
	int r = regcomp(&func_re, pattern, REG_NOSUB|REG_NEWLINE);
	FREE_NULL(pattern);
	DIE_IF(r, "Can't compile regex.");

	// check lines in dofile against regex
	FILE* fp = fopen(dofile, "r");
	DIE_IF(!fp, "Can't open dofile");

	char buffer[1024];
	while (fgets(buffer, 1024, fp)) {
		r = regexec(&func_re, buffer, 0, NULL, 0);
		if (r == 0) {
			set_cwd(dofile, buffer);
			fclose(fp);
			regfree(&func_re);
			return;
		}
	}
	fprintf(stderr, "Can't locate function '%s'\n", func);
	exit(EXIT_FAILURE);
}


void set_cwd(char* dofile, char* func_line) {
	size_t suf_n = strlen(FUNC_LOCAL_SUF);
	if (strncmp(func_line + strlen(func_line) - 1 - suf_n, FUNC_LOCAL_SUF, suf_n) == 0) {
		return;
	}

	size_t n = strlen(dofile) + 1;
	char* tmp = malloc(n);
	memcpy(tmp, dofile, n);
	tmp = dirname(tmp);
	DIE_IF(chdir(tmp), "Can't set working directory.");
	FREE_NULL(tmp);
}


void run_func(char* dofile, char* func, char* args) {
	char* func_full;
	if (args) {
		asprintf(&func_full, EXEC_ARGS_FMT, dofile, func, args);
	}
	else {
		asprintf(&func_full, EXEC_FMT, dofile, func);
	}
	DIE_IF(!func_full, "Can't allocate memory");
	system(func_full);
	FREE_NULL(func_full);
}


int main(int argc, char *argv[]) {
	if (argc > 1 && (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)) {
		// print function names and exit
		fprintf(
			stderr,
			"Usage: %s [-f | -l | FUNCTION [ARG1 ...]]\n%s\n%s\n%s\n",
			argv[0],
			"  -f                   Print path of the local dofile",
			"  -l                   Print local functions",
			"  FUNCTION [ARG1 ...]  Call local function with optional arguments"
		);
		return EXIT_SUCCESS;
	}

	// get dofile, determine filetype and apply settings
	char *dofile = get_dofile_rec();
	Dotype dotype = get_type(dofile);
	DIE_IF(dotype == UNKNOWN, "Dofile has unknown type");
	apply_type(dotype);

	// check arguments
	if (argc > 1 && strcmp(argv[1], "-f") == 0) {
		// print dofile path and exit
		printf("%s\n", dofile);
	}
	else if (argc > 1 && strcmp(argv[1], "-l") == 0) {
		// print function names and exit
		print_funcs(dofile);
	}
	else {
		// build function name and args
		char *func = NULL;
		char *args = NULL;
		get_func(argc, argv, &func, &args);

		// check if function exists, set working dir and run
		prep_func(dofile, func);
		run_func(dofile, func, args);

		FREE_NULL(func);
		FREE_NULL(args);
	}
	FREE_NULL(dofile);
}
