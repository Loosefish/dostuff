/* TODO
 * Flag for forced local execution?
 * DOFILE as list of filenames? -> Multiple dofiles per dir?
 */
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


bool LOCAL = false;

char* EXEC_ARGS_FMT = NULL;
char* EXEC_FMT = NULL;
char* FUNC_ARG_SEP = NULL;
char* FUNC_LOCAL_SUF = NULL;
char* FUNC_PATTERN = NULL;
char* NAME_PATTERN = NULL;


/* Look for dofiles recursively upwards.
 * Returns: NULL-terminated array of pathnames.
 */
char** get_dofiles() {
	size_t n = 0;
	char** dofiles = calloc(sizeof(char*), 1);

	char* dir = getcwd(NULL, 0);
	char* dofile;
	do {
		asprintf(&dofile, "%s/%s", dir, get_dofile_name());
		// file readable?
		if (access(dofile, R_OK) == 0) {
			dofiles[n++] = dofile;
			dofiles = realloc(dofiles, sizeof(char*) * (n + 1));
		}
		else {
			FREE_NULL(dofile);
		}
		dir = dirname(dir);
	}
	while (strcmp(dir, "/"));
	FREE_NULL(dir);
	dofiles[n] = NULL;
	return dofiles;
}


char* get_dofile_name() {
	char* name = getenv(DOFILE_ENV);
	return name ? name : DOFILE_DEFAULT;
}


/* Returns the Dotype for the given path. */
Dotype get_type(char* path) {
	const char* magic_type;
	magic_t magic_cookie = magic_open(MAGIC_MIME);
	DIE_IF(!magic_cookie, "Couldn't initialize magic library");
	DIE_IF(magic_load(magic_cookie, NULL), "Couldn't load magic database");

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


/* Sets global variables according to the given Dotype. */
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
			EXEC_ARGS_FMT = "source \"%s\" && %s %s";
			EXEC_FMT = "source \"%s\" && %s";
			FUNC_ARG_SEP = " ";
			FUNC_LOCAL_SUF = "do_local";
			FUNC_PATTERN = "^ *%s *() *{ *";
			NAME_PATTERN = "^ *%s\\([[:alnum:]]*\\) *() *{ *";
			break;
	}
}


/* Prints all function names in the given dofiles. */
void print_funcs(char** dofiles) {
	regex_t func_re;
	char* pattern;
	size_t nmatch = 2;
	regmatch_t pmatch[2];
	char buffer[1024];

	char** df = dofiles;
	while (*df) {
		// apply settings for current file type
		apply_type(get_type(*df));

		// prepare regex
		asprintf(&pattern, NAME_PATTERN, DOFILE_PREFIX);
		DIE_IF(!pattern, "Couldn't allocate memory");
		int r = regcomp(&func_re, pattern, REG_NEWLINE);
		DIE_IF(r, "Couldn't compile regex");
		FREE_NULL(pattern);

		// check lines in dofile against regex
		FILE* fp = fopen(*df, "r");
		DIE_IF(!fp, "Couldn't open dofile");

		while (fgets(buffer, 1024, fp)) {
			r = regexec(&func_re, buffer, nmatch, pmatch, 0);
			if (r == 0) {
				buffer[pmatch[1].rm_eo] = 0;
				printf("%s\n", buffer + pmatch[1].rm_so);
			}
		}
		fclose(fp);
		regfree(&func_re);
		df++;
	}
}


/* Translates the program args into a function name and an argument string. */
void get_func(int argc, char* argv[], char** func, char** args) {
	if (argc > 2) {
		size_t total = 1;
		for (size_t i = 2; i < argc; ++i) {
			total += strlen(argv[i]);
		}
		*args = calloc(total + 1 + ((argc - 3) * strlen(FUNC_ARG_SEP)), 1);
		DIE_IF(!args, "Couldn't allocate memory");
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
	DIE_IF(!func, "Couldn't allocate memory");
}


/* Checks if a function exists in a dofile and sets the working directory
 * according to the function definition.
 * Returns: true if function exists in file, false otherwise.
 */
bool prep_func(char* dofile, char* func) {
	regex_t func_re;
	char* pattern;
	asprintf(&pattern, FUNC_PATTERN, func);
	DIE_IF(!pattern, "Couldn't allocate memory");

	// compile regex
	int r = regcomp(&func_re, pattern, REG_NOSUB|REG_NEWLINE);
	FREE_NULL(pattern);
	DIE_IF(r, "Couldn't compile regex.");

	// check lines in dofile against regex
	FILE* fp = fopen(dofile, "r");
	DIE_IF(!fp, "Couldn't open dofile");

	char buffer[1024] = {0};
	while (fgets(buffer, 1024, fp)) {
		r = regexec(&func_re, buffer, 0, NULL, 0);
		if (r == 0) {
			break;
		}
	}
	set_cwd(dofile, buffer);
	fclose(fp);
	regfree(&func_re);
	return r == 0 ? true : false;
}


/* Sets the working directory according to the given function definition.
 * If the function is to be executed globally, the working directory is set to
 * the directory containing the dofile.
 */
void set_cwd(char* dofile, char* func_line) {
	// TODO: Use regex? How safe is this?
	size_t suf_n = strlen(FUNC_LOCAL_SUF);
	int offset = strlen(func_line) - 1 - suf_n;
	if (LOCAL || (offset > 0 && strncmp(func_line + offset, FUNC_LOCAL_SUF, suf_n) == 0)) {
		return;
	}

	size_t n = strlen(dofile) + 1;
	char* tmp = malloc(n);
	memcpy(tmp, dofile, n);
	tmp = dirname(tmp);
	DIE_IF(chdir(tmp), "Couldn't set working directory.");
	FREE_NULL(tmp);
}


/* Executes a function with or without arguments. */
void execute_func(char* dofile, char* func, char* args) {
	char* func_full;
	if (args) {
		asprintf(&func_full, EXEC_ARGS_FMT, dofile, func, args);
	}
	else {
		asprintf(&func_full, EXEC_FMT, dofile, func);
	}
	DIE_IF(!func_full, "Couldn't allocate memory");
	system(func_full);
	FREE_NULL(func_full);
}


/* Tries to execute a function according to the program arguments.
 * Iterates though dofiles until execution is successful.
 */
void execute(int argc, char* argv[], char** dofiles) {
	char* func = NULL;
	char* args = NULL;
	char** df = dofiles;
	bool done = false;
	while (*df && !done) {
		// apply settings for current file type
		apply_type(get_type(*df));

		// build function string with arguments
		get_func(argc, argv, &func, &args);
		// check if function exists and set working dir
		if (prep_func(*df, func)) {
			// execute function
			execute_func(*df, func, args);
			done = true;
		}
		FREE_NULL(func);
		FREE_NULL(args);
		df++;
	}
	DIE_IF(!done, "No such function");
}


int main(int argc, char* argv[]) {
	if (argc > 1 && (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)) {
		fprintf(
		    stderr,
		    "Usage: %s [-f | -p | [-l] FUNCTION [ARG1 ...]]\n%s\n%s\n%s\n%s\n",
		    argv[0],
		    "  -f                   Print paths of available dofiles",
		    "  -p                   Print names of available functions",
		    "  -l                   Force execution in current working directory",
		    "  FUNCTION [ARG1 ...]  Call local function with optional arguments"
		);
		return EXIT_SUCCESS;
	}

	// gather dofiles
	char** dofiles = get_dofiles();
	DIE_IF(!dofiles[0], "Couldn't locate dofile");

	// check for flags
	if (argc > 1 && strcmp(argv[1], "-f") == 0) {
		// print dofile paths and exit
		char** df = dofiles;
		while (*df) {
			printf("%s\n", *df);
			df++;
		}
	}
	else if (argc > 1 && strcmp(argv[1], "-p") == 0) {
		// print function names and exit
		print_funcs(dofiles);
	}
	else {
		if (argc > 1 && strcmp(argv[1], "-l") == 0) {
			// force local execution
			LOCAL = true;
			argc--;
			argv = &argv[1];
		}
		execute(argc, argv, dofiles);
	}

	// free dofiles
	char** df = dofiles;
	while (*df) {
		FREE_NULL(*df);
		df++;
	}
	FREE_NULL(dofiles);
}
