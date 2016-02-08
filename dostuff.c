#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <magic.h>
#include <regex.h>

#include "dostuff.h"


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
	// TODO: ARGS
	char* cmd_full;
	switch (dotype) {
		case PY:
			asprintf(&cmd_full, EX_PY, dofile, cmd);
			break;
		default:
		case SH:
			asprintf(&cmd_full, EX_SH, dofile, cmd);
			break;
	}
	system(cmd_full);
	free(cmd_full);
}


int main(int argc, char *argv[]) {
	char *dofile = get_dofile();
	if (dofile) {
		Dotype dotype = get_type(dofile);
		DIE_IF(dotype == UNKNOWN, "Dofile has unknown type");

		char *cmd = get_cmd(argc, argv);
		printf("DOFILE: %s\n", dofile);
		printf("CMD: %s\n", cmd);
		DIE_IF(!has_cmd(dofile, dotype, cmd), "Unknown command");
		// if arg is path -> init Dofile in path

		run_cmd(dofile, dotype, cmd);

		free(cmd);
		free(dofile);
	}
	else {
		printf("No Dofile!\n");
	}
}
