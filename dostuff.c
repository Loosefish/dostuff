#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <magic.h>

#define DOFILE_DEFAULT "Dofile"
#define DOFILE_ENV "DOFILE"
#define DOFILE_PREFIX "do_"
#define PATH_SEP '/'


int test(char* actual_file) {
	const char *magic_full;
	magic_t magic_cookie = magic_open(MAGIC_NONE);
	if (!magic_cookie) {
		printf("unable to initialize magic library\n");
		return 1;
	}

	printf("Loading default magic database\n");
	if (magic_load(magic_cookie, NULL)) {
		printf("cannot load magic database - %s\n", magic_error(magic_cookie));
		magic_close(magic_cookie);
		return 1;
	}

	magic_full = magic_file(magic_cookie, actual_file);
	if (magic_full) {
		printf("%s\n", magic_full);
	}
	magic_close(magic_cookie);
	return 0;
}


char* get_dofile_name() {
	char* name = getenv(DOFILE_ENV);
	return name ? name : DOFILE_DEFAULT;
}

char* get_dofile() {
	// get working dir and Dofile name
	char* cwd = getcwd(NULL, 0);
	char* dofile = get_dofile_name();
	if (!cwd || !dofile) {
		return 0;
	}
	size_t cwd_n = strlen(cwd);
	size_t dofile_n = strlen(dofile);

	// build expected path for Dofile
	char* dopath = malloc(cwd_n + dofile_n + 2);
	strncpy(dopath, cwd, cwd_n);
	dopath[cwd_n] = PATH_SEP;
	strncpy(dopath + cwd_n + 1, dofile, dofile_n);

	// check if Dofile is readable
	if (access(dopath, R_OK) != -1) {
		return dopath;
	}
	free(cwd);
	free(dopath);
	return NULL;
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
		printf("Dofile exists!\n");
		test(dofile);
	}
	else {
		printf("No Dofile!\n");
	}
}
