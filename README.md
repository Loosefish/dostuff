# dostuff

Locally scoped functions for the command line.

## Usage

A dofile is a Shell or Python script containing functions (named *do_<something>*). We can call these functions from the command line by executing `dostuff <something>` in the same directory or in a subdirectory of the dofile.

By default the dofile is named `Dofile`, this can be changed by setting the `DOFILE` environment variable.

Use `-f` to print all currently available dofiles and `-p` for all available functions.

You can check if a dofile is correctly recognized by running `file --mime-type <dofile>`. The output should be *text/x-shellscript* or *text/x-python*.

## Advanced Usage

We can also call functions with arguments: `dostuff <func> <arg1> <arg2> ...`.

Running the program without any arguments will execute a function named *do_*, arguments can't be passed to this default function.

If a function can't be found in the *local dofile* we go *up the directory tree* looking for other dofiles:

	foo
	├── Dofile      # look here last
	└── bar	        # the current directory
    	├── Dofile  # look here first
    	├── x
    	├── y
    	└── z
        


By default functions will be run with the working directory set to the *directory containing the dofile*. We can force the directory to be the *current working directory* with the `-l` flag.

We can also mark a function for execution in the callers working directory by appending a magic comment to the function definition line:

	do_wherever() {  # do_local
		...

or

	def do_wherever():  # do_local
		...

## Zsh

Tab completion for available functions:

	__dostuff () {
		local -a commands
		if test -e ./Dofile; then
			commands=($(dostuff -p))
			if (( CURRENT == 2 )); then
				_describe -t commands 'commands' commands
			fi
			return 0
		else
			return 1
		fi
	}
	compdef __dostuff dostuff




## Building

Use `tup` or the `build.sh` script (generated with `tup`).

Requires *libmagic* (sometimes packaged together with `file`).
