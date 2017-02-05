# dostuff

Locally scoped functions for the command line.

## Usage

A dofile is a Bash script containing functions (named *do_<something>*). We can call these functions from the command line by executing `dostuff <something>` in the same directory or in a subdirectory of the dofile.

By default the dofile is named `Dofile`, this can be changed by setting the `DOFILE` environment variable.

Use `-f` to print all currently available dofiles and `-p` for all available functions.

Reachable dofiles may be `source`'d on invocation. To avoid accidental execution it's best to keep all code inside functions.

## Advanced Usage

We can also call functions with arguments: `dostuff <func> <arg1> <arg2> ...`.

Running the program without any arguments will execute a function named `do_`, arguments can't be passed to this default function.

If a function can't be found in the *local dofile* we go *up the directory tree* looking for other dofiles:

	foo
	├── Dofile      # look here last
	└── bar	        # the current directory
    	├── Dofile  # look here first
    	├── x
    	├── y
    	└── z



By default functions will be run with the working directory set to the *directory containing the dofile*. We can force the directory to be the *current working directory* with the `-l` flag.

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

Use `cabal build`.

See `dostuff.cabal` for requierements.
