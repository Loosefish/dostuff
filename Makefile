CC=gcc
CFLAGS=-Wall -std=gnu99 -O3 
LIBS=-lmagic
DEPS=dostuff.h
OBJ=dostuff.o

all: dostuff

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

dostuff: $(OBJ)
	gcc -o $@ $^ $(CFLAGS) $(LIBS)

clean:
	rm -f dostuff *.o
