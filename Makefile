CFLAGS=-std=c99 -Wall -ledit -lm -g

all: parsing

parsing:
	cc $(CFLAGS) parsing.c mpc.c -o parsing
clean:
	rm -f parsing
