CFLAGS=-std=c99 -Wall -ledit -lm -g

all: prompt parsing

parsing:
	cc $(CFLAGS) parsing.c mpc.c -o parsing
clean:
	rm -f prompt
	rm -f parsing
