CC=ghc


all: PrettyJSON.o Main.o SimpleJSON.o simple Prettify.o

PrettyJSON.o: SimpleJSON.o
	$(CC) -c PrettyJSON.hs

Prettify.o:
	$(CC) -c Prettify.hs

simple: SimpleJSON.o Main.o
	$(CC) -o simple SimpleJSON.o Main.o

SimpleJSON.o: SimpleJSON.hs
	$(CC) -c SimpleJSON.hs

Main.o: Main.hs
	$(CC) -c Main.hs

clean:
	rm simple Main.o SimpleJSON.o Prettify.o PrettyJSON.o
