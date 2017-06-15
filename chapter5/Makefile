CC=ghc

simple: SimpleJSON.o Main.o
	$(CC) -o simple SimpleJSON.o Main.o

SimpleJSON.o: SimpleJSON.hs
	$(CC) -c SimpleJSON.hs

Main.o: Main.hs
	$(CC) -c Main.hs

clean:
	rm simple Main.o SimpleJSON.o
