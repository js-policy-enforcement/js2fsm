main: *.hs
	ghc -o js2fsm Main.hs
	rm *.hi *.o

clean:
	rm js2fsm
