all: 
	ghc -O3 -threaded -with-rtsopts="-N" --make -o dmenu_run++ dmenu_run++.hs
	rm *.o *.hi
