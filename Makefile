all:
	dune build enable_wins.exe

clean:
	dune clean
	rm -f *~
