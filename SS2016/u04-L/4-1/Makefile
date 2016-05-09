
# Wer kein stack installiert hat, sollte die folgenden Zeilen einkommentieren
# INT = runghc
# CC = ghci

# Wer stack installiert hat, sollte die folgenden Zeilen auskommentieren
INT = stack runghc
CC = stack ghc

FLAGS = -O2

compile-all: clength cappend celem creverse clean
profile-all: length append elem reverse

clength: laenge.hs

		$(CC) -- laenge.hs $(FLAGS) -o laenge

cappend: anhaengen.hs

		$(CC) -- anhaengen.hs $(FLAGS) -o anhaengen

celem: enthaelt.hs

		$(CC) -- enthaelt.hs $(FLAGS) -o enthaelt

creverse: drehen.hs

		$(CC) -- drehen.hs $(FLAGS) -o drehen

length: laenge.hs

	$(INT)  -- laenge.hs --output laenge-bench.html

append: anhaengen.hs

	$(INT) -- anhaengen.hs --output anhaengen-bench.html

elem: enthaelt.hs

	$(INT) -- enthaelt.hs --output enthaelt-bench.html

reverse: drehen.hs

	$(INT) -- drehen.hs --output drehen-bench.html


clean-all: clean clean-bin clean-html

clean:
	-rm -f *.o
	-rm -f *.hi

clean-bin:
	-rm -f laenge
	-rm -f anhaengen
	-rm -f enthaelt
	-rm -f drehen

clean-html:
	-rm -f *.html 