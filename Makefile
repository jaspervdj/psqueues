coverage:
	mkdir -p .hpc
	rm -f .hpc/*.mix
	rm -f Main.tix
	ghc --make -fhpc -fforce-recomp -itests -o .hpc/Main tests/Main.hs
	-./.hpc/Main
	hpc report Main
	hpc markup --destdir=.hpc \
	    --exclude=Data.IntPSQ.Tests    \
	    --exclude=Data.PSQ.Class       \
	    --exclude=Main                 \
	    Main
	rm -f Main.tix
	@echo "Now go check out .hpc/hpc_index.html"
