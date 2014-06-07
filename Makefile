coverage:
	mkdir -p .hpc
	ghc --make -fhpc -itests -o .hpc/Main tests/Main.hs
	-./.hpc/Main
	hpc report Main
	hpc markup --destdir=.hpc \
	    --exclude=Data.IntPSQ.Tests    \
	    --exclude=Data.PSQ.Class       \
	    --exclude=Data.PSQ.Class.Tests \
	    --exclude=Main                 \
	    Main
	@echo "Now go check out .hpc/hpc_index.html"
