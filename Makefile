coverage:
	mkdir -p .hpc
	rm -f .hpc/*.mix
	rm -f Main.tix
	ghc --make -fhpc -fforce-recomp -isrc -itests -o .hpc/Main tests/Main.hs
	-./.hpc/Main
	hpc report Main
	hpc markup --destdir=.hpc \
	    --exclude=Data.HashPSQ.Tests    \
	    --exclude=Data.IntPSQ.Tests     \
	    --exclude=Data.PSQ.Class        \
	    --exclude=Data.PSQ.Class.Gen    \
	    --exclude=Data.PSQ.Class.Tests  \
	    --exclude=Data.PSQ.Tests        \
	    --exclude=Data.PSQ.Tests.Util   \
	    --exclude=Main                  \
	    Main
	rm -f Main.tix
	@echo "Now go check out .hpc/hpc_index.html"
