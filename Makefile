
gravity: $(wildcard src/*.hs)
	pushd src; \
	ghc -outputdir ../build -O2 --make Main.hs -o ../gravity; \
	popd

clean:
	rm -rf gravity build

#push.git:
#	git push origin

