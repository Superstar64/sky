bin: sky runsky

sky: sky.hs
	ghc $@

runsky: runsky.c
	gcc $< -o $@

samples: sample/hello.run sample/math.run

sample/hello.sky sample/math.sky: %.sky : %.lambda sky
	./sky $< -o $@ 

sample/hello.run sample/math.run: %.run : %.sky runsky
	./runsky $<
	touch $@

