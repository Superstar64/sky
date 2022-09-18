bin: sky runsky

sky: sky.hs
	ghc $@

runsky: runsky.c
	gcc $< -o $@

samples=sample/hello.lambda sample/math.lambda sample/lazy.lambda

samples: $(samples:%.lambda=%.run_c) $(samples:%.lambda=%.run_py)

$(samples:%.lambda=%.sky): %.sky : %.lambda sky
	./sky $< -o $@ 

$(samples:%.lambda=%.run_c): %.run_c : %.sky runsky
	./runsky $<
	touch $@

$(samples:%.lambda=%.run_py): %.run_py : %.sky runsky.py
	python3 runsky.py $<
	touch $@

