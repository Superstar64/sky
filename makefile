bin: sky run_sky

sky: sky.hs
	ghc $@

run_sky: run_sky.c
	gcc $< -o $@

samples=sample/hello.lambda sample/math.lambda sample/lazy.lambda

samples: $(samples:%.lambda=%.run_c) $(samples:%.lambda=%.run_py)

$(samples:%.lambda=%.sky): %.sky : %.lambda sky
	./sky $< -o $@ 

runtime.sky : runtime.lambda sky
	./sky $< -o $@

$(samples:%.lambda=%.run_c): %.run_c : %.sky run_sky runtime.sky
	./run_sky runtime.sky $<
	touch $@

$(samples:%.lambda=%.run_py): %.run_py : %.sky run_sky.py
	python3 run_sky.py $<
	touch $@

