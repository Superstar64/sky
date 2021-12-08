ski: ski.hs
	ghc ski

samples: sample/square.ski sample/fix.ski

%.ski : %.lambda ski
	./ski $< -o $@ 
