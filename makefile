ski: ski.hs
	ghc ski

samples: sample/square.ski sample/fix.ski


%.lambda : %.lambdacpp
	cpp -P $< -o $@

%.ski : %.lambda ski
	cat $< | ./ski > $@ 
