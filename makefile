ski: ski.hs
	ghc ski

samples: sample/square.ski sample/fix.ski sample/insert_sort/insert.py

sample/insert_sort/insert.py: sample/insert_sort/insert_runtime.py sample/insert_sort/insert.ski
	sed "s/TERM/"`cat sample/insert_sort/insert.ski`'/g' sample/insert_sort/insert_runtime.py > $@


%.lambda : %.lambdacpp
	cpp -P $< -o $@

%.ski : %.lambda ski
	cat $< | ./ski > $@ 
