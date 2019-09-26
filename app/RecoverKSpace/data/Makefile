all:
	python3 data.py
	cp *.h5 ../../../algorithms/lbfgs/
	cd ../../.. ; stack build --fast; stack exec HashedExpression-exe
	cd ../../../algorithms/lbfgs; make fast; ./lbfgs
#	cp ../../../algorithms/lbfgs/x.txt p.txt
#	python3 display.py