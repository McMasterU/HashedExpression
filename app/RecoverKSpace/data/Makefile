all:
	python3 data.py
	cp *.h5 ../../../algorithms/lbfgs/
	cd ../../.. ; stack build --fast; stack exec HashedExpression-exe
	cd ../../../algorithms/lbfgs; make fast; ./lbfgs
	cd ../../../algorithms/lbfgs; python3 plot.py x
