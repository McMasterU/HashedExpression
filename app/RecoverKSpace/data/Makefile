all:
	python3 data.py
	cp *.h5 ../../../algorithms/lbfgs-b-c/
	cd ../../.. ; stack build --fast; stack exec HashedExpression-exe
	cd ../../../algorithms/lbfgs-b; make fast; ./lbfgs-b
	cd ../../../algorithms/lbfgs-b; python3 plot.py x
