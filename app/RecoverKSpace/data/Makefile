all:
	python3 data.py
	cp *.h5 ../../../algorithms/lbfgs-b-c/
	cd ../../.. ; stack build --fast; stack exec HashedExpression-exe
	cd ../../../algorithms/lbfgs-b-c; make fast; ./lbfgs-b
