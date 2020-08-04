all:
	mkdir -p brain
	symphony brain.sp -o brain
	cp *.h5 brain
	cd brain; make fast; ./lbfgs-b; python3 plot.py x
