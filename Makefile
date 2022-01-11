out/matrix.o: src/matrix.f08
	gfortran -fopenacc -c -g src/matrix.f08	-o out/matrix.o

program: out/matrix.o ./tests/program.f08
	gfortran -fopenacc -g out/matrix.o tests/program.f08 -o out/run


clean:
	rm -rf out
	rm matrix.mod
	mkdir out
