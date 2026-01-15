FC=gfortran

a.out: mod_sierpinski.o sierpinski.o
	$(FC) -o a.out mod_sierpinski.o sierpinski.o -L/usr/lib64 -lplplotfortran

mod_sierpinski.o: mod_sierpinski.f90
	$(FC) -c mod_sierpinski.f90 -I/usr/lib64/gfortran/modules

sierpinski.o: sierpinski.f90
	$(FC) -c sierpinski.f90

all: mod_sierpinski.o sierpinski.o a.out

clean:
	rm -rf *.mod *.out *.o
