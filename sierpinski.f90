!     Problem is taken from a course on FORTRAN.
!     KULeuvenX: FORTRANx-Fortran-For-Scientific-Programming
!     
!     Problem: Sierpinski triangle is an example of a fractal. There are several
!     algorithms to construct it, but the "chaos game" is rather neat.  Look at
!     the description on Wikipedia:
!     https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle#Chaos_game
!     Implement an application that writes the positions of the points to
!     standard output.  Make a plot using your favorite visualization
!     application.
!
!     A Sierpinski triange can be understood as a generalization of a binary tree.
!     https://en.wikipedia.org/wiki/M-ary_tree
!     Compile the program using:
!     gfortran -c mod_sierpinski.f90 -I/usr/lib64/gfortran/modules
!     gfortran -c sierpinski.f90
!     Linking:
!     gfortran -o a.out sierpinski.o mod_sierpinski.o -L/usr/lib64 -lplplotfortran
      PROGRAM sierpinski
      USE sierpinski_mod, ONLY: sierpinski_t
      IMPLICIT NONE
      TYPE(sierpinski_t) :: sierpinski_gasket
      INTEGER            :: nmax
      INTEGER            :: counter

      WRITE(*,FMT='(A)') 'Enter the maximum level of construction (INTEGER >0)'
      READ(*,*) nmax

      CALL sierpinski_gasket%init()

      CALL sierpinski_gasket%generate(max_level=nmax)

      CALL sierpinski_gasket%plot_driver(max_level=nmax)

      END PROGRAM sierpinski
