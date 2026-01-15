      MODULE sierpinski_mod
      USE, INTRINSIC::ISO_FORTRAN_ENV, ONLY: DP => REAL64, I8=>INT64
      IMPLICIT NONE
      PRIVATE

      ! Integers to access the vertices.
      INTEGER, PRIVATE, PARAMETER :: V1 = 1
      INTEGER, PRIVATE, PARAMETER :: V2 = 2
      INTEGER, PRIVATE, PARAMETER :: V3 = 3

      TYPE, PRIVATE :: coordinate2D_t
      REAL(KIND=DP) :: x
      REAL(KIND=DP) :: y
      END TYPE coordinate2D_t

      TYPE, PRIVATE :: triangle_t
      INTEGER                              :: level
      TYPE(coordinate2D_t), DIMENSION(1:3) :: vertices
      END TYPE triangle_t

      TYPE, PUBLIC :: sierpinski_t
      TYPE(triangle_t) :: parent
      TYPE(sierpinski_t), DIMENSION(:), ALLOCATABLE :: children
      CONTAINS
        PROCEDURE, PUBLIC, PASS :: init
        PROCEDURE, PUBLIC, PASS :: generate
        PROCEDURE, PUBLIC, PASS :: plot_driver
        PROCEDURE, PUBLIC, PASS :: show
      END TYPE sierpinski_t

      CONTAINS

      SUBROUTINE init(sierpinski)
      USE, INTRINSIC::ISO_FORTRAN_ENV, ONLY: DP => REAL64
      IMPLICIT NONE
      CLASS(sierpinski_t), INTENT(INOUT) :: sierpinski

      ASSOCIATE(s => sierpinski, p => sierpinski%parent)
        p%level = 1
        ! Vertex 1
        p%vertices(V1)%x = -0.5_DP
        p%vertices(V1)%y = +0.0_DP
        ! Vertex 2
        p%vertices(V2)%x = +0.0_DP
        p%vertices(V2)%y = SQRT(1.0_DP - 0.5_DP**2)
        ! Vertex 3
        p%vertices(V3)%x = +0.5_DP
        p%vertices(V3)%y = +0.0_DP
      END ASSOCIATE

      END SUBROUTINE init

      RECURSIVE SUBROUTINE generate(sierpinski, max_level)
      USE, INTRINSIC::ISO_FORTRAN_ENV, ONLY: DP => REAL64
      IMPLICIT NONE
      CLASS(sierpinski_t), INTENT(INOUT) :: sierpinski
      INTEGER, INTENT(IN)                :: max_level
      TYPE(coordinate2D_t)               :: pA, pB, pC, mAB, mBC, mCA
      INTEGER                            :: i

      ASSOCIATE(s => sierpinski, p=>sierpinski%parent)
        ! s: sierpinski object.
        ! p: parent triangle in the sierpinski object.
        ! Vertex A.
        pA%x = p%vertices(V1)%x
        pA%y = p%vertices(V1)%y
        ! Vertex B.
        pB%x = p%vertices(V2)%x
        pB%y = p%vertices(V2)%y
        ! Vertex C.
        pC%x = p%vertices(V3)%x
        pC%y = p%vertices(V3)%y
        ! Mid-point of edge AB.
        mAB%x = 0.5_DP*(pA%x + pB%x)
        mAB%y = 0.5_DP*(pA%y + pB%y)
        ! Mid-point of edge BC.
        mBC%x = 0.5_DP*(pB%x + pC%x)
        mBC%y = 0.5_DP*(pB%y + pC%y)
        ! Mid-point of edge CA.
        mCA%x = 0.5_DP*(pC%x + pA%x)
        mCA%y = 0.5_DP*(pC%y + pA%y)
        ! Return if the maximum iteration level is reached already.
        IF (p%level .EQ. max_level) RETURN
        ! Allocate children (each parent has three children).
        ALLOCATE(s%children(1:3))
        ! Loop over all the children.
        DO i = 1, 3
          ASSOCIATE(si=>s%children(i), pi=>s%children(i)%parent)
            ! c: ith sierpinski object.
            ! s: parent triangle of this object.
            SELECT CASE (i)
              CASE(1)
                ! Vertex 1.
                pi%vertices(V1)%x = pA%x
                pi%vertices(V1)%y = pA%y
                ! Vertex 2.
                pi%vertices(V2)%x = mAB%x
                pi%vertices(V2)%y = mAB%y
                ! Vertex 3.
                pi%vertices(V3)%x = mCA%x
                pi%vertices(V3)%y = mCA%y
              CASE(2)
                ! Vertex 1.
                pi%vertices(V1)%x = mAB%x
                pi%vertices(V1)%y = mAB%y
                ! Vertex 2.
                pi%vertices(V2)%x = pB%x
                pi%vertices(V2)%y = pB%y
                ! Vertex 3.
                pi%vertices(V3)%x = mBC%x
                pi%vertices(V3)%y = mBC%y
              CASE(3)
                ! Vertex 1.
                pi%vertices(V1)%x = mCA%x
                pi%vertices(V1)%y = mCA%y
                ! Vertex 2.
                pi%vertices(V2)%x = mBC%x
                pi%vertices(V2)%y = mBC%y
                ! Vertex 3.
                pi%vertices(V3)%x = pC%x
                pi%vertices(V3)%y = pC%y
            END SELECT
            ! Update the current iteration level
            pi%level = p%level + 1
            CALL si%generate(max_level)
          END ASSOCIATE
        END DO
      END ASSOCIATE

      END SUBROUTINE generate

      SUBROUTINE plot_driver(sierpinski, max_level)
      USE plplot
      USE, INTRINSIC::ISO_FORTRAN_ENV, ONLY: DP => REAL64
      IMPLICIT NONE
      CLASS(sierpinski_t), INTENT(IN) :: sierpinski
      INTEGER,             INTENT(IN) :: max_level
      CHARACTER                       :: UI

      ! Set the device (keyword) name
      CALL plsdev('xwin')

      ! Initialize plplot
      CALL plinit

      ! Set the color
      CALL plcol0(15)

      ! Create a labelled box to hold the plot.
      CALL plenv (-0.5, 0.5, 0.0, 1.0, 1, -2)

      CALL sierpinski%show(max_level)

100   WRITE(*,FMT='(A)') 'Do you want to close the plot window? [y/n]'
      READ(*,FMT='(A)') UI

      IF (UI .EQ. 'n') THEN
        CALL SLEEP (5)
        GOTO 100
      END IF

      ! By default, PLplot's interactive devices (Xwin, TK, etc.) go into a wait
      ! state after a call to plend or other functions which trigger the end of
      ! a plot page. To avoid this, use the plspause function.
      ! https://plplot.sourceforge.net/docbook-manual/plplot-html-5.15.0/plend.html
      CALL plspause(.TRUE.)

!       CALL plend

      END SUBROUTINE plot_driver

      RECURSIVE SUBROUTINE show(sierpinski, max_level)
      USE plplot
      USE, INTRINSIC::ISO_FORTRAN_ENV, ONLY: DP => REAL64
      IMPLICIT NONE
      CLASS(sierpinski_t), INTENT(IN) :: sierpinski
      INTEGER,             INTENT(IN) :: max_level
      INTEGER                         :: i
      REAL(KIND=DP)                   :: x(2), y(2)

      ASSOCIATE(s => sierpinski, p=>sierpinski%parent)
        ! Draw a line between V1 and V2.
        x(1) = s%parent%vertices(V1)%x
        x(2) = s%parent%vertices(V2)%x
        y(1) = s%parent%vertices(V1)%y
        y(2) = s%parent%vertices(V2)%y
        CALL plline (x, y)
        ! Draw a line between V2 and V3.
        x(1) = s%parent%vertices(V2)%x
        x(2) = s%parent%vertices(V3)%x
        y(1) = s%parent%vertices(V2)%y
        y(2) = s%parent%vertices(V3)%y
        CALL plline (x, y)
        ! Draw a line between V3 and V1.
        x(1) = s%parent%vertices(V3)%x
        x(2) = s%parent%vertices(V1)%x
        y(1) = s%parent%vertices(V3)%y
        y(2) = s%parent%vertices(V1)%y
        CALL plline (x,y)
        ! Return if the maximum iteration level is reached already.
        IF (p%level .EQ. max_level) RETURN
        DO i = 1, 3
          CALL s%children(i)%show(max_level)
        END DO
      END ASSOCIATE

      END SUBROUTINE show

      END MODULE sierpinski_mod
