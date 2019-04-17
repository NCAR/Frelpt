!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column
  IMPLICIT NONE

  TYPE ty_column

  CONTAINS
    PROCEDURE :: compute_column
  END TYPE ty_column

CONTAINS

  ! Compute only one column
  SUBROUTINE compute_column(this, nz, q, t, pd_start, pd_stop, pd_step)
    IMPLICIT NONE

    CLASS(ty_column)      :: this
    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:,:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:,:) ! Field declared as one column only
    INTEGER :: k                  ! Loop index
    REAL :: c                     ! Coefficient
    INTEGER, INTENT(IN)   :: pd_start, pd_stop, pd_step
    INTEGER               :: pd_i

    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw parallelize

    c = 5.345
    DO k = 2, nz
      DO pd_i=pd_start, pd_stop, pd_step
        t(pd_i, k) = c * k
      END DO
      DO pd_i=pd_start, pd_stop, pd_step
        q(pd_i, k) = q(pd_i, k - 1)  + t(pd_i, k) * c
      END DO
    END DO
    DO pd_i=pd_start, pd_stop, pd_step
      q(pd_i, nz) = q(pd_i, nz) * c
    END DO
  END SUBROUTINE compute_column

END MODULE mo_column
