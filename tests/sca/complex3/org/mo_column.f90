!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column
  IMPLICIT NONE

  TYPE :: ty_column

    CONTAINS
    PROCEDURE :: compute_column
  END TYPE ty_column

  CONTAINS

    ! Compute only one column
    SUBROUTINE compute_column(frelpt_start, frelpt_stop, frelpt_step, this, nz, q, t)
    IMPLICIT NONE

    CLASS(ty_column) :: this
    INTEGER, INTENT(IN) :: nz
    ! Size of the array field
    REAL, INTENT(INOUT) :: t(:, :)
    ! Field declared as one column only
    REAL, INTENT(INOUT) :: q(:, :)
    ! Field declared as one column only
    INTEGER :: k
    ! Loop index
    REAL :: c
    ! Coefficient

    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw parallelize

    INTEGER, INTENT(IN) :: frelpt_start, frelpt_stop, frelpt_step
    INTEGER :: frelpt_index
    c = 5.345
    DO k = 2, nz
      DO frelpt_index = frelpt_start, frelpt_stop, frelpt_step
        t(frelpt_index, k) = c * k
      END DO
      DO frelpt_index = frelpt_start, frelpt_stop, frelpt_step
        q(frelpt_index, k) = q(frelpt_index, k - 1) + t(frelpt_index, k) * c
      END DO
    END DO
    DO frelpt_index = frelpt_start, frelpt_stop, frelpt_step
      q(frelpt_index, nz) = q(frelpt_index, nz) * c
    END DO
  END SUBROUTINE compute_column

END MODULE mo_column
