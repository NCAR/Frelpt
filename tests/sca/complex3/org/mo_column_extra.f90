!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column_extra
  USE mo_column, ONLY: ty_column, compute_column
  IMPLICIT NONE

  CONTAINS

    SUBROUTINE compute_one(frelpt_start, frelpt_stop, frelpt_step, nz, q1, t1)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nz
    ! Size of the array field
    REAL, INTENT(INOUT) :: t1(:, :)
    ! Field declared as one column only
    REAL, INTENT(INOUT) :: q1(:, :)
    ! Field declared as one column only
    TYPE(ty_column) :: column

    !$claw parallelize forward
    INTEGER, INTENT(IN) :: frelpt_start, frelpt_stop, frelpt_step
    INTEGER :: frelpt_index
    CALL column % compute_column(frelpt_start, frelpt_stop, frelpt_step, nz, q1, t1)

  END SUBROUTINE compute_one

    SUBROUTINE compute_two(frelpt_start, frelpt_stop, frelpt_step, nz, q, t)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nz
    ! Size of the array field
    REAL, INTENT(INOUT) :: t(:, :)
    ! Field declared as one column only
    REAL, INTENT(INOUT) :: q(:, :)
    ! Field declared as one column only
    TYPE(ty_column) :: column

    !!! TEST YSK
    INTEGER, INTENT(IN) :: frelpt_start, frelpt_stop, frelpt_step
    INTEGER :: frelpt_index
    DO frelpt_index = frelpt_start, frelpt_stop, frelpt_step
      q(frelpt_index, :) = q(frelpt_index, :) + t(frelpt_index)
    END DO

    !!!!!! expected !!!!!
    !    DO frelpt_index=frelpt_start, frelpt_stop, frelpt_step
    !        q(frelpt_index,:) = q(frelpt_index,:) + sum(t(frelpt_index,:))
    !    END DO
  END SUBROUTINE compute_two

END MODULE mo_column_extra
