!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column_extra
  USE mo_column, ONLY: ty_column, compute_column
  IMPLICIT NONE

CONTAINS

  SUBROUTINE compute_one(nz, q, t, pd_start, pd_stop, pd_step)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:,:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:,:) ! Field declared as one column only
    TYPE(ty_column)       :: column
    INTEGER, INTENT(IN)   :: pd_start, pd_stop, pd_step

    !$claw parallelize forward
    CALL column%compute_column(nz, q, t, pd_start, pd_stop, pd_step)
 
  END SUBROUTINE compute_one

  SUBROUTINE compute_two(nz, q, t, pd_start, pd_stop, pd_step)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:,:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:,:) ! Field declared as one column only
    TYPE(ty_column)       :: column
    INTEGER, INTENT(IN)   :: pd_start, pd_stop, pd_step
    INTEGER               :: pd_i

    !!! TEST YSK
    DO pd_i=pd_start, pd_stop, pd_step
      q(pd_i,:) = q(pd_i,:) + sum(t(pd_i,:))
    END DO

  END SUBROUTINE compute_two

END MODULE mo_column_extra
