MODULE mo_column_extra
 USE mo_column , ONLY: ty_column , compute_column

CONTAINS
 SUBROUTINE compute_one ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  TYPE ( ty_column ) :: column

  INTEGER :: proma

  CALL column % compute_column ( nz , q , t , nproma = nproma )

  DO proma = 1 , nproma , 1
      q(proma, :) = q(proma, :) + sum(t(proma, : ))
  END DO

  ! sum of the particular colume t
  ! add the sum to all of a particular column of q
  !q = q + sum(t)

 END SUBROUTINE compute_one

END MODULE mo_column_extra

