
#fc='ifort'
fc='gfortran'

pushdown: clean
	${fc} -o pushdown.exe mo_column_pushdown.f90 mo_column_extra_pushdown.f90 main_pushdown.f90 && ./pushdown.exe

clean:
	rm -rf *.o *.mod *.exe

