
#fc='ifort'
fc='gfortran'

org: clean
	${fc} -o org.exe mo_column.f90 mo_column_extra.f90 main.f90 && ./org.exe

clean:
	rm -rf *.o *.mod *.exe

