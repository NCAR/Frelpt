
#fc='ifort'
fc='gfortran'

#CLAW_VER = 09212018
CLAW_VER = 02252019

claw='/glade/p/cisl/asap/youngsun/opt/claw/${CLAW_VER}/bin/clawfc'

# main.f90
# mo_column_extra.f90
# mo_column.f90

org: clean
	${fc} -o org.exe mo_column.f90 mo_column_extra.f90 main.f90 && ./org.exe

cpu:
	${claw} -o test_mo_column_cpu.f90 -d=claw -t=cpu mo_column.f90
	${claw} -o test_mo_column_extra_cpu.f90 -d=claw -t=cpu mo_column_extra.f90
	${claw} -o test_main_cpu.f90 -d=claw -t=cpu main.f90
	${fc} -o ref.exe mo_column.f90 mo_column_extra.f90 main.f90 && ./ref.exe
	${fc} -o test.exe test_mo_column_cpu.f90 test_mo_column_extra_cpu.f90 test_main_cpu.f90 && ./test.exe


clean:
	rm -rf __xmod__ *.xmod *.mod test_* ref.exe test.exe org.exe

