
#fc='ifort'
fc='gfortran'

CLAW_VER = 02252019
#CLAW_VER = 09212018
claw='/glade/p/cisl/asap/youngsun/opt/claw/${CLAW_VER}/bin/clawfc'

# main.f90
# mo_column_extra.f90
# mo_column.f90

org: clean
	${fc} -o org.exe mo_column.f90 mo_column_extra.f90 main.f90 && ./org.exe

cpu: clean
	${claw} -o test_mo_column_cpu.f90 -d=claw -t=cpu mo_column.f90
	${claw} -o test_mo_column_extra_cpu.f90 -d=claw -t=cpu mo_column_extra.f90
	${claw} -o test_main_cpu.f90 -d=claw -t=cpu main.f90
	${fc} -o test.exe test_mo_column_cpu.f90 test_mo_column_extra_cpu.f90 test_main_cpu.f90 && ./test.exe

ref: clean
	${fc} -o ref.exe ref_column_cpu.f90 ref_extra_cpu.f90 ref_main_cpu.f90 && ./ref.exe

clean:
	rm -rf *.o __xmod__ *.xmod *.mod test_* org.exe ref.exe test.exe
