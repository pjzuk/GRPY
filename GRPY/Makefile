linux:
	cd src/lapackGRPY; make -f Makefile all
	cd src/GRPY; make -f Makefile all

windows_x86:
	cd src/lapackGRPY; make -f Makefile.windows_x86 all
	cd src/GRPY; make -f Makefile.windows_x86 all

windows_x64:
	cd src/lapackGRPY; make -f Makefile.windows_x64 all
	cd src/GRPY; make -f Makefile.windows_x64 all

plasmaLinux:
	cd src/plasmaGRPY; make -f Makefile all

cleanLinux:
	cd src/lapackGRPY; make -f Makefile clean
	cd src/GRPY; make -f Makefile clean
	cd src/plasmaGRPY; make -f Makefile clean

clean_x86:
	cd src/lapackGRPY; make -f Makefile.windows_x86 clean
	cd src/GRPY; make -f Makefile.windows_x86 clean

clean_x64:
	cd src/lapackGRPY; make -f Makefile.windows_x64 clean
	cd src/GRPY; make -f Makefile.windows_x64 clean

clean:
	cd src/lapackGRPY; make -f Makefile clean
	cd src/GRPY; make -f Makefile clean
	cd src/plasmaGRPY; make -f Makefile clean
