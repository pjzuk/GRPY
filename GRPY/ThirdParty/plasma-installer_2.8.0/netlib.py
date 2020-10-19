#! /usr/bin/env python
# -*- coding: utf-8 -*-

class Config:
  compiler    = "GNU"
  blasname    = "Unknown"
  blaslib     = ""                # the BLAS library
  cblaslib    = ""                # the CBLAS library
  lapacklib   = ""                # the Lapack library
  lapclib     = ""                # the Lapack C interface
  lapackinc   = ""                # the Lapack headers
  cc          = "cc"              # the C compiler for plasma
  fc          = "gfortran"        # the Fortran compiler for core_lapack
  ranlib      = ""                # Ranlib
  arflags     = "rc"              # ar flags
  ldflags_c   = ""                # loader flags when main program is in C
  ldflags_fc  = ""                # loader flags when main program is in Fortran
  ld_fcmain   = ""                # the option to link C main with fortran linker
  withf90     = 1                 # Compile the f90 interface
  ccflags     = "-O2"
  fcflags     = "-O2"
  noopt       = "-O0"
  lapinstalled = 0

  def __init__(self, version):
    self.version = version
