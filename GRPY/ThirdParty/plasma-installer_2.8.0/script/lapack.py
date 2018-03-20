#! /usr/bin/env python
# -*- coding: utf-8 -*-

###
#
# @file lapack.py
#
#  PLASMA is a software package provided by Univ. of Tennessee,
#  Univ. of California Berkeley and Univ. of Colorado Denver
#
# @version 2.8.0
# @author Julie Langou
# @author Mathieu Faverge
# @date 2010-11-15
#
###

from utils import writefile, runShellCommand, killfiles, downloader, getURLName
import sys
import os
import shutil
import framework

class Lapack(framework.Framework):
    """ This class takes care of the LAPACK installation. """
    def __init__(self, config, plasma):
        print "\n","="*40
        print "  LAPACK installation/verification"
        print "="*40

        self.config     = config
        self.downlapack = plasma.downlapack
        self.downcmd    = plasma.downcmd
        self.prefix     = plasma.prefix
        self.plasma     = plasma
        self.lapversion = plasma.lapversion
        self.lapackurl  = "http://www.netlib.org/lapack/"+self.lapversion+".tgz"

        if self.downlapack == 2:
            self.down_install_lapack()

        ret = self.check_lapack()

        if ret != 0:
            if self.downlapack == 1:
                self.down_install_lapack()
            else:
                print """
Please provide a working LAPACK library. If a LAPACK library is not
present on the system, the netlib LAPACK library can be automatically
downloaded and installed by adding the --downlapack flag.
Most used BLAS implementations already include the LAPACK library as
MKL, ACML, Goto, Goto2 or ATLAS. If you want to use one of these
libraries, you just have to specify correctly the --blaslib option or
you can specify where is located your own LAPACK library by using the
--lapacklib option.

With LAPACK, PLASMA require also the tmglib from LAPACK which is only
included in MKL and netlib LAPACK (--lapacklib=-ltmg -llapack). For
other BLAS libraries, LAPACK will be automatically downloaded from
netlib.

The LAPACK library is not needed in the case where testing is disabled
by means of the --notesting flag.

What do you want to do ?
    - d : download the netlib LAPACK
    - q : quit and try with another BLAS library or define the
      lapacklib parameter.
                """
                answer = raw_input(">[q] ")
                if answer == "d":
                    self.down_install_lapack()
                else:
                    sys.exit()

    def check_lapack(self):

        print "Checking if provided LAPACK works...",
        # This function simply generates a C program
        # that contains few calls to LAPACK routine and then
        # checks if compilation, linking and execution are succesful

        sys.stdout.flush()
        writefile('tmpf.f',"""

      program ftest
      integer  N
      parameter (N = 1)
      double precision A(N, N), B(N)
      integer  I(N)
      integer  INFO
      B(:)   = 1
      A(:,:) = 2
      I(:)   = 0
      call cheevd( 'N', 'U', N, A, N, B, B, -1,
     $     B, -1, I, -1, INFO)
      stop
      end\n""")

        ldflg = self.config.lapacklib+' '+self.config.blaslib+' '+self.config.ldflags_fc+' -lm'
        ccomm = self.config.fc+' -o tmpf '+'tmpf.f '+ldflg
        (output, error, retz) = runShellCommand(ccomm)

        if(retz != 0):
            if self.plasma.verbose:
                print '\n\nLAPACK: provided LAPACK cannot be used! aborting...'
                print 'error is:\n','*'*40,'\n',ccomm,'\n',error,'\n','*'*40
            else:
                print "no"
            return -1;

        comm = './tmpf'
        (output, error, retz) = runShellCommand(comm)
        if(retz != 0):
            if self.plasma.verbose:
                print '\n\nLAPACK: provided LAPACK cannot be used! aborting...'
                print 'error is:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
            else:
                print "no"
            return -1;

        killfiles(['tmpf.f','tmpf'])
        print 'yes'

        print "Checking if provided LAPACK contains functions for test works...",
        # This function simply generates a C program
        # that contains few calls to LAPACK routine and then
        # checks if compilation, linking and execution are succesful

        sys.stdout.flush()
        writefile('tmpf.f',"""
      program ftest
        double precision D(1), A(1:1), B(2)
        integer          ISEED( 4 )
        integer          INFO
        B(1)   = 1

        do  I = 1, 4
            ISEED( I ) = 1
        enddo
        call dlarnv( 1, ISEED, 1, D )
        call dlagsy( 1, 0, D, A, 1, ISEED, B, INFO )
        stop
      end\n""")

        ccomm = self.config.fc+' -o tmpf '+'tmpf.f '+ldflg
        (output, error, retz) = runShellCommand(ccomm)

        if(retz != 0):
            print 'no'
            self.plasma.needtmg = 1
        else:
            comm = './tmpf'
            (output, error, retz) = runShellCommand(comm)
            if(retz != 0):
                print 'no'
                self.plasma.needtmg = 1;
            else:
                self.plasma.needtmg = 0;
                print 'yes'
        killfiles(['tmpf.f','tmpf'])

        return 0;


    def down_install_lapack(self):

        print """
The reference LAPACK library is being installed.
"""
        sys.stdout.flush()

        savecwd = os.getcwd()

        # creating the build,lib and log dirs if don't exist
        if not os.path.isdir(os.path.join(self.prefix,'lib')):
            os.mkdir(os.path.join(self.prefix,'lib'))

        if not os.path.isdir(os.path.join(os.getcwd(),'log')):
            os.mkdir(os.path.join(os.getcwd(),'log'))

        # Check if lapack.tgz is already present in the working dir
        # otherwise download it
        if not os.path.isfile(os.path.join(os.getcwd(),getURLName(self.lapackurl))):
            print "Downloading reference LAPACK...",
            downloader(self.lapackurl,self.downcmd)
            print "done"

        # unzip and untar
        print 'Unzip and untar reference BLAS...',
        comm = 'gunzip -f '+self.lapversion+'.tgz'
        (output, error, retz) = runShellCommand(comm)
        if retz:
            print '\n\nLAPACK: cannot unzip '+self.lapversion+'.tgz'
            print 'stderr:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
            sys.exit()

        comm = 'tar xf '+self.lapversion+'.tar'
        (output, error, retz) = runShellCommand(comm)
        if retz:
            print '\n\nLAPACK: cannot untar '+self.lapversion+'.tar'
            print 'stderr:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
            sys.exit()
        os.remove(self.lapversion+'.tar')
        print 'done'

        #Apply the patch to correct [sd]lantr
        print 'Apply patch on lapacke...',
        comm = '(cd '+self.lapversion+' && patch -p 0 < '+(os.path.join(savecwd,'../script/patch_lantr'))+')'
        (output, error, retz) = runShellCommand(comm)
        print 'done'

#         # Overwrite [sd]lamch.f
#         shutil.copy(os.path.join(self.plasma.installerdir,'src/dlamch.f'),
#                     os.path.join(os.getcwd(),'lapack-3.3.1/INSTALL'))
#         shutil.copy(os.path.join(self.plasma.installerdir,'src/slamch.f'),
#                     os.path.join(os.getcwd(),'lapack-3.3.1/INSTALL'))

        # change to BLAS dir
        os.chdir(os.path.join(os.getcwd(), self.lapversion))

        # Write Makefile.in
        writefile('make.inc', """
# -*- Makefile generated by PLASMA installer -*-
####################################################################
#  LAPACK make include file.                                       #
#  LAPACK, Version """+self.lapversion+""""                                           #
#  April 2012                                                      #
####################################################################
#
SHELL = /bin/sh
#
#  Modify the FORTRAN and OPTS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader and
#  desired load options for your machine.
#
FORTRAN  = """+self.config.fc+"""
OPTS     = """+self.config.fcflags+"""
DRVOPTS  = $(OPTS)
NOOPT    = """+self.config.noopt+"""
LOADER   = """+self.config.fc+"""
LOADOPTS = """+self.config.ldflags_fc+"""
MAKE     = make -j 8
#
# Timer for the SECOND and DSECND routines
#
# Default : SECOND and DSECND will use a call to the EXTERNAL FUNCTION ETIME
# TIMER    = EXT_ETIME
# For RS6K : SECOND and DSECND will use a call to the EXTERNAL FUNCTION ETIME_
# TIMER    = EXT_ETIME_
# For gfortran compiler: SECOND and DSECND will use a call to the INTERNAL FUNCTION ETIME
# TIMER    = INT_ETIME
# If your Fortran compiler does not provide etime (like Nag Fortran Compiler, etc...)
# SECOND and DSECND will use a call to the INTERNAL FUNCTION CPU_TIME
TIMER    = INT_CPU_TIME
# If neither of this works...you can use the NONE value... In that case, SECOND and DSECND will always return 0
# TIMER     = NONE
#
#  Configuration LAPACKE: Native C interface to LAPACK
#  To generate LAPACKE library: type 'make lapackelib'
#  Configuration file: turned off (default)
#  Complex types: C99 (default)
#  Name pattern: mixed case (default)
#  (64-bit) Data model: LP64 (default)
#
# CC is the C compiler, normally invoked with options CFLAGS.
#
CC     = """+self.config.cc+"""
CFLAGS = """+self.config.ccflags+"""
#
#  The archiver and the flag(s) to use when building archive (library)
#  If you system has no ranlib, set RANLIB = echo.
#
ARCH     = ar
ARCHFLAGS= """+self.config.arflags+"""
RANLIB   = """+self.config.ranlib+"""
#
#  The location of BLAS library for linking the testing programs.
#  The target's machine-specific, optimized BLAS library should be
#  used whenever possible.
#
BLASLIB      = """+self.config.blaslib+"""
#
#  Location of the extended-precision BLAS (XBLAS) Fortran library
#  used for building and testing extended-precision routines.  The
#  relevant routines will be compiled and XBLAS will be linked only if
#  USEXBLAS is defined.
#
# USEXBLAS    = Yes
XBLASLIB     =
# XBLASLIB    = -lxblas
#
#  Names of generated libraries.
#
LAPACKLIB    = liblapack.a
TMGLIB       = libtmg.a
EIGSRCLIB    = libeigsrc.a
LINSRCLIB    = liblinsrc.a
LAPACKELIB   = liblapacke.a
""")

        # compile and generate library
        print 'Compile and generate LAPACK...',
        sys.stdout.flush()
        comm = self.make+' lapacklib tmglib'
        (output, error, retz) = runShellCommand(comm)
        if retz:
            print "\n\nLAPACK: cannot compile LAPACK"
            print "stderr:\n","*"*40,"\n",comm,'\n',error,"\n","*"*40
            sys.exit()

        log = output+error

        # write the log on a file
        log = log+output+error
        fulllog = os.path.join(savecwd,'log/lapacklog')
        writefile(fulllog, log)
        print 'Installation of liblapack.a successful.'
        print '(log is in ',fulllog,')'

        # move libcblas.a to the lib directory
        shutil.copy('liblapack.a',os.path.join(self.prefix,'lib/liblapack.a'))
        shutil.copy('libtmg.a',os.path.join(self.prefix,'lib/libtmg.a'))

        # set framework variables to point to the freshly installed BLAS library
        self.config.lapacklib  = '-L'+os.path.join(self.prefix,'lib')+' -ltmg -llapack'
        os.chdir(savecwd)

        self.config.lapinstalled = 1;

        # Check if the installation is successful
        self.plasma.verbose = 1
        ret = self.check_lapack()
        self.plasma.verbose = 0
        if ret != 0:
            sys.exit()
