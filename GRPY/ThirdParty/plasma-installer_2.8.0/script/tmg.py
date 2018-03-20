#! /usr/bin/env python
# -*- coding: utf-8 -*-

###
#
# @file tmg.py
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

class Tmg(framework.Framework):

    """ This class takes care of the libtmg from LAPACK installation. """
    def __init__(self, config, plasma):
        print "\n","="*40
        print "  libtmg installation/verification"
        print "="*40

        self.config  = config
        self.downcmd = plasma.downcmd
        self.prefix  = plasma.prefix
        self.plasma  = plasma
        self.lapversion = plasma.lapversion
        self.lapackurl  = "http://www.netlib.org/lapack/"+self.lapversion+".tgz"

        if self.plasma.downtmg:
            print "The netlib LAPACK will be downloaded to install libtmg.a"
            self.down_install_tmg()
        else:
            print """
The Lapack library you provided doesn't contain Matrix generation function
for PLASMA testings and timings.

The library is not needed in the case when testing is disabled
by means of the --notesting flag.

What do you want to do ?
    - d : download the netlib LAPACK and install the tmg library ONLY.
            (LAPACK library won't be installed)
    - q : quit to download and install manually the netlib LAPACK.
"""
            answer = raw_input(">[q] ")
            if answer == "d":
                self.down_install_tmg()
            else:
                sys.exit()


    def check_tmg(self):

        print "Checking if provided LAPACK works...",
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

        ldflg = self.config.cblaslib+' '+self.config.lapacklib+' '+self.config.blaslib+' '+self.config.ldflags_fc
        ccomm = self.config.fc+' -o tmpf '+'tmpf.f '+ldflg
        (output, error, retz) = runShellCommand(ccomm)

        if(retz != 0):
            if self.plasma.verbose:
                print '\n\nlibtmg: provided libtmg cannot be used! aborting...'
                print 'error is:\n','*'*40,'\n',ccomm,'\n',error,'\n','*'*40
            else:
                print "no"
            sys.exit()

        comm = './tmpf'
        (output, error, retz) = runShellCommand(comm)
        if(retz != 0):
            if self.plasma.verbose:
                print '\n\nlibtmg: provided libtmg cannot be used! aborting...'
                print 'error is:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
            else:
                print "no"
            sys.exit()

        killfiles(['tmpf.f','tmpf'])
        print 'yes'

        return 0;


    def down_install_tmg(self):

        print """
The libtmg from reference LAPACK library is being installed.
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
            print '\n\nlibtmg: cannot unzip '+self.lapversion+'.tgz'
            print 'stderr:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
            sys.exit()

        comm = 'tar xf '+self.lapversion+'.tar'
        (output, error, retz) = runShellCommand(comm)
        if retz:
            print '\n\nlibtmg: cannot untar '+self.lapversion+'.tar'
            print 'stderr:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
            sys.exit()
        os.remove(self.lapversion+'.tar')
        print 'done'

        # change to BLAS dir
        os.chdir(os.path.join(os.getcwd(), self.lapversion))

        # Write Makefile.in
        writefile('make.inc', """
# -*- Makefile -*-
####################################################################
#  LAPACK make include file.                                       #
#  LAPACK, Version 3.3.1                                           #
#  April 2009                                                      #
####################################################################
#
# See the INSTALL/ directory for more examples.
#
SHELL = /bin/sh
#
#  The machine (platform) identifier to append to the library names
#
PLAT = _LINUX
#
#  Modify the FORTRAN and OPTS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader
#  and desired load options for your machine.
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
# SECOND and DSECND will use a call to the Fortran standard INTERNAL FUNCTION CPU_TIME
TIMER    = INT_CPU_TIME
# If neither of this works...you can use the NONE value... In that case, SECOND and DSECND will always return 0
# TIMER     = NONE
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
BLASLIB      = """+self.config.cblaslib+"""
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
""")

        # compile and generate library
        print 'Compile and generate libtmg...',
        sys.stdout.flush()
        comm = self.make+' tmglib'
        (output, error, retz) = runShellCommand(comm)
        if retz:
            print "\n\nlintmg: cannot compile libtmg"
            print "stderr:\n","*"*40,"\n",comm,'\n',error,"\n","*"*40
            sys.exit()

        log = output+error

        # write the log on a file
        log = log+output+error
        fulllog = os.path.join(savecwd,'log/tmglog')
        writefile(fulllog, log)
        print 'Installation of libtmg.a successful.'
        print '(log is in ',fulllog,')'

        # move libcblas.a to the lib directory
        shutil.copy('libtmg.a',os.path.join(self.prefix,'lib/libtmg.a'))

        # set framework variables to point to the freshly installed BLAS library
        self.config.lapacklib = '-L'+os.path.join(self.prefix,'lib')+' -ltmg '+self.config.lapacklib

        os.chdir(savecwd)

        # Check if the installation is successful
        self.plasma.verbose = 1
        self.check_tmg()
        self.plasma.verbose = 0
