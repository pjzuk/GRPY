#! /usr/bin/env python
# -*- coding: utf-8 -*-

###
#
# @file lapcwrapper.py
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

class Lapcwrapper(framework.Framework):

    """ This class takes care of the LAPACK C interface installation. """
    def __init__(self, config, plasma):
        print "\n","="*40
        print "  LAPACK C interface installation/verification"
        print "="*40

        self.config   = config
        self.downlapc = plasma.downlapc
        self.downcmd  = plasma.downcmd
        self.mangling = plasma.mangling
        self.prefix   = plasma.prefix
        self.plasma   = plasma
        self.lapversion = plasma.lapversion
        self.lapackurl  = "http://www.netlib.org/lapack/"+self.lapversion+".tgz"

        if self.downlapc == 2:
            self.down_install_lapc()

        ret = self.check_lapc()
        if ret != 0:
            if self.downlapc == 1:
                self.down_install_lapc()
            else :
                print """
Please provide a working LAPACK C interface. If a LAPACK C interface is not
present on the system, the netlib LAPACK C interface can be automatically
downloaded and installed by adding the --downlapc flag.

What do you want to do ?
    - d : download automatically the netlib LAPACK C interface
    - q : quit to download and install manually a LAPACK C interface.
"""
                answer = raw_input(">[q] ")
                if answer == "d":
                    self.down_install_lapc()
                else:
                    sys.exit()

    def check_lapc(self):

        print "Checking if provided LAPACK C interface works...",
        # This function simply generates a C program
        # that contains few calls to LAPACK C interface routine and then
        # checks if compilation, linking and execution are succesful

        sys.stdout.flush()
        writefile('tmpc.c',"""
#include <lapacke.h>
int main(int argc, char*agv[]) {
  double eps;
  eps = LAPACKE_dlamch('e');
  LAPACKE_dlatms_work(0, 0, 0, 'A', NULL, 'A', NULL, 0, 0., 0., 0, 0, 'A', NULL, 0, NULL );
  return 0;
}
\n""")

        ccomm = self.config.cc+' '+self.config.ldflags_c+' -o tmpc.o -c tmpc.c '+self.config.lapackinc
        (output, error, retz) = runShellCommand(ccomm)
        if(retz != 0):
            if self.plasma.verbose:
                print '\n\nLAPCWRAPPER: provided LAPACK C interface cannot be used! aborting...'
                print 'error is:\n','*'*40,'\n',ccomm,'\n',error,'\n','*'*40
            else:
                print "no"
            return -1;

        ldflg = self.config.ld_fcmain+' '+self.config.ldflags_fc+' '+self.config.lapclib+' '+self.config.lapacklib+' '+self.config.blaslib+' -lm'
        ccomm = self.config.fc+' -o tmpc tmpc.o '+ldflg
        (output, error, retz) = runShellCommand(ccomm)
        if(retz != 0):
            if self.plasma.verbose:
                print '\n\nLAPCWRAPPER: provided LAPACK C interface cannot be used! aborting...'
                print 'error is:\n','*'*40,'\n',ccomm,'\n',error,'\n','*'*40
            else:
                print "no"
            return -1;

        comm = './tmpc'
        (output, error, retz) = runShellCommand(comm)
        if(retz != 0):
            if self.plasma.verbose:
                print '\n\nLAPCWRAPPER: provided LAPACK C interface cannot be used! aborting...'
                print 'error is:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
            else:
                print "no"
            return -1;

        killfiles(['tmpc.o','tmpc.c','tmpc'])
        print 'yes'

        return 0;


    def down_install_lapc(self):

        print """
The reference LAPACK C interface is being installed.
"""
        sys.stdout.flush()

        savecwd = os.getcwd()

        # creating the build,lib and log dirs if don't exist
        if not os.path.isdir(os.path.join(self.prefix,'lib')):
            os.mkdir(os.path.join(self.prefix,'lib'))

        if not os.path.isdir(os.path.join(self.prefix,'include')):
            os.mkdir(os.path.join(self.prefix,'include'))

        if not os.path.isdir(os.path.join(os.getcwd(),'log')):
            os.mkdir(os.path.join(os.getcwd(),'log'))

        if self.config.lapinstalled == 0:
            # Check if lapacke.tgz is already present in the working dir
            # otherwise download it
            if not os.path.isfile(os.path.join(os.getcwd(),getURLName(self.lapackurl))):
                print "Downloading reference LAPACK C interface...",
                downloader(self.lapackurl,self.downcmd)
                print "done"

            # unzip and untar
            print 'Unzip and untar reference LAPACK C interface...',
            comm = 'gunzip -f '+self.lapversion+'.tgz'
            (output, error, retz) = runShellCommand(comm)
            if retz:
                print '\n\nLAPCWRAPPER: cannot unzip '+self.lapversion+'.tgz'
                print 'stderr:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
                sys.exit()

            comm = 'tar xf '+self.lapversion+'.tar'
            (output, error, retz) = runShellCommand(comm)
            if retz:
                print '\n\nLAPCWRAPPER: cannot untar '+self.lapversion+'.tar'
                print 'stderr:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
                sys.exit()
            os.remove(self.lapversion+'.tar')
            print 'done'

            #Apply the patch to correct [sd]lantr
            print 'Apply patch on lapacke...',
            comm = '(cd '+self.lapversion+' && patch -p 0 < '+(os.path.join(savecwd,'../script/patch_lantr'))+')'
            (output, error, retz) = runShellCommand(comm)
            print 'done'
            
        # change to LAPACK C interface dir
        os.chdir(os.path.join(os.getcwd(), self.lapversion))

        # Write Makefile.in
        writefile('make.inc', """
# -*- Makefile generated by PLASMA installer -*-
####################################################################
#  LAPACK make include file.                                       #
#  LAPACK, Version 3.4.0                                           #
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
CFLAGS = """+self.config.ccflags+' '+self.mangling+"""
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
        print 'Compile and generate reference LAPACK C interface...',
        sys.stdout.flush()
        comm = self.make+' -j 4 lapackelib'
        (output, error, retz) = runShellCommand(comm)
        if retz:
            print "\n\nLAPCWRAPPER: cannot compile LAPACK C interface"
            print "stderr:\n","*"*40,"\n",comm,'\n',error,"\n","*"*40
            sys.exit()

        log = output+error

        # write the log on a file
        log = log+output+error
        fulllog = os.path.join(savecwd,'log/lapackcwrapperlog')
        writefile(fulllog, log)
        print 'Installation of reference LAPACK C interface successful.'
        print '(log is in ',fulllog,')'

        # move liblapacke.a to the lib directory
        shutil.copy('liblapacke.a',os.path.join(self.prefix,'lib/liblapacke.a'))

        # move headers to the include directory
        shutil.copy('LAPACKE/include/lapacke.h', os.path.join(self.prefix,'include/lapacke.h'))
        shutil.copy('LAPACKE/include/lapacke_config.h', os.path.join(self.prefix,'include/lapacke_config.h'))
        shutil.copy('LAPACKE/include/lapacke_utils.h', os.path.join(self.prefix,'include/lapacke_utils.h'))
        shutil.copy('LAPACKE/include/lapacke_mangling.h', os.path.join(self.prefix,'include/lapacke_mangling.h'))
        shutil.copy('LAPACKE/include/lapacke_mangling_with_flags.h', os.path.join(self.prefix,'include/lapacke_mangling_with_flags.h'))

        # set framework variables to point to the freshly installed BLAS library
        self.config.lapclib   = '-L'+os.path.join(self.prefix,'lib')+' -llapacke'
        self.config.lapackinc = '-I'+os.path.join(self.prefix,'include')
        os.chdir(savecwd)

        # Check if the installation is successful
        self.plasma.verbose = 1
        ret = self.check_lapc()
        self.plasma.verbose = 0
        if ret != 0 :
            sys.exit()
