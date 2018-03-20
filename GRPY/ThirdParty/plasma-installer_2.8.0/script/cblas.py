#! /usr/bin/env python
# -*- coding: utf-8 -*-

###
#
# @file cblas.py
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

class CBlas(framework.Framework):
    cblasurl     = "http://www.netlib.org/blas/blast-forum/cblas.tgz"

    """ This class takes care of the CBLAS installation. """
    def __init__(self, config, plasma):
        print "\n","="*40
        print "  CBLAS installation/verification"
        print "="*40

        self.config    = config
        self.downcblas = plasma.downcblas
        self.downcmd   = plasma.downcmd
        self.mangling  = plasma.mangling
        self.prefix    = plasma.prefix
        self.plasma    = plasma

        if self.downcblas == 2:
            self.down_install_cblas()

        ret = self.check_cblas()
        if ret != 0:
            if self.downcblas == 1:
                self.down_install_cblas()
            else :
                print """
Please provide a working CBLAS library. If a CBLAS library is not
present on the system, the netlib CBLAS library can be automatically
downloaded and installed by adding the --downcblas flag.
Most used BLAS implementations already include a CBLAS interface as
MKL, ACML, Goto, Goto2 or ATLAS. If you want to use one of these
libraries, you just have to specify correctly the --blaslib option or
you can specify where is located your own CBLAS library by using the
--cblaslib option.

The CBLAS library is not needed in the case where testing is disabled
by means of the --notesting flag.

What do you want to do ?
    - d : download the netlib cblas
    - q : quit and try with another BLAS library or define the
      cblaslib parameter.
"""
                answer = raw_input(">[q] ")
                if answer == "d":
                    self.down_install_cblas()
                else:
                    sys.exit()

    def check_cblas(self):

        print "Checking if provided CBLAS works...",
        # This function simply generates a C program
        # that contains few calls to CBLAS routine and then
        # checks if compilation, linking and execution are succesful

        sys.stdout.flush()
        writefile('tmpc.c',"""
int main(int argc, char*agv[]) {
  double da    = 2.0;
  double dx[2] = {1.0, 2.0};
  cblas_dscal(2, da, dx, 1);
  return 0;
}
\n""")

        ccomm = self.config.cc+' -o tmpc.o '+'-c tmpc.c '+self.config.ccflags
        (output, error, retz) = runShellCommand(ccomm)

        if(retz != 0):
            if self.plasma.verbose:
                print '\n\nCBLAS: provided CBLAS cannot be used! aborting...'
                print 'error is:\n','*'*40,'\n',ccomm,'\n',error,'\n','*'*40
            else:
                print "no"
            return -1;

        ccomm = self.config.fc+' -o tmpc '+'tmpc.o '+self.config.fcflags+' '+self.config.cblaslib+' '+self.config.blaslib+' '+self.config.ldflags_fc+' '+self.config.ld_fcmain+' -lm'
        (output, error, retz) = runShellCommand(ccomm)

        if(retz != 0):
            if self.plasma.verbose:
                print '\n\nCBLAS: provided CBLAS cannot be used! aborting...'
                print 'error is:\n','*'*40,'\n',ccomm,'\n',error,'\n','*'*40
            else:
                print "no"
            return -1;

        comm = './tmpc'
        (output, error, retz) = runShellCommand(comm)
        if(retz != 0):
            if self.plasma.verbose:
                print '\n\nCBLAS: provided CBLAS cannot be used! aborting...'
                print 'error is:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
            else:
                print "no"
            return -1;

        killfiles(['tmpc.o', 'tmpc.c','tmpc'])
        print 'yes'

        return 0;


    def down_install_cblas(self):

        print """
The reference CBLAS library is being installed.
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

        # Check if cblas.tgz is already present in the working dir
        # otherwise download it
        if not os.path.isfile(os.path.join(os.getcwd(),getURLName(self.cblasurl))):
            print "Downloading reference CBLAS...",
            downloader(self.cblasurl,self.downcmd)
            print "done"

        # unzip and untar
        print 'Unzip and untar reference CBLAS...',
        comm = 'gunzip -f cblas.tgz'
        (output, error, retz) = runShellCommand(comm)
        if retz:
            print '\n\nCBLAS: cannot unzip cblas.tgz'
            print 'stderr:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
            sys.exit()

        comm = 'tar xf cblas.tar'
        (output, error, retz) = runShellCommand(comm)
        if retz:
            print '\n\nCBLAS: cannot untar cblas.tgz'
            print 'stderr:\n','*'*40,'\n',comm,'\n',error,'\n','*'*40
            sys.exit()
        os.remove('cblas.tar')
        print 'done'

        # change to BLAS dir
        os.chdir(os.path.join(os.getcwd(),'CBLAS'))

        # Write Makefile.in
        writefile('Makefile.in', """
# Makefile.in (Plasma Installer)
#
#-----------------------------------------------------------------------------
# Shell
#-----------------------------------------------------------------------------
SHELL = /bin/sh

#-----------------------------------------------------------------------------
# Platform
#-----------------------------------------------------------------------------

PLAT = """+self.plat+"""

#-----------------------------------------------------------------------------
# Libraries and includs
#-----------------------------------------------------------------------------

BLLIB = """+self.config.blaslib+"""
CBDIR = """+os.getcwd()+"""
CBLIBDIR = $(CBDIR)/lib
CBLIB = $(CBLIBDIR)/libcblas.a

#-----------------------------------------------------------------------------
# Compilers
#-----------------------------------------------------------------------------

CC = """+self.config.cc+"""
FC = """+self.config.fc+"""
LOADER = $(FC)

#-----------------------------------------------------------------------------
# Flags for Compilers
#-----------------------------------------------------------------------------

CFLAGS = """+self.config.ccflags+""" """+self.mangling+"""
FFLAGS = """+self.config.fcflags+"""

#-----------------------------------------------------------------------------
# Archive programs and flags
#-----------------------------------------------------------------------------

ARCH      = ar
ARCHFLAGS = """+self.config.arflags+"""
RANLIB    = """+self.config.ranlib+"""
""")

        # compile and generate library
        print 'Compile and generate reference CBLAS...',
        sys.stdout.flush()
        comm = self.make+' alllib'
        (output, error, retz) = runShellCommand(comm)
        if retz:
            print "\n\nCBLAS: cannot compile cblas"
            print "stderr:\n","*"*40,"\n",comm,'\n',error,"\n","*"*40
            sys.exit()

        log = output+error

        # write the log on a file
        log = log+output+error
        fulllog = os.path.join(savecwd,'log/cblaslog')
        writefile(fulllog, log)
        print 'Installation of reference CBLAS successful.'
        print '(log is in ',fulllog,')'

        # move libcblas.a to the lib directory
        shutil.copy('lib/libcblas.a',os.path.join(self.prefix,'lib/libcblas.a'))

        # move cblas.h to the include directory
        shutil.copy('include/cblas.h',os.path.join(self.prefix,'include/cblas.h'))

        # set framework variables to point to the freshly installed BLAS library
        self.config.cblaslib  = '-L'+os.path.join(self.prefix,'lib')+' -lcblas'
        os.chdir(savecwd)

        # Check if the installation is successful
        self.plasma.verbose = 1;
        ret = self.check_cblas()
        self.plasma.verbose = 0;
        if ret != 0 :
            sys.exit()
