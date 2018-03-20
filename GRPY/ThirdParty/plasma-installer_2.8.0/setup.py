#! /usr/bin/env python
# -*- coding: utf-8 -*-

###
#
# @file setup.py
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

import sys

VERSION_MAJOR = 2
VERSION_MINOR = 8
VERSION_MICRO = 0

from script.blas        import Blas
from script.cblas       import CBlas
from script.lapack      import Lapack
from script.tmg         import Tmg
from script.lapcwrapper import Lapcwrapper
from script.plasma      import Plasma

import netlib

def main(argv):

  ### Store history of executed commands in config.log
  cmd = ""
  for arg in argv:
      cmd += arg+" "
  cmd += "\n"
  fp = open("history.log",'a')
  fp.write(cmd)
  fp.close()
  ### END

  config = netlib.Config((VERSION_MAJOR, VERSION_MINOR, VERSION_MICRO))

  plasma = Plasma(argv, config)

  if plasma.testing or plasma.src or plasma.downblas :
    Blas(config, plasma)

  if plasma.testing or plasma.src or plasma.downcblas :
    CBlas(config, plasma)

  if plasma.testing or plasma.src or plasma.downlapack :
    Lapack(config, plasma)

  # plasma.downtmg set to 1 by lapack if necessary
  if plasma.needtmg :
    Tmg(config, plasma)

  # Always required for the lapack.h
  Lapcwrapper(config, plasma)

  plasma.resume()

  return 0

if "__main__" == __name__:
  sys.exit(main(sys.argv))
