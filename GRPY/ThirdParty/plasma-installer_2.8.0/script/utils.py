#! /usr/bin/env python
# -*- coding: utf-8 -*-

###
#
# @file utils.py
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

import os
import re
import shutil
import subprocess
import select


def writefile(fname, fill):
    """ writes the file fname with content fill """
    fp = open(fname,'w')
    fp.write(fill)
    fp.close()

def killfiles(lst):
    """ deletes a list of files """
    for i in lst:
        if(os.path.isfile(i)):
            os.remove(i)

def openPipe(command):

    pipe = None
    if hasattr(popen2, 'Popen3'):
        pipe   = popen2.Popen3(command, 1)
        input  = pipe.tochild
        output = pipe.fromchild
        err    = pipe.childerr
    else:
        (input, output, err) = os.popen3(command)

    return (input, output, err, pipe)

class NullFile:
    def write(self, s): pass
    def flush(self): pass

def runShellCommand(command, outfile=None):
    """ runs a shell command """
    if not outfile:
      outfile = NullFile()

    proc = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (out, err) = proc.communicate()
    return (out, err, proc.returncode)

def getURLName(url):
    directory=os.curdir

    name="%s%s%s" % (
        directory,
        os.sep,
        url.split("/")[-1]
    )

    return name



def downloader(uri,cmd):
    """ downloads the content of an URL """

    savedir = os.getcwd()
    downdir = savedir+"/download"
    if(not os.path.isdir(downdir)):
        print"Creating directory", downdir
        os.mkdir(downdir)
    os.chdir(downdir)

    name = getURLName(uri)
    try:
        if(os.path.isfile(downdir+"/"+name)):
            print "Package "+name+" already downloaded"
        elif(cmd == 'urllib2'):
            import urllib2
            url = urllib2.urlopen(uri)
            f = open(name,'w')
            for line in url.readlines():
                f.write(line)
            url.close()
            f.close()
        elif(cmd == 'wget'):
            comm = 'wget '+uri
            (output, error, retz) = runShellCommand(comm)
        else:
            raise
    except:
        print " "
        print "================================================================================="
        print "ERROR: Cannot download "+name
        print "Make sure the network is reachable."
        print "Packages may be downloaded with wget though a proxy; in order to"
        print "enable this feature it is enough the set the http_proxy environment"
        print "variable (read the wget man pages for more details)."
        print "If you still have troubles, you can manually download "+name+" from this URL:"
        print uri
        print "into the current directory:"
        print os.getcwd()
        print ""

    os.chdir(savedir)
    shutil.copy('download/'+name, './')


def fixpaths(inpath):

    lst = inpath.split(" ")

    outpath = ""

    if ((len(lst) == 1) and (inpath[0] != "download") and (inpath[0] != '-')):
        outpath = os.path.abspath(inpath)
        return outpath
    else:
        for i in lst:
            if re.search("^-L",i):
                p = "-L"+os.path.abspath(i[2:])
            else:
                p = i

            outpath = outpath+p+" "

    return outpath
