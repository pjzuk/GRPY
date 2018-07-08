#!/usr/bin/env python

#python 2.7

from pylab import *
from scipy import *
import numpy as np
import pylab
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
from sys import argv
from scipy.optimize import leastsq
import scipy.stats as stats
from scipy import integrate

def round(number):
    if type(number) is list:
        return "%.2g" % number[0] + "$\pm$" +  "%.2g" % number[1]
    else:
        return "%.2g" % number

def roundFloat(number):
    return float("%.2g" % number)


for fname in argv[1:]:

    fileIn = open(fname,'r')
 
    carridgeList=[]
    carridge=[None]*21
  
    for line in fileIn:
        words=line.split()
        if words[0]=='molecule':
            carridgeList.append(carridge)
            carridge=[None]*21
            carridge[1]=words[1].replace("dim","").replace("Mon","")
            carridge[2]=float(words[2])
        elif words[0]=='huReadName':
            carridge[0]=' '.join(words[1:])
        elif words[0]=='refs':
            carridge[4]=words[1:]
        elif len(words)==1:
            if float(words[0]) < 1.e-3:
                carridge[3]=float(words[0])*1.e+7
            else:
                carridge[3]=float(words[0])
        elif len(words) > 3:
            if words[0]=='GRPY':
                if words[1]=='A10R30syOThyG2' or words[1]=='OThyG2':
                    carridge[7]=float(words[3])
#                if words[1]=='A10R30hyG2':
#                    carridge[7]=float(words[3])
                if words[1]=='A10R30syOThyG5' or words[1]=='OThyG5':
                    carridge[10]=float(words[3])
#                if words[1]=='A10R30hyG5':
#                    carridge[11]=float(words[3])
                if words[1]=='A20R50hiOT':
                    carridge[13]=float(words[3])
                if words[1]=='A20R50':
                    carridge[15]=float(words[3])
                if (words[1].split('_'))[-1] == 'output' and ((words[1].split('_'))[0])[-2:] == 'HR':
                    carridge[20]=float(words[3])
            if words[0]=='SMI':
                if words[1]=='A10R30syOThyG2' or words[1]=='OThyG2':
                    carridge[5]=float(words[3])
                if words[1]=='A10R30syOThyG5' or words[1]=='OThyG5':
                    carridge[8]=float(words[3])
                if words[1]=='A20R50hiOT':
                    carridge[11]=float(words[3])
            if words[0]=='HP':
                carridge[16]=float(words[3])
            if words[0]=='hpp':
                carridge[18]=float(words[3])
            if words[0]=='zno':
                if len(words[1].split('-')) > 1:
                    if (words[1].split('-'))[1]=='A10R30syOThyG2' or (words[1].split('-'))[1]=='OThyG2':
                        carridge[6]=float(words[3])
                    if (words[1].split('-'))[1]=='A10R30syOThyG5' or (words[1].split('-'))[1]=='OThyG5':
                        carridge[9]=float(words[3])
                    if (words[1].split('-'))[1]=='A20R50hiOT' or (words[1].split('-'))[1]=='hiOT':
                        carridge[12]=float(words[3])
                    if (words[1].split('-'))[1]=='A20R50' or (words[1].split('-'))[1]=='ovlp':
                        carridge[14]=float(words[3])
                elif (words[1].split('_'))[-1] =='shell':
                    carridge[19]=float(words[3])
            if words[0]=='BEST':
                carridge[17]=float(words[3])

    carridgeList.append(carridge)
    del carridgeList[0]
    carridgeList.sort(key=lambda x: x[2])
    carridgeListTr = map(list, zip(*carridgeList))
    names=set(carridgeListTr[0])
    reducedCarridgeList = []
    for name in names:
      count=0
      for ent in carridgeList:
        if ent[0] == name:
          if count == 0:
            entry = []
            for ei in ent:
              entry.append(ei)
            count += 1
          else:
            for i in range(5,len(ent)):
              if (not ent[i] == None) and (not entry[i] == None):
                entry[i] = entry[i] + ent[i]
            count += 1
      if count > 1:
        for i in range(5,len(ent)):
          if not entry[i] == None:
            entry[i] = entry[i]/float(count)
        print entry
      reducedCarridgeList.append(entry)
    carridgeListTr = map(list, zip(*reducedCarridgeList))
    carridge=[None]*21
    carridge2=[None]*21
    for ind in range(5,len(carridgeListTr)):
        column = carridgeListTr[ind]
        val=0
        valM=0
        valSDM=0
        n=0
        for indIns in range(0,len(column)):
            ent = column[indIns]
            name = carridgeListTr[0][indIns]
            weight=1./float(carridgeListTr[0].count(name))
            if not ent == None:
                val+=weight*roundFloat(ent)**2
                valM+=weight*roundFloat(ent)
                n+=1*weight
        if n > 1:
           valM = valM/float(n)
           carridge[ind] = ((val/float(n))**0.5)
           for indIns in range(0,len(column)):
               ent = column[indIns]
               name = carridgeListTr[0][indIns]
               weight=1./float(carridgeListTr[0].count(name))
               if not ent == None:
                   valSDM+=weight*(ent-valM)**2
           valSDM=(valSDM/float(n)/(n-1.))**0.5           
           carridge2[ind] = [valM,valSDM]
        else:
           carridge[ind] = 0
           carridge2[ind] = [0,0]
    carridgeList.append(carridge)
    carridgeList.append(carridge2)

    for carridge in carridgeList:
        for ind in range(0,len(carridge)-1):
            item = carridge[ind]
            if ind==2:
                if item==None:
                    print "    & ",
                else:
                    print "%.4g" % item," & ",
            elif ind==4:
                if not item==None:
                    sys.stdout.write("\cite{")
                    for i in range(0,len(item)-1):
                        sys.stdout.write(item[i])
                        sys.stdout.write(",")
                    sys.stdout.write(item[len(item)-1])
                    print "}&",
                else:
                    print "    & ",
            elif item==None:
                print "      & ",
            elif type(item) is str:
                print item," & ",
            elif ind in [16]:
                print "\multicolumn{1}{|c|}{",round(item),"} & ",
            elif ind in [5,8,11,14,18]:
                print "\multicolumn{1}{|c}{",round(item),"} & ",
            else:
                print "\multicolumn{1}{c}{",round(item),"} & ",
        item = carridge[len(carridge)-1]
        if item==None:
            print "\\""\\"
        else:
            print round(item), "\\""\\"

    for c in carridgeList[:-2]:
      toterr=0
      x=[]
      for f in c[5:21]:
        if not f == -983: #c[14]:
          toterr=toterr + f**2
          x.append(abs(f))
      print c[1],(toterr/(len(c[5:16])-1))**0.5,np.median(x)


    fileIn.close()
