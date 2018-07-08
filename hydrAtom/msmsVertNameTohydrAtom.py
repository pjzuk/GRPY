from pylab import *
from scipy import *
import sys


                # polar + ionic residues of aminoacid by Knutz 1974

dictWaters = {'ALA': 1,
              'CYS': 0, 
              'ASP': 6,   #ionic
              'GLU': 7,   #ionic
              'PHE': 0, 
              'GLY': 1,   
              'HIS': 4,   #ionic
              'ILE': 1, 
              'LYS': 4,   #ionic
              'LEU': 1, 
              'MET': 1, 
              'ASN': 2, 
              'PRO': 3, 
              'GLN': 2, 
              'ARG': 3,   #ionic
              'SER': 2, 
              'THR': 2, 
              'VAL': 1, 
              'TRP': 2, 
              'TYR': 3,   #polar
              'PCA': 0, 
              'GAL': 3, 
              'MAN': 3, 
              'NAG': 3, 
              'SIA': 9, 
              'FUC': 2, 
              'OG1': 3, 
              'OG2': 0, 
              'OG3': 0, 
              'OXT': 5}

volH2O = 24 # A^3 is water partcile volume by Gerstein and Chotia 1996

# program takes as an argument msms program file .vert calculated from .xyzrn file (including atom names)
# both should be in the same folder

for fname in sys.argv[1:]:

    msmsFile = open(fname,'r')
    line = msmsFile.readline()
    a = line.split()
    xyzrFileName = a[7]
   
    line = msmsFile.readline()
    line = msmsFile.readline()
    a = line.split()
    nLines = int(a[0])

    outerAtomList = []
    for i in range(0,nLines):
        line = msmsFile.readline()
        a = line.split()
        atomNumber = int(a[7])
        if not atomNumber in outerAtomList:
            outerAtomList.append(atomNumber)
    
    msmsFile.close()
    xyzrFile = open(xyzrFileName,'r')

    lineN=1
    surfaceAtomList=[]
    for line in xyzrFile:
        a = line.split()
        if lineN in outerAtomList:
            name=a[5].split('_')
            surfaceAtomList.append([float(a[0]),float(a[1]),float(a[2]),float(a[3]),name[0],name[1],int(name[2])])
        lineN += 1

    atomOld=surfaceAtomList[0]
    residue=[]
    residueList=[]
    for atom in surfaceAtomList:
        if atom[5]==atomOld[5]:
            residue.append(atom)
            atomOld=atom
        if not atom[5]==atomOld[5]:
            atomOld=atom
            residueList.append(residue)
            residue=[]
            residue.append(atom)
    residueList.append(residue)

    totalHydrationVolume = 0
        
    for residue in residueList:
        atomCount=len(residue)
        if atomCount > 0:
            totalVolume = 0
            for atom in residue:
                totalVolume += 4./3.*pi*(atom[3]**3)
            try:
                hydrationVolume=volH2O*dictWaters[atom[5]]
                totalHydrationVolume+=hydrationVolume
            except:
                sys.stderr.write('h-atom warning: '+str(atom[5])+' not in dictionary\n')
                sys.stderr.write('                 atoms in this residue did not recieve hydration layer\n')
                hydrationVolume = 0
            unitHydrationVolume = hydrationVolume/atomCount
            for atom in residue:
                hydLayCoeff=6.375 #the coefficient of hydration layer 6.375 - approx. all hydration volume on the surface.
                atom[3] = ((atom[3]**3) + hydLayCoeff*unitHydrationVolume/(4./3.*pi))**(1./3.) 
                print atom[0],atom[1],atom[2],atom[3]

    sys.stderr.write('h-atom info: total hydration volume '+str(totalHydrationVolume)+' A^3\n')
   
    xyzrFile.close()
