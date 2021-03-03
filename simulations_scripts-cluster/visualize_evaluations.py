# -*- coding: utf-8 -*-
"""
Created on Tue Jan 26 15:22:18 2021

@author: gorizadeh
"""

import os
import itertools
import numpy as np
from matplotlib import pyplot as plt
import matplotlib.pylab as pylab
params = {#'legend.fontsize': 'x-large',
         # 'figure.figsize': (15, 5),
         'figure.titlesize': 20,
         'axes.labelsize':20,
         'axes.titlesize':20,
         'xtick.labelsize':20,
         'ytick.labelsize':20}

pylab.rcParams.update(params)
projectDir="C:/Users/gorizadeh/Documents/GitHub/ranger/"

# True model values
corelations=[0.5,0.05,0.8,0.8,0.05,0.5]
trueRsqr   =[0.604,0.604,0.604,0.901,0.903,0.902]
trueMSE    =[1.34,1.61,4.81,0.11,0.28,0.41]



def find_min_median(dataDict):
    mn = 1e20
    for k in dataDict.keys():
        v = dataDict[k]
        m = np.median(np.asarray(v))
        if(m<mn):
            mn = m
    return(mn)

def find_max_median(dataDict):
    mn = -1
    for k in dataDict.keys():
        v = dataDict[k]
        m = np.median(np.asarray(v))
        if(m>mn):
            mn = m
    return(mn)

def draw_box_plot(dataDict,missType,measure,missRatio,saveDir,trueVal,cor,rsqr):
    title=missType + "  (" +str(int(missRatio*100))+"%), Cor = "+str(cor)+", $R^2$ = "+str(rsqr)[:3]
    fig, ax = plt.subplots(figsize=(6.5,5.5),dpi=150)
    ax.title.set_text(title)
    x = range(1,len(dataDict.keys())+1)
    y = [trueVal]*len(dataDict.keys())
    ax.plot(x, y, '-', color="r",linewidth=2)
    ax.boxplot(dataDict.values())
    x = range(1,len(dataDict.keys())+1)
    y = [find_min_median(dataDict)]*len(dataDict.keys())
    ax.plot(x, y, '--', linewidth=1.5)
    
    ax.set_xticklabels(dataDict.keys(),rotation=-90)
    saveName=saveDir+os.sep+measure+"-"+missType + "-" +str(int(missRatio*100))+".png"
   # plt.show()
    plt.tight_layout()
    fig.savefig(saveName, dpi=fig.dpi)

def draw_box_plot_Rsqr(dataDict,missType,measure,missRatio,saveDir,trueVal,cor,rsqr):
    title=missType + "  (" +str(int(missRatio*100))+"%), Cor = "+str(cor)+", $R^2$ = "+str(rsqr)[:3]
    fig, ax = plt.subplots(figsize=(6.5,5.5),dpi=150)
    ax.title.set_text(title)
    x = range(1,len(dataDict.keys())+1)
    y = [trueVal]*len(dataDict.keys())
    ax.plot(x, y, '-', color="r",linewidth=2)
    
    ax.boxplot(dataDict.values())
    x = range(1,len(dataDict.keys())+1)
    y = [find_max_median(dataDict)]*len(dataDict.keys())
    ax.plot(x, y, '--', linewidth=1.5)
    
    ax.set_xticklabels(dataDict.keys(),rotation=-90)
    saveName=saveDir+os.sep+measure+"-"+missType + "-" +str(int(missRatio*100))+".png"
   # plt.show()
    plt.tight_layout()
    fig.savefig(saveName, dpi=fig.dpi)
    
def read_evaluation_file(fName):
    dataMSEtrain = dict()
    dataMSEtest = dict()
    dataRsqrTrain = dict()
    with open(fName,'r') as f:
        lines = f.readlines()
        i = 0
        for l in lines:
            i = i+1
            if i==1:
                continue
            l = l.rstrip()
            lParts = l.split(',')
            k = lParts[0][1:-1]
            dataMSEtrain[k]  = float(lParts[1])
            dataMSEtest[k]   = float(lParts[3])
            dataRsqrTrain[k] = float(lParts[2])
            
    return (dataMSEtrain,dataRsqrTrain,dataMSEtest)

def read_all_evaluation_files(evPath,missType,missRatio):
    fPath = evPath + missType 
    dataMSEtrain = dict({"m.":list(),"PCA":list(),"emp.":list(),"rand.":list(),\
                  "r.wT":list(),"r.wT.wF":list(),\
                  "m.N":list(),"m.N.wT":list(),\
                  "m.N.wT.wF":list()})
    dataMSEtest = dict({"m.":list(),"PCA":list(),"emp.":list(),"rand.":list(),\
                  "r.wT":list(),"r.wT.wF":list(),\
                  "m.N":list(),"m.N.wT":list(),\
                  "m.N.wT.wF":list()})
    dataRsqrTrain = dict({"m.":list(),"PCA":list(),"emp.":list(),"rand.":list(),\
                  "r.wT":list(),"r.wT.wF":list(),\
                  "m.N":list(),"m.N.wT":list(),\
                  "m.N.wT.wF":list()})
    # Name mapper   
    nm = {"median":"m.","PCA":"PCA","Ishwaran":"emp.","random":"rand.",\
                  "randomWeightedTree":"r.wT","randomWeightedTreeAndForest":"r.wT.wF",\
                  "medianAtNode":"m.N","medianAtNodeRandomWeightedTree":"m.N.wT",\
                  "medianAtNodeRandomWeightedTreeAndForest":"m.N.wT.wF"}
        
    for i in range(1,101):
        fName = fPath + os.sep + str(i)+'-'+str(missRatio)+'.txt'
        mseTr,rsqr,mseTe = read_evaluation_file(fName)
        for k in mseTr.keys():
            dataMSEtrain[nm[k]].append(mseTr[k])
            dataMSEtest[nm[k]].append(mseTe[k])
            dataRsqrTrain[nm[k]].append(rsqr[k])
            
    return(dataMSEtrain,dataRsqrTrain,dataMSEtest)


for i in range(6):
    print(i)
    dirName=str(i+2)+"-evaluation-cor"+str(corelations[i])+\
        "-Rsqr"+str(trueRsqr[i])[:3]
    dataDir = projectDir+dirName+"/evaluations/"
    saveDir =  projectDir+dirName+"/visualizing-evaluations/"
    print(dataDir)
        
    if(True):
        allMissingType=["MCAR","MAR","MNAR"]
        allMissingRatio=[0.05,0.25,0.5,0.75,0.9]
        
        for mt in allMissingType:
            for mr in allMissingRatio:
                mseTr,rsqr,mseTe = read_all_evaluation_files(dataDir,mt,mr)
                draw_box_plot(mseTr,mt,"trainMSE",mr,saveDir,trueMSE[i],corelations[i],trueRsqr[i])
                draw_box_plot(mseTe,mt,"testMSE",mr,saveDir,trueMSE[i],corelations[i],trueRsqr[i])
                draw_box_plot_Rsqr(rsqr,mt,"trainRsqr",mr,saveDir,trueRsqr[i],corelations[i],trueRsqr[i])
