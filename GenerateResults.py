
import os
import sys
import statistics 
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib as mpl, matplotlib.pyplot as plt
import os.path
from os import path
import random

from EvaluationUtil import *

plotGBar = True
plotTrend = True
Rank = False

BASE_DIR = os.getcwd()
baseInFolder = os.path.join(BASE_DIR, 'output')


def plotTrendLine(inputName, testName , outcomeName, allAxis, datasetName, gcount, minY=None, maxY=None, label=False, xlabel=False):


    fold_nbr = 5
    
    factor = 'FOLLOW_REC'
    methods = []
    
    startCount = gcount    
    
    perPop = [0.2, 0.4,  0.6, 0.8, 1.0]        
    tickLabel  = ['0.2','0.4', '0.6', '0.8', '1.0']  
    
    tickLabel = None
    
    method = 'IPOP'
    methods.append(method)
    prefileName = inputName + '_' + testName + '_' +  method + '_'  
    postfileName = ''
    fullResultFolder = baseInFolder + '/'+ method +'/' + inputName
    newMolde = genQiniDataML(fullResultFolder, fold_nbr, prefileName, postfileName, method, outcomeName)
    improvementModels = pd.DataFrame({})
    improvementModels = improvementModels.append(newMolde)
    improvementModels['uplift'] = improvementModels['uplift']* 100
    improvementModels['grUplift'] = improvementModels['grUplift']* 100

    if(label):
        title = datasetName
    else:
        title = None
    ixlabel = method if (xlabel) else None
    plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY,title=title, xlabel=ixlabel)
    startCount = startCount + 1
    
    
    method = 'UBCF'
    methods.append(method)
    prefileName = inputName + '_' + testName + '_' +  method + '_'    
    postfileName = ''
    fullResultFolder = baseInFolder + '/'+ method +'/' + inputName
    newMolde = genQiniDataML(fullResultFolder, fold_nbr, prefileName, postfileName, method, outcomeName)
    improvementModels = pd.DataFrame({})
    improvementModels = improvementModels.append(newMolde)
    improvementModels['uplift'] = improvementModels['uplift']* 100
    improvementModels['grUplift'] = improvementModels['grUplift']* 100     

    if(label):
        title = datasetName
    else:
        title = None
    ixlabel = method if (xlabel) else None
    plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY,title=title, xlabel=ixlabel)
    startCount = startCount + 1

    method = 'IBCF'
    methods.append(method)
    prefileName = inputName + '_' + testName + '_' +  method + '_'   
    postfileName = ''
    fullResultFolder = baseInFolder + '/'+ method +'/' + inputName
    newMolde = genQiniDataML(fullResultFolder, fold_nbr, prefileName, postfileName, method, outcomeName)
    improvementModels = pd.DataFrame({})
    improvementModels = improvementModels.append(newMolde)
    improvementModels['uplift'] = improvementModels['uplift']* 100
    improvementModels['grUplift'] = improvementModels['grUplift']* 100     
    if(label):
        title = datasetName
    else:
        title = None
    ixlabel = method if (xlabel) else None
    plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY,title=title, xlabel=ixlabel)
    startCount = startCount + 1

    method = 'IROI'
    methods.append(method)
    prefileName = inputName + '_' + testName + '_' +  method + '_'   
    postfileName = ''
    fullResultFolder = baseInFolder + '/'+ method +'/' + inputName
    newMolde = genQiniDataML(fullResultFolder, fold_nbr, prefileName, postfileName, method, outcomeName)
    improvementModels = pd.DataFrame({})
    improvementModels = improvementModels.append(newMolde)
    improvementModels['uplift'] = improvementModels['uplift']* 100
    improvementModels['grUplift'] = improvementModels['grUplift']* 100     
    if(label):
        title = datasetName
    else:
        title = None
    ixlabel = method if (xlabel) else None
    plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY,title=title, xlabel=ixlabel)
    startCount = startCount + 1    
    

        

        
def plotAUUCBar(inputName, testName, outcomeName, iaxis, datasetName, gcount, minY=None):


    fold_nbr = 5
    methods = []
    auucScores = []
    
    method = 'IPOP'
    methods.append(method)
    prefileName = inputName + '_' + testName + '_' +  method + '_'  
    postfileName = ''
    fullResultFolder = baseInFolder + '/'+ method +'/' + inputName 
    opiAuc = getAUUCTopGroup(fullResultFolder, fold_nbr, prefileName, postfileName, outcomeName, False)
    auucScores.append(opiAuc)
    print(method + ": " + str(opiAuc))    


    method = 'UBCF'
    methods.append(method)
    prefileName = inputName + '_' + testName + '_' +  method + '_'  
    postfileName = ''
    fullResultFolder = baseInFolder + '/'+ method +'/' + inputName 
    opiAuc = getAUUCTopGroup(fullResultFolder, fold_nbr, prefileName, postfileName, outcomeName, False)
    auucScores.append(opiAuc)
    print(method + ": " + str(opiAuc))   
    
    
    method = 'IBCF'
    methods.append(method)
    prefileName = inputName + '_' + testName + '_' +  method + '_'  
    postfileName = ''
    fullResultFolder = baseInFolder + '/'+ method +'/' + inputName 
    opiAuc = getAUUCTopGroup(fullResultFolder, fold_nbr, prefileName, postfileName, outcomeName, False)
    auucScores.append(opiAuc)
    print(method + ": " + str(opiAuc))   


    method = 'IROI'
    methods.append(method)
    prefileName = inputName + '_' + testName + '_' +  method + '_'  
    postfileName = ''
    fullResultFolder = baseInFolder + '/'+ method +'/' + inputName 
    opiAuc = getAUUCTopGroup(fullResultFolder, fold_nbr, prefileName, postfileName, outcomeName, False)
    auucScores.append(opiAuc)
    print(method + ": " + str(opiAuc))   

    
    grhData = pd.DataFrame({'CIMP': auucScores, 'Method': methods})
        
    colors = ['olive', 'lightsteelblue', 'steelblue', 'tab:orange']
    g = sns.barplot(x='Method', y='CIMP', data= grhData, order = ['IPOP', 'UBCF','IBCF','IROI'],
    palette = colors,
    ax = iaxis[gcount])
    
    g.patch.set_facecolor('w')
        
    g.set_xlabel('')
    if((gcount % 2) == 0):
        g.set_ylabel('AUUC', fontsize=15)
    else:
        g.set_ylabel('')

    g.tick_params(axis='both', which='major', labelsize=15)
    g.set_xticklabels(g.get_xticklabels(), rotation=45)
    
    g.spines['top'].set_visible(False)
    g.spines['right'].set_visible(False) 
    g.spines['left'].set_color('black')
    g.spines['bottom'].set_color('black')
    g.spines['left'].set_linewidth(1)
    g.spines['bottom'].set_linewidth(1)
    if(minY != None):
        g.set(ylim=(minY, None))
            
            
        
def generateRank(inputName, factors, outcomeName, datasetName):

    ncauses  = factors.copy()
    perPop = [0.25, 0.5,  0.75, 1.0]
    methods = []
    cfactors = []
    kendals = []
    spears = []
    
    column_names = ['Dataset', 'CT', 'TOT', 'TST',  'FT',  'CF', 'XL', 'DRL', 'PIR']
    res = pd.DataFrame(columns = column_names)

    
    for factor in ncauses:
    
        dictVal = {}
        
        if(inputName == 'adultML'):
            dictVal['Dataset'] = 'ACI'

        if(inputName == 'turnoverML'):
            dictVal['Dataset'] = 'TO'
            
        if(inputName == 'hrML'):
            dictVal['Dataset'] = 'HR'

            
        mtag = ''
        
        if(factor == 'bestFactor'):
            mtag = '_pFactor'
            
        method = 'CT'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear

        method = 'FT'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear

        method = 'TST'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear

        method = 'TOT'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear

        
        method = 'CF'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear


        method = 'Xlearner'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        method = 'XL'
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear


        method = 'DRL'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear
        
                    
        method = 'PIR'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        method = 'PIR'
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear
        
        q0 = pd.DataFrame(dictVal, index =[0]) 
        
        res = pd.concat([q0, res]).reset_index(drop = True)        
                
    return res


random.seed(20)

if(plotTrend == True):

    print('plotTrend')


   
    
    plt.clf()    
    figure, allAxis = plt.subplots(2, 4, figsize=(8,5),  sharey='row', dpi=300)    
    allAxis = allAxis.flatten()
    gcount = 0


    
    outcomeName = 'longTermCandidate'
    inputName = 'turnover'
    datasetName = 'turnover'
    minY = 0
    maxY = 40
    plotTrendLine(inputName, 'train', outcomeName, allAxis, datasetName, gcount, minY, maxY, xlabel=True)
    allAxis[gcount].set_ylabel('OCI', fontsize=20)
    gcount += 4   
    
    
    outcomeName = 'longTermCandidate'
    inputName = 'turnover'
    datasetName = 'turnover'
    minY = -5
    maxY = 18
    plotTrendLine(inputName, 'test', outcomeName, allAxis, datasetName, gcount, minY, maxY, xlabel=True)
    allAxis[gcount].set_ylabel('OCI', fontsize=20)
    gcount += 4
    
    

    figure.tight_layout(w_pad=0.2, h_pad=1.5)
    figure.subplots_adjust(top=0.9)
    appResults = os.path.join(baseInFolder, 'PerformanceEval')
    figure.patch.set_facecolor('w')
    plt.savefig(appResults + '/'  + inputName + '_Trend.png', dpi=300, facecolor = 'w')   
      
    
    
    plt.clf()    
    figure, allAxis = plt.subplots(2, 4, figsize=(8,5),  sharey='row', dpi=300)    
    allAxis = allAxis.flatten()
    gcount = 0
    
    
    outcomeName = 'prof'
    inputName = 'adult'
    datasetName = 'adult'
    minY = 0
    maxY = 50
    plotTrendLine(inputName, 'train', outcomeName, allAxis, datasetName, gcount, minY, maxY, xlabel=True)
    allAxis[gcount].set_ylabel('OCI', fontsize=20)
    gcount += 4   
    
    
    
    
    outcomeName = 'prof'
    inputName = 'adult'
    datasetName = 'adult'
    minY = 0
    maxY = 50
    plotTrendLine(inputName, 'test', outcomeName, allAxis, datasetName, gcount, minY, maxY, xlabel=True)
    allAxis[gcount].set_ylabel('OCI', fontsize=20)
    gcount += 4
    

    
    figure.tight_layout(w_pad=0.2, h_pad=1.5)
    figure.subplots_adjust(top=0.9)
    appResults = os.path.join(baseInFolder, 'PerformanceEval')
    figure.patch.set_facecolor('w')
    plt.savefig(appResults + '/'  + inputName + '_Trend.png', dpi=300, facecolor = 'w')  
    

    
    
if(plotGBar == True):

    print('plotGBarML')
        
        
   
    
    plt.clf()    
    figure, allAxis = plt.subplots(1, 2, figsize=(8,9),  sharey=False, dpi=300)    
    allAxis = allAxis.flatten()
    gcount = 0
    

    
    outcomeName = 'longTermCandidate'
    inputName = 'turnover'
    datasetName = 'turnover'
    minY = 0 #20
    plotAUUCBar(inputName, 'train', outcomeName, allAxis, datasetName, gcount, minY)
    gcount += 1  

    
    outcomeName = 'longTermCandidate'
    inputName = 'turnover'
    datasetName = 'turnover'
    minY = 0 #20
    plotAUUCBar(inputName, 'test', outcomeName, allAxis, datasetName, gcount, minY)
    gcount += 1  
    
    figure.tight_layout(pad=1.0)
    figure.patch.set_facecolor('w')

    appResults = os.path.join(baseInFolder, 'PerformanceEval')
    plt.savefig(appResults + '/'  + inputName + '_AUUC.png', dpi=300, facecolor = 'w')
    
    
    
    
    
    
    plt.clf()    
    figure, allAxis = plt.subplots(1, 2, figsize=(8,9),  sharey=False, dpi=300)    
    allAxis = allAxis.flatten()
    gcount = 0
    
    
    outcomeName = 'prof'
    inputName = 'adult'
    datasetName = 'adult'
    minY = 0 #20
    plotAUUCBar(inputName, 'train', outcomeName, allAxis, datasetName, gcount, minY)
    gcount += 1  
    
    

    
    outcomeName = 'prof'
    inputName = 'adult'
    datasetName = 'adult'
    minY = 0 #20
    plotAUUCBar(inputName, 'test', outcomeName, allAxis, datasetName, gcount, minY)
    gcount += 1  
    
    
    figure.tight_layout(pad=1.0)
    figure.patch.set_facecolor('w')

    appResults = os.path.join(baseInFolder, 'PerformanceEval')
    plt.savefig(appResults + '/'  + inputName + '_AUUC.png', dpi=300, facecolor = 'w')
    
    
    
    

    
    
if(Rank):

    
    outcomeName = 'Prof'
    inputName = 'adultML'
    factors = ['bestFactor']
    datasetName = 'Adult Income'
    res1 = generateRank(inputName, factors, outcomeName, datasetName)
    
    
    
    outcomeName = 'Long_emp_retention'
    inputName = 'turnoverML'
    factors = ['bestFactor']
    datasetName = 'Turnover'
    res2 = generateRank(inputName, factors, outcomeName, datasetName)

    
    outcomeName = 'time_spend_company'
    inputName = 'hrML'
    factors = ['bestFactor']
    datasetName = 'HR'
    res3 = generateRank(inputName, factors, outcomeName, datasetName)
    
    
    results = pd.concat([res1, res2, res3 ]).reset_index(drop = True)
    
    results.loc['Average'] = results.mean()
    
    results = results[["Dataset", "CT", "TOT", "TST", 
                       "FT", "CF", "XL", "DRL", "PIR"]]
    appResults = os.path.join(baseInFolder, 'PerformanceEval')
    results.to_csv(appResults + '/'  + 'Spearman.csv', index=False) 