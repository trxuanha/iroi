
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib as mpl, matplotlib.pyplot as plt
import os.path
from os import path
from scipy import stats
import statistics
        
       
def estimateQiniCurve(estimatedImprovements, outcomeName, modelName):

    ranked = pd.DataFrame({})
    ranked['uplift_score'] = estimatedImprovements['Improvement']
    ranked['NUPLIFT'] = estimatedImprovements['UPLIFT']
    ranked['FollowRec'] = estimatedImprovements['FollowRec']
    ranked[outcomeName] = estimatedImprovements[outcomeName]
    ranked['countnbr'] = 1
    ranked['n'] = ranked['countnbr'].cumsum() / ranked.shape[0]
    uplift_model, random_model = ranked.copy(), ranked.copy()
    C, T = sum(ranked['FollowRec'] == 0), sum(ranked['FollowRec'] == 1)
    ranked['CR'] = 0
    ranked['TR'] = 0
    ranked.loc[(ranked['FollowRec'] == 0)
                            &(ranked[outcomeName]  == 1),'CR'] = ranked[outcomeName]
    ranked.loc[(ranked['FollowRec'] == 1)
                            &(ranked[outcomeName]  == 1),'TR'] = ranked[outcomeName]
    ranked['NotFollowRec'] = 1
    ranked['NotFollowRec'] = ranked['NotFollowRec']  - ranked['FollowRec'] 
    ranked['NotFollowRecCum'] = ranked['NotFollowRec'].cumsum() 
    ranked['FollowRecCum'] = ranked['FollowRec'].cumsum() 
    ranked['CR/C'] = ranked['CR'].cumsum() / ranked['NotFollowRec'].cumsum()    
    ranked['TR/T'] = ranked['TR'].cumsum() / ranked['FollowRec'] .cumsum()
    # Calculate and put the uplift into dataframe
    uplift_model['uplift'] = round((ranked['TR/T'] - ranked['CR/C'])*ranked['n'] ,5)
    uplift_model['uplift'] = round((ranked['NUPLIFT'])*ranked['n'] ,5)
    uplift_model['grUplift'] =ranked['NUPLIFT']
    random_model['uplift'] = round(ranked['n'] * uplift_model['uplift'].iloc[-1],5)
    ranked['uplift']  = ranked['TR/T'] - ranked['CR/C']
    # Add q0
    q0 = pd.DataFrame({'n':0, 'uplift':0}, index =[0])
    uplift_model = pd.concat([q0, uplift_model]).reset_index(drop = True)
    random_model = pd.concat([q0, random_model]).reset_index(drop = True)  
    # Add model name & concat
    uplift_model['model'] = modelName
    random_model['model'] = 'Random model'
    return uplift_model
    
def plotBarArea(model):
    plt.clf()
    ax = sns.barplot(x='Model', y='Area',  data= model)
    sns.set_style('whitegrid')
    handles, labels = ax.get_legend_handles_labels()
    plt.xlabel('Proportion targeted',fontsize=15)
    plt.ylabel('Area Under the Curve',fontsize=15)
    plt.legend(fontsize=12)
    ax.tick_params(labelsize=15)
    ax.legend(handles=handles[1:], labels=labels[1:], loc='lower right')
    

def plotBarImprovementTop(improvementModel, modelNames, perPop = [0.2, 0.4, 0.6, 0.8, 1.0], additionalComment ='', resolution = 0):
    plt.clf()
    if(resolution > 0):
        plt.figure(dpi= resolution)
    averageImprovement = []
    inPerPop = []
    inModelNames = []
    for modelName in modelNames:
        for per in perPop:
            tempVal = improvementModel[(improvementModel['n'] < per)&(improvementModel['model'] ==  modelName)].copy()
            tempVal.reset_index(drop=True, inplace=True)
            averageImprovement.append(tempVal['grUplift'].iloc[-1] )
            inPerPop.append(per)
            inModelNames.append(modelName)
    topImModel = pd.DataFrame({'AverageImp': averageImprovement, 'perPop': inPerPop, 'model': inModelNames})
    ax = sns.barplot(x='perPop', y='AverageImp',  hue="model", data= topImModel)
    ax.grid(False)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    handles, labels = ax.get_legend_handles_labels()
    plt.xlabel('Top proportion' + ' ' + additionalComment,fontsize=15)
    plt.ylabel('Improvement of top groups (%)',fontsize=15)
    plt.legend(fontsize=12)
    ax.tick_params(labelsize=15)
    ax.legend(handles=handles[1:])
    
    
def plotBarImprovementTopV2(improvementModel, modelNames, iaxis, startCount, perPop = [0.2, 0.4, 0.6, 0.8, 1.0], tickLabel=None, minY=None, maxY=None, title=None, xlabel=None):
    averageImprovement = []
    inPerPop = []
    inModelNames = []
    transfModelNames = []
    for modelName in modelNames:
        tempModel = ''
        if(modelName == 'CausalTree'):
            tempModel = 'CT'   
        elif(modelName == 'TOTree'):
            tempModel = 'TOT'   
        elif(modelName == 'TStatisticTree'):
            tempModel = 'ST'  
        elif(modelName == 'FitBasedTree'):
            tempModel = 'FT'   
        elif(modelName == 'MCTTree'):
            tempModel = 'MCT'   
        elif(modelName == 'CausalTree_En'):
            tempModel = 'CTEn'   
        elif(modelName == 'TOTree_En'):
            tempModel = 'TOTEn'   
        elif(modelName == 'TStatisticTree_En'):
            tempModel = 'STEn'  
        elif(modelName == 'FitBasedTree_En'):
            tempModel = 'FTEn'
        elif(modelName == 'IndiNet'):
            tempModel = 'PIR'            
        else:
            tempModel = modelName
            
        transfModelNames.append(tempModel)
        prePop = float('-inf')
        for per in perPop:   
            #tempVal = improvementModel[(improvementModel['n'] < per)&(improvementModel['model'] ==  modelName)].copy()
            tempVal = improvementModel[(improvementModel['n'] < per)&(improvementModel['n'] >= prePop)&(improvementModel['model'] ==  modelName)].copy()
            tempVal.reset_index(drop=True, inplace=True)
            averageImprovement.append(tempVal['grUplift'].iloc[-1] )
            inPerPop.append(per)
            inModelNames.append(tempModel)
            prePop = per
    topImModel = pd.DataFrame({'AverageImp': averageImprovement, 'perPop': inPerPop, 'model': inModelNames})
            

        
    for fileCount in range(0, len(transfModelNames)):
        tempImModel =  topImModel[topImModel.model == transfModelNames[fileCount] ]          
        flatui = ["tab:orange"]         
        if(transfModelNames[fileCount] == 'UBCF'):
            flatui = 'lightsteelblue' 
        if(transfModelNames[fileCount] == 'IBCF'):
            flatui = 'steelblue'   
        if(transfModelNames[fileCount] == 'IPOP'):
            flatui = 'olive'  
        if(transfModelNames[fileCount] == 'FT'):
            flatui = 'yellowgreen'    
        if(transfModelNames[fileCount] == 'IROI'):
            flatui = 'tab:orange' 
        if(transfModelNames[fileCount] == 'Xlearner'):
            flatui = 'darkred'    
        if(transfModelNames[fileCount] == 'DRL'):
            flatui = 'saddlebrown' 
        if(transfModelNames[fileCount] == 'CF'):
            flatui = 'deeppink'            

        print('For model: ' + transfModelNames[fileCount] )
        print(tempImModel )
            
        g = sns.barplot(x='perPop', y='AverageImp',  data= tempImModel, ax = iaxis[startCount + fileCount], color = flatui )
        patch = g.patches[0]
        tempX = np.arange(0,len(tempImModel)) + patch.get_width()/2
        sns.lineplot(data = tempImModel, x=tempX, y='AverageImp', color = 'blue',  marker='o', ax = iaxis[startCount + fileCount])
        iaxis[startCount + fileCount].grid(True, axis = 'y', color='b', linestyle='--', linewidth=1)
        
        
        iaxis[startCount + fileCount].set_xlabel('')
        iaxis[startCount + fileCount].set_ylabel('')
        iaxis[startCount + fileCount].spines['top'].set_visible(False)
        iaxis[startCount + fileCount].spines['right'].set_visible(False)
        iaxis[startCount + fileCount].spines['left'].set_color('black')
        iaxis[startCount + fileCount].spines['bottom'].set_color('black')
        iaxis[startCount + fileCount].tick_params(axis='both', which='major', labelsize=25)
        #iaxis[startCount + fileCount].set_xticklabels(iaxis[startCount + fileCount].get_xticklabels(), rotation=45)
        iaxis[startCount + fileCount].set_xticklabels([])
        #iaxis[startCount + fileCount].legend(fontsize=16, ncol=1)
        iaxis[startCount + fileCount].set(ylim=(minY, maxY))
        
        iaxis[startCount + fileCount].yaxis.set_ticks(np.arange(0, maxY, 10))
        
        iaxis[startCount + fileCount].spines['left'].set_linewidth(1)
        iaxis[startCount + fileCount].spines['bottom'].set_linewidth(1) 
        iaxis[startCount + fileCount].patch.set_facecolor('w')  
        #iaxis[startCount + fileCount].yaxis.set_major_locator(ticker.MultipleLocator(3))
        #iaxis[startCount + fileCount].yaxis.set_major_formatter(ticker.ScalarFormatter())        

        if(title !=None):
            iaxis[startCount + fileCount].set_title(title, fontsize=20)
            
        if(xlabel != None):
            iaxis[startCount + fileCount].set_xlabel(xlabel,fontsize=20) 
            
        if(tickLabel != None):
            iaxis[startCount + fileCount].set_xticklabels(tickLabel, rotation=45)
        iaxis[startCount + fileCount].tick_params(axis='both', which='major', labelsize=20)

    
def plotQini(model):
    plt.clf()
    
    # plot the data
    ax = sns.lineplot(x='n', y='uplift', hue='model', data=model,
                      style='model', dashes=False)
    # Plot settings
    sns.set_style('whitegrid')
    handles, labels = ax.get_legend_handles_labels()
    plt.xlabel('Proportion targeted',fontsize=15)
    plt.ylabel('CIMP (%)',fontsize=15)
    plt.subplots_adjust(right=1)
    plt.subplots_adjust(top=1)
    plt.legend(fontsize=12)
    ax.tick_params(labelsize=15)
    ax.legend(handles=handles[1:], labels=labels[1:], loc='upper left')
     
def areaUnderCurve(models, modelNames):
    modelAreas = []
    for modelName in modelNames:   
        area = 0
        tempModel = models[models['model'] == modelName].copy()
        tempModel.reset_index(drop=True, inplace=True)       
        for i in range(1, len(tempModel)):  # df['A'].iloc[2]
            delta = tempModel['n'].iloc[i] - tempModel['n'].iloc[i-1]
            y = (tempModel['uplift'].iloc[i] + tempModel['uplift'].iloc[i-1]  )/2
            area += y*delta
        modelAreas.append(area)  
    return modelAreas
    
def estimateImprovmentCurve(estimatedImprovements, outcomeName):   
    estimatedImprovements['ABS_Improvement'] = estimatedImprovements['Improvement'].abs()
    estimatedImprovements.sort_values(by=['ABS_Improvement'], ascending = [False], inplace=True, axis=0)
    estimatedImprovements = estimatedImprovements.reset_index(drop=True)    
    Sum_Y_Follow_Rec    = np.array([])
    Sum_Nbr_Follow_Rec    = np.array([])
    Sum_Y_Not_Follow_Rec    = np.array([])
    Sum_Nbr_Not_Follow_Rec    = np.array([])
    Improvement    = np.array([])
    total_Y_Follow_Rec  = 0
    total_Nbr_Follow_Rec = 0
    total_Y_Not_Follow_Rec  = 0
    total_Nbr_Not_Follow_Rec = 0    
    for index, individual in estimatedImprovements.iterrows():
        improvementTemp = 0
        if(individual['FollowRec'] == 1):
            total_Nbr_Follow_Rec = total_Nbr_Follow_Rec + 1
            total_Y_Follow_Rec = total_Y_Follow_Rec + individual[outcomeName]
        else:
            total_Nbr_Not_Follow_Rec = total_Nbr_Not_Follow_Rec + 1
            total_Y_Not_Follow_Rec = total_Y_Not_Follow_Rec + individual[outcomeName]   
        Sum_Nbr_Follow_Rec = np.append (Sum_Nbr_Follow_Rec, total_Nbr_Follow_Rec)
        Sum_Y_Follow_Rec = np.append (Sum_Y_Follow_Rec, total_Y_Follow_Rec)
        Sum_Nbr_Not_Follow_Rec = np.append (Sum_Nbr_Not_Follow_Rec, total_Nbr_Not_Follow_Rec)
        Sum_Y_Not_Follow_Rec = np.append (Sum_Y_Not_Follow_Rec, total_Y_Not_Follow_Rec)
        if(total_Nbr_Follow_Rec == 0 or total_Nbr_Not_Follow_Rec == 0 ):
            if(total_Nbr_Follow_Rec > 0):
                improvementTemp = (total_Y_Follow_Rec/total_Nbr_Follow_Rec)
            else:
                improvementTemp = 0
        else:
            improvementTemp = (total_Y_Follow_Rec/total_Nbr_Follow_Rec) - (total_Y_Not_Follow_Rec/total_Nbr_Not_Follow_Rec)   
        Improvement = np.append (Improvement, improvementTemp)
    ser = pd.Series(Sum_Nbr_Follow_Rec)
    estimatedImprovements['Total_Nbr_Follow_Rec'] = ser
    ser = pd.Series(Sum_Y_Follow_Rec)
    estimatedImprovements['Total_Y_Follow_Rec'] = ser
    ser = pd.Series(Sum_Nbr_Not_Follow_Rec)
    estimatedImprovements['Total_Nbr_Not_Follow_Rec'] = ser
    ser = pd.Series(Sum_Y_Not_Follow_Rec)
    estimatedImprovements['Total_Y_Not_Follow_Rec'] = ser
    ser = pd.Series(Improvement)
    estimatedImprovements['UPLIFT'] = ser
    return estimatedImprovements
    
def getDiversification(FolderLocation, fold, prefileName, postfileName, outcomeName, perPop):
    globalKendal = 0
    globalVariation = 0
    globalVariance = 0
    improvementModel = pd.DataFrame({})  
    newMolde = genQiniDataML(FolderLocation,fold, prefileName, postfileName, 'NoName', outcomeName)
    
    improvementModel = improvementModel.append(newMolde)
    improvementModel['uplift'] = improvementModel['uplift']* 100
    improvementModel['grUplift'] = improvementModel['grUplift']* 100   
    averageImprovement = []
    inPerPop = []
    averageVal = 0
    for per in perPop:
        tempVal = improvementModel[(improvementModel['n'] < per)].copy()
        tempVal.reset_index(drop=True, inplace=True)          
        tempVal = tempVal['grUplift'].iloc[-1]
        averageImprovement.append( tempVal)
        inPerPop.append(per)
        if(per == 1):
            averageVal = tempVal       
    topImModel = pd.DataFrame({'AverageImprove': averageImprovement, 'perPop': inPerPop})
    topImModel.fillna(averageVal)
    topImModel['perPop'] = sorted(perPop, reverse=True)
    res = stats.kendalltau(topImModel['AverageImprove'], topImModel['perPop'])
    globalKendal =  res[0]
    res = stats.spearmanr(topImModel['AverageImprove'], topImModel['perPop'])
    globalSpear = res[0]           
    return (globalKendal, globalSpear)
         
def getAUCNTopPopReSort(FolderLocation, fold, prefileName, postfileName, outcomeName, ImageName, newImageFolder, resort = False):
    improvementMtreeModels = []
    for fileCount in range (1, fold + 1):
        improveFilePath = os.path.join(FolderLocation, prefileName + str(fileCount) + postfileName + '.csv')
        if(not (path.exists(improveFilePath) )):
            continue
        results = pd.read_csv(improveFilePath,  encoding = "ISO-8859-1", engine='python')
        if (not('Improvement' in results.columns)):
            results ['Improvement'] =  results ['LIFT_SCORE'] 
        if (not('FollowRec' in results.columns)):
            results ['FollowRec'] = 0
            for index, row in results.iterrows(): 
                treatmentName = row['TREATMENT_NAME']
                if(results.at[index,'Improvement'] > 0):
                    if(row[treatmentName] == 1):
                        results.at[index,'FollowRec'] = 1
                    else:
                        results.at[index,'FollowRec'] = 0
                else:
                    
                    if(row[treatmentName] == 1):
                        results.at[index,'FollowRec'] = 0
                    else:
                        results.at[index,'FollowRec'] = 1         
        if(resort):
            results['ABS_Improvement'] = results['Improvement'].abs()
            results.sort_values(by=['ABS_Improvement', outcomeName], ascending = [False, False], inplace=True, axis=0)  
        results = results.reset_index(drop=True)    
        newImprovement = estimateQiniCurve(results, outcomeName, 'Tree')
        improvementMtreeModels.append(newImprovement) 
    improvementMtreeCurves = pd.DataFrame({})
    improvementMtreeCurves['n'] = improvementMtreeModels[0]['n']
    improvementMtreeCurves['model'] = improvementMtreeModels[0]['model']
    icount = 1
    modelNames = []
    groupModelNames = []
    for eachM in improvementMtreeModels:
        improvementMtreeCurves['uplift' + str(icount)] = eachM['uplift']
        modelNames.append('uplift' + str(icount))
        improvementMtreeCurves['grUplift' + str(icount)] = eachM['grUplift']
        groupModelNames.append('grUplift' + str(icount))
        icount = icount + 1  
    improvementMtreeCurves['uplift'] = improvementMtreeCurves[modelNames].mean(axis=1)
    improvementMtreeCurves['grUplift'] = improvementMtreeCurves[groupModelNames].mean(axis=1)
    improvementModels = pd.DataFrame({})
    improvementModels = improvementModels.append(improvementMtreeCurves)
    ## convert to percent
    improvementModels['uplift'] = improvementModels['uplift']* 100
    improvementModels['grUplift'] = improvementModels['grUplift']* 100
    plotQini(improvementModels)
    curveNames = ['Tree']   
    improvementModels['uplift'] = improvementModels['uplift'].fillna(0)
    estimateAres = areaUnderCurve(improvementModels, curveNames)   
    return  estimateAres[0]

def getAuucScore(FolderLocation, fold, prefileName, postfileName, outcomeName, factor):
    icount = 0
    totalScoreg = 0
    totalScoregi = 0
    for fileCount in range (1, fold + 1):
        improveFilePath = os.path.join(FolderLocation, prefileName + str(fileCount) + postfileName + '.csv')
        if(not (path.exists(improveFilePath) )):
            continue
        results = pd.read_csv(improveFilePath,  encoding = "ISO-8859-1", engine='python')
        df = pd.DataFrame({})
        df['Y'] = results[outcomeName]
        df['T'] = results[factor]
        df['CUR'] = results['LIFT_SCORE'] 
        icount = icount + 1
        scoreg = auuc_score(df,  outcome_col='Y', treatment_col='T')
        scoregi = qini_score(df,  outcome_col='Y', treatment_col='T')
        totalScoreg = totalScoreg + scoreg['CUR']
        totalScoregi = totalScoregi + scoregi['CUR']
        
    return totalScoreg/icount, totalScoregi/icount
        
        
def getAUCNTopPop(FolderLocation, fold, prefileName, postfileName, outcomeName, plotFig=True):
    improvementMtreeModels = []
    for fileCount in range (1, fold + 1):
        improveFilePath = os.path.join(FolderLocation, prefileName + str(fileCount) + postfileName + '.csv')
        if(not (path.exists(improveFilePath) )):
            continue
        results = pd.read_csv(improveFilePath,  encoding = "ISO-8859-1", engine='python')
        if (not('Improvement' in results.columns)):
            results ['Improvement'] =  results ['LIFT_SCORE'] 
        if (not('FollowRec' in results.columns)):
            results ['FollowRec'] = results ['FOLLOW_REC']
                        
        newImprovement = estimateQiniCurve(results, outcomeName, 'Tree')
        improvementMtreeModels.append(newImprovement)   
    improvementMtreeCurves = pd.DataFrame({})
    improvementMtreeCurves['n'] = improvementMtreeModels[0]['n']
    improvementMtreeCurves['model'] = improvementMtreeModels[0]['model']
    icount = 1
    modelNames = []
    groupModelNames = []
    for eachM in improvementMtreeModels:
        improvementMtreeCurves['uplift' + str(icount)] = eachM['uplift']
        modelNames.append('uplift' + str(icount))
        improvementMtreeCurves['grUplift' + str(icount)] = eachM['grUplift']
        groupModelNames.append('grUplift' + str(icount))
        icount = icount + 1  
    improvementMtreeCurves['uplift'] = improvementMtreeCurves[modelNames].mean(axis=1)
    improvementMtreeCurves['grUplift'] = improvementMtreeCurves[groupModelNames].mean(axis=1)
    improvementModels = pd.DataFrame({})
    improvementModels = improvementModels.append(improvementMtreeCurves)
    ## convert to percent
    improvementModels['uplift'] = improvementModels['uplift']* 100
    improvementModels['grUplift'] = improvementModels['grUplift']* 100
    if(plotFig):
        plotQini(improvementModels)
    curveNames = ['Tree']   
    improvementModels['uplift'] = improvementModels['uplift'].fillna(0)
    estimateAres = areaUnderCurve(improvementModels, curveNames)
    return  estimateAres[0]

def getAUCNTopPopV2(FolderLocation, fold, prefileName, postfileName, outcomeName, plotFig=True):
    improvementMtreeModels = []
    mycount = 0
    sumFollow = 0
    for fileCount in range (1, fold + 1):
        improveFilePath = os.path.join(FolderLocation, prefileName + str(fileCount) + postfileName + '.csv')
        if(not (path.exists(improveFilePath) )):
            continue
        results = pd.read_csv(improveFilePath,  encoding = "ISO-8859-1", engine='python')
        if (not('Improvement' in results.columns)):
            results ['Improvement'] =  results ['LIFT_SCORE'] 
        if (not('FollowRec' in results.columns)):
            results ['FollowRec'] = 0
            for index, row in results.iterrows():
                treatmentName = row['TREATMENT_NAME']
                if(results.at[index,'Improvement'] > 0):
                    if(row[treatmentName] == 1): 
                        results.at[index,'FollowRec'] = 1
                    else:
                        results.at[index,'FollowRec'] = 0
                else:
                    if(row[treatmentName] == 1):
                        results.at[index,'FollowRec'] = 0
                    else:
                        results.at[index,'FollowRec'] = 1
        newImprovement = estimateQiniCurve(results, outcomeName, 'Tree')
        improvementMtreeModels.append(newImprovement) 
        
        lastRow = results.tail(1)
        sumFollow = sumFollow + lastRow.iloc[0].loc['Total_Nbr_Follow_Rec']*100.0 / (lastRow.iloc[0].loc['Total_Nbr_Follow_Rec'] 
        + lastRow.iloc[0].loc['Total_Nbr_Not_Follow_Rec'])
        mycount = mycount + 1

    improvementMtreeCurves = pd.DataFrame({})
    improvementMtreeCurves['n'] = improvementMtreeModels[0]['n']
    improvementMtreeCurves['model'] = improvementMtreeModels[0]['model']
    icount = 1
    modelNames = []
    groupModelNames = []
    for eachM in improvementMtreeModels:
        improvementMtreeCurves['uplift' + str(icount)] = eachM['uplift']
        modelNames.append('uplift' + str(icount))
        improvementMtreeCurves['grUplift' + str(icount)] = eachM['grUplift']
        groupModelNames.append('grUplift' + str(icount))
        icount = icount + 1  
    improvementMtreeCurves['uplift'] = improvementMtreeCurves[modelNames].mean(axis=1)
    improvementMtreeCurves['grUplift'] = improvementMtreeCurves[groupModelNames].mean(axis=1)
    improvementModels = pd.DataFrame({})
    improvementModels = improvementModels.append(improvementMtreeCurves)
    ## convert to percent
    improvementModels['uplift'] = improvementModels['uplift']* 100
    improvementModels['grUplift'] = improvementModels['grUplift']* 100
    if(plotFig):
        plotQini(improvementModels)
    curveNames = ['Tree']   
    improvementModels['uplift'] = improvementModels['uplift'].fillna(0)
    estimateAres = areaUnderCurve(improvementModels, curveNames)
    
    
    
    return  estimateAres[0], sumFollow/mycount
              
def splitData(dataset, fold, outputPath, outputName):
    fileCount = 1
    for icount in range(20, 20 + fold):
        Mtrain, Mtest = train_test_split(dataset, test_size=0.30, random_state=icount, shuffle=True)
        Mtrain.reset_index(drop=True, inplace=True)
        Mtest.reset_index(drop=True, inplace=True)
        MtrainPath = os.path.join(outputPath, outputName + '_' + 'train' +'_' + str(fileCount) +'.csv')
        Mtrain.to_csv(MtrainPath, index=False) 
        MtestPath = os.path.join(outputPath, outputName + '_' + 'test' +'_' + str(fileCount) +'.csv')
        Mtest.to_csv(MtestPath, index=False) 
        fileCount = fileCount + 1
       
def genQiniDataOneFile(fileName, modelName, outcomeName): 
    improvementMtreeModels = []
    Sdataset0 = pd.read_csv(fileName,  encoding = "ISO-8859-1", engine='python')
    if (not('Improvement' in Sdataset0.columns)):
        Sdataset0 ['Improvement'] =  Sdataset0 ['LIFT_SCORE'] 
    if (not('FollowRec' in Sdataset0.columns)):
        Sdataset0 ['FollowRec'] = 0
        for index, row in Sdataset0.iterrows():
            treatmentName = row['TREATMENT_NAME']
            if(Sdataset0.at[index,'Improvement'] > 0):
                if(row[treatmentName] == 1):
                    Sdataset0.at[index,'FollowRec'] = 1
                else:
                    Sdataset0.at[index,'FollowRec'] = 0
            else:
                if(row[treatmentName] == 1):
                    
                    Sdataset0.at[index,'FollowRec'] = 0
                else:
                    Sdataset0.at[index,'FollowRec'] = 1           
    newImprovement = estimateQiniCurve(Sdataset0, outcomeName, modelName)
    improvementMtreeModels.append(newImprovement)   
    improvementMtreeCurves = pd.DataFrame({})
    improvementMtreeCurves['n'] = improvementMtreeModels[0]['n']
    improvementMtreeCurves['model'] = improvementMtreeModels[0]['model']
    icount = 1
    modelNames = []
    groupModelNames = []  
    for eachM in improvementMtreeModels:
        improvementMtreeCurves['uplift' + str(icount)] = eachM['uplift']
        modelNames.append('uplift' + str(icount))
        improvementMtreeCurves['grUplift' + str(icount)] = eachM['grUplift']
        groupModelNames.append('grUplift' + str(icount))
        icount = icount + 1
    improvementMtreeCurves['uplift'] = improvementMtreeCurves[modelNames].mean(axis=1)
    improvementMtreeCurves['grUplift'] = improvementMtreeCurves[groupModelNames].mean(axis=1)
    return improvementMtreeCurves 
    
def genQiniDataML(folderName, fold, prefileName, postfileName, modelName, outcomeName):
    improvementMtreeModels = []
    for fileCount in range (1, fold + 1):
        fPath = os.path.join(folderName, prefileName + str(fileCount) + postfileName + '.csv')   
        Sdataset0 = pd.read_csv(fPath,  encoding = "ISO-8859-1", engine='python')
        if (not('Improvement' in Sdataset0.columns)):
            Sdataset0 ['Improvement'] =  Sdataset0 ['LIFT_SCORE'] 
        if (not('FollowRec' in Sdataset0.columns)):
            Sdataset0 ['FollowRec'] = Sdataset0 ['FOLLOW_REC']           
        newImprovement = estimateQiniCurve(Sdataset0, outcomeName, modelName)
        improvementMtreeModels.append(newImprovement)   
    improvementMtreeCurves = pd.DataFrame({})
    improvementMtreeCurves['n'] = improvementMtreeModels[0]['n']
    improvementMtreeCurves['model'] = improvementMtreeModels[0]['model']   
    icount = 1
    modelNames = []
    groupModelNames = []   
    for eachM in improvementMtreeModels:
        improvementMtreeCurves['uplift' + str(icount)] = eachM['uplift']
        modelNames.append('uplift' + str(icount))
        improvementMtreeCurves['grUplift' + str(icount)] = eachM['grUplift']
        groupModelNames.append('grUplift' + str(icount))
        icount = icount + 1   
    improvementMtreeCurves['uplift'] = improvementMtreeCurves[modelNames].mean(axis=1)
    improvementMtreeCurves['grUplift'] = improvementMtreeCurves[groupModelNames].mean(axis=1)
    return improvementMtreeCurves

def estimateUplift(estimatedImprovements, outcomeName, sortbyabs=False):
    
    if(sortbyabs):
        estimatedImprovements['ABS_Improvement'] = estimatedImprovements['LIFT_SCORE'].abs()
        estimatedImprovements.sort_values(by=['ABS_Improvement'], ascending = [False], inplace=True, axis=0)
    else:
        estimatedImprovements.sort_values(by=['LIFT_SCORE'], ascending = [False], inplace=True, axis=0)
    estimatedImprovements = estimatedImprovements.reset_index(drop=True) 
    
    Sum_Y_Follow_Rec    = np.array([])
    Sum_Nbr_Follow_Rec    = np.array([])
    Sum_Y_Not_Follow_Rec    = np.array([])
    Sum_Nbr_Not_Follow_Rec    = np.array([])
    Improvement    = np.array([])
    total_Y_Follow_Rec  = 0
    total_Nbr_Follow_Rec = 0
    total_Y_Not_Follow_Rec  = 0
    total_Nbr_Not_Follow_Rec = 0    
    for index, individual in estimatedImprovements.iterrows():
        improvementTemp = 0
        if(individual['FOLLOW_REC'] == 1):
            total_Nbr_Follow_Rec = total_Nbr_Follow_Rec + 1
            total_Y_Follow_Rec = total_Y_Follow_Rec + individual[outcomeName]
        else:
            total_Nbr_Not_Follow_Rec = total_Nbr_Not_Follow_Rec + 1
            total_Y_Not_Follow_Rec = total_Y_Not_Follow_Rec + individual[outcomeName]   
        Sum_Nbr_Follow_Rec = np.append (Sum_Nbr_Follow_Rec, total_Nbr_Follow_Rec)
        Sum_Y_Follow_Rec = np.append (Sum_Y_Follow_Rec, total_Y_Follow_Rec)
        Sum_Nbr_Not_Follow_Rec = np.append (Sum_Nbr_Not_Follow_Rec, total_Nbr_Not_Follow_Rec)
        Sum_Y_Not_Follow_Rec = np.append (Sum_Y_Not_Follow_Rec, total_Y_Not_Follow_Rec)
        if(total_Nbr_Follow_Rec == 0 or total_Nbr_Not_Follow_Rec == 0 ):
            if(total_Nbr_Follow_Rec > 0):
                improvementTemp = (total_Y_Follow_Rec/total_Nbr_Follow_Rec)
            else:
                improvementTemp = 0
        else:
            improvementTemp = (total_Y_Follow_Rec/total_Nbr_Follow_Rec) - (total_Y_Not_Follow_Rec/total_Nbr_Not_Follow_Rec)   
        Improvement = np.append (Improvement, improvementTemp)
    ser = pd.Series(Sum_Nbr_Follow_Rec)
    estimatedImprovements['N_TREATED'] = ser
    ser = pd.Series(Sum_Y_Follow_Rec)
    estimatedImprovements['Y_TREATED'] = ser
    ser = pd.Series(Sum_Nbr_Not_Follow_Rec)
    estimatedImprovements['Y_UNTREATED'] = ser
    ser = pd.Series(Sum_Y_Not_Follow_Rec)
    estimatedImprovements['N_UNTREATED'] = ser
    ser = pd.Series(Improvement)
    estimatedImprovements['UPLIFT'] = ser
    return estimatedImprovements
    
    
def getAUUCTopGroup(FolderLocation, fold, prefileName, postfileName, outcomeName, plotFig=True):
    improvementMtreeModels = []
    for fileCount in range (1, fold + 1):
        improveFilePath = os.path.join(FolderLocation, prefileName + str(fileCount) + postfileName + '.csv')
        if(not (path.exists(improveFilePath) )):
            continue
        results = pd.read_csv(improveFilePath,  encoding = "ISO-8859-1", engine='python')
        if (not('Improvement' in results.columns)):
            results ['Improvement'] =  results ['LIFT_SCORE'] 
        if (not('FollowRec' in results.columns)):
            results ['FollowRec'] = results ['FOLLOW_REC']
                        
        newImprovement = estimateQiniCurve(results, outcomeName, 'Tree')
        improvementMtreeModels.append(newImprovement)   
    improvementMtreeCurves = pd.DataFrame({})
    improvementMtreeCurves['n'] = improvementMtreeModels[0]['n']
    improvementMtreeCurves['model'] = improvementMtreeModels[0]['model']
    icount = 1
    modelNames = []
    groupModelNames = []
    for eachM in improvementMtreeModels:
        improvementMtreeCurves['uplift' + str(icount)] = eachM['uplift']
        modelNames.append('uplift' + str(icount))
        improvementMtreeCurves['grUplift' + str(icount)] = eachM['grUplift']
        groupModelNames.append('grUplift' + str(icount))
        icount = icount + 1  
    improvementMtreeCurves['uplift'] = improvementMtreeCurves[modelNames].mean(axis=1)
    improvementMtreeCurves['grUplift'] = improvementMtreeCurves[groupModelNames].mean(axis=1)
    improvementModels = pd.DataFrame({})
    improvementModels = improvementModels.append(improvementMtreeCurves)
    ## convert to percent
    improvementModels['uplift'] = improvementModels['uplift']* 100
    improvementModels['grUplift'] = improvementModels['grUplift']* 100
    if(plotFig):
        plotQini(improvementModels)
    curveNames = ['Tree']   
    improvementModels['uplift'] = improvementModels['uplift'].fillna(0)
    estimateAres = areaUnderCurve(improvementModels, curveNames)
    return  estimateAres[0]