# A function that returns the variable which has skew/bias value. 
# i.e. perticular value is repeated in 80% of train data
# trainData : pass the train dataset
# returns : the name of columns that needs to drop.
GetBiasVariables = function(trainData,cutoff=0.8)
{
  totalRecords=nrow(trainData)
  biasResult = data.frame(ColumnName=NULL,Value=NULL,Bias=NULL)
  for (varName in names(trainData)) {
    
    freqTable = table(trainData[varName])
    maxValue=max(freqTable)
    maxName=names(freqTable[which.max(freqTable)])
    if(maxValue/totalRecords>=cutoff)
    {
      biasResult=rbind(biasResult,data.frame(ColumnName=varName,Value=maxName,Bias=maxValue/totalRecords))
    }
  }
  biasResult
}



# A function that replace NA value with provided value
# dataset : pass the object of dataframe
# colNames : pass the column names
# value : replacement for NA
# returns : dataset with replaced NA values
replaceNAValues = function(dataset,colNames,value)
{
  for (varName in names(dataset)) {
    if(varName %in% colNames)
    {
      if(is.factor(dataset[,varName])) # need to do this. OW it will throw the error
      {
        levels(dataset[,varName]) = c(levels(dataset[,varName]),value)
        levels(dataset[,varName]) = c(levels(dataset[,varName]),value)
      }
      dataset[,varName][(is.na(dataset[,varName]))] <- value
    }
  }
  dataset
}


factorEligebleColumns=function(dataset)
{
  result = data.frame(ColumnName=NULL,Count=NULL,Values=NULL)
  for(column in colnames(dataset))
  {
    if(class(dataset[,column])!='factor'){
      cnt=length(unique(dataset[,column]))
      if(cnt<=2)
      {
        values=(unique(dataset[,column]))
        valStr=''
        for(v in values)
        {
          valStr=paste(valStr,v )
        }
        result=rbind(result,data.frame(ColumnName=column,Count=cnt,Values=valStr))
      }
      
    }
  }
  result
}

covertToFactors=function(dataset,columnList)
{
  for (col in columnList) {
    dataset[,col] = as.factor(dataset[,col])
  }
  dataset
}


GetCorrelatedColumns = function(dataset)
{
  nums <- sapply(dataset, is.numeric)
  corMatrix=cor(dataset[nums])
  corFeatures=data.frame('Var1'=NULL,'Var2'=NULL,'Relationship'=NULL,'Corr'=NULL)
  
  for(col in colnames(corMatrix)){
    for(row in rownames(corMatrix))
    {
      if((!is.na(corMatrix[row,col])) && col!=row){
        if(corMatrix[row,col]>=0.5 || corMatrix[row,col]<=-0.5)
        {
          if(row<col)
          {
            temp=row;row=col;col=temp;
          }
          corFeatures=rbind.data.frame(corFeatures,data.frame('Var1'=col,'Var2'=row,'Relationship'='Strong','Corr'=corMatrix[row,col]))
        }
        
        else if(corMatrix[row,col]>=0.3 || corMatrix[row,col]<=-0.3)
        {
          if(row<col)
          {
            temp=row;row=col;col=temp;
          }
          corFeatures=rbind.data.frame(corFeatures,data.frame('Var1'=col,'Var2'=row,'Relationship'='Moderate','Corr'=corMatrix[row,col]))
        }
      }
    }
  }
  corFeatures$Var1=as.character(corFeatures$Var1)
  corFeatures$Var2=as.character(corFeatures$Var2)
  corFeatures=unique(corFeatures[order(corFeatures$Relationship,corFeatures$Var1,corFeatures$Var2),]);
  rownames(corFeatures)=NULL
  return(corFeatures)
}
GetCorrlationWithTarget=function(dataset,targetVector)
{
  corFeatures=data.frame('Var'=NULL,'Target_Correlation'=NULL)
  for(col in colnames(dataset))
  {
    corFeatures=rbind.data.frame(corFeatures,data.frame('Var'=col, 'Target_Correlation'=cor(dataset[col],targetVector)))
  }
  corFeatures=corFeatures[order(-abs(corFeatures$Target_Correlation)),]
  rownames(corFeatures)=NULL
  return(corFeatures)
}
