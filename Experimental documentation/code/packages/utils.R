
divideToKsubsets <- function(data, k=10, seed, rnd=T){
  set.seed(seed=seed)
  
  if(rnd){data <- data[order((runif(nrow(data)))),]}
  
  numResidual <- k - nrow(data)%%k
  dummyData<-as.data.frame(matrix(NA,nrow=numResidual,ncol=ncol(data)))
  names(dummyData)<-names(data)
  
  data<-rbind(data,dummyData)
  splitData<-split(data,1:k)
  
  for(i in 1:k){
    splitData[[i]] <- na.omit(splitData[[i]])
  }
  
  return(splitData)
}

divideToKsubsets <- function(data, k=10, seed, rnd=T){
  set.seed(seed=seed)
  
  if(rnd){data <- data[order((runif(nrow(data)))),]}
  
  numResidual <- k - nrow(data)%%k
  dummyData<-as.data.frame(matrix(NA,nrow=numResidual,ncol=ncol(data)))
  names(dummyData)<-names(data)
  
  data<-rbind(data,dummyData)
  splitData<-split(data,1:k)
  
  for(i in 1:k){
    splitData[[i]] <- na.omit(splitData[[i]])
  }
  
  return(splitData)
}

decChurn <- function(data){
  nf  <- data$nf             
  lt  <- data$lt * nf
  lt[lt==0] <- lt[lt==0] + 1
  churn <- ((data$la + data$ld) * lt)/2  # LA and LD was normalized by LT
  
  return (churn)
}

doSampling <- function(data, obj, seed=0){
  set.seed(seed=seed)   #### random seed make the experiment result to be replicated
  dataT <- data[data[, obj]==1, ]
  dataN <- data[data[, obj]==0, ]
  if (nrow(dataT) >= nrow(dataN)) {
    dataT <- dataT[order(runif(nrow(dataT))), ]
    dataT <- dataT[1:nrow(dataN), ]
  } else {
    dataN <- dataN[order(runif(nrow(dataN))), ]
    dataN <- dataN[1:nrow(dataT), ]
  }
  data <- rbind(dataT, dataN)
  
  return (data)
}

### this function sort the data frame base on the predicted value and second by other policies
SortData <- function(data, effortaware=FALSE, sorted=FALSE, worstcase=FALSE, bestcase=FALSE, LOCUP=FALSE)
{
  if (!all(c("NUM", "LOC", "PRE") %in% colnames(data))) { stop("ERROR: NUM, REL, LOC, or PRE is not colnames of data frame") }
  
  key.2nd <- "REL"
  if (effortaware) { key.2nd <- "density" }
  if (effortaware) {
    if (!(key.2nd %in% colnames(data))) {
      data[[key.2nd]] <- data$NUM/(data$LOC+1)
    }
    sorted <- FALSE
  }
  
  if (!sorted) {
    if (worstcase) {
      data <- data[order(-data$PRE, +data[[key.2nd]], -data$LOC), ]
    } else if (bestcase) {
      data <- data[order(-data$PRE, -data[[key.2nd]], +data$LOC), ]
    } else if (LOCUP) {
      data <- data[order(-data$PRE, +data$LOC), ]
    } else {
      data <- data[order(-data$PRE), ]
    }
    sorted <- TRUE
  }
  
  return(data)
}

SortData_CBS <- function(data, effortaware=FALSE, sorted=FALSE)
{
  if (!all(c("NUM", "REL", "LOC", "PRE") %in% colnames(data))) { stop("ERROR: NUM, REL, LOC, or PRE is not colnames of data frame") }
  
  key.2nd <- "REL"
  if (effortaware) { key.2nd <- "density" }
  if (effortaware) {
    if (!(key.2nd %in% colnames(data))) {
      data[[key.2nd]] <- data$NUM/(data$LOC+1)
    }
    sorted <- FALSE
  }
  
  threshold <- 0.5 #log(2)/2
  defectiveData <- subset(data, PRE > threshold)
  nondefectiveData <- subset(data,PRE<=threshold)
  ###鍒楄〃骞惰捣鏉?
  
  
  ### sort only the predicted defecitve data
  ###CBS+搴旇鏄牴鎹己闄峰€惧悜鎬RE璺烲OC鐨勬瘮鍊艰繘琛屾帓搴忕殑
  ###杩斿洖鐨勬帓搴忓垪琛ㄥ簲璇ユ槸鎶婃湁缂洪櫡鍒楄〃鍜屾棤缂洪櫡鍒楄〃鍚堝苟璧锋潵
  if (nrow(defectiveData)>0) {
    data <- defectiveData
    if (!sorted) {
      data <- data[order(+data$LOC), ]#PRE/LOC
      sorted <- TRUE
    }
    data <- rbind(data, nondefectiveData)#rbind鍚堝苟
  } else {
    if (!sorted) {
      data <- data[order(-data$PRE), ]
      sorted <- TRUE
    }
  }
  
  return(data)
}


### compute area under alberg curve
ComputeArea <- function(data, effortaware=TRUE, sorted=FALSE, worstcase=FALSE, bestcase=FALSE, LOCUP=FALSE, sortDef=FALSE)
{
  if (!sorted) {
    if (!sortDef) {
      data <- SortData(data=data, effortaware=effortaware, sorted=sorted, worstcase=worstcase, bestcase=bestcase, LOCUP=LOCUP)
    }else {
      data <- SortData_CBS(data=data, effortaware=effortaware, sorted=sorted)
    }
    
    sorted <- TRUE
  }
  
  len <- nrow(data)
  ### cumulative sums of LOC or NUM
  cumXs <- cumsum(data$LOC) # x: LOC% effort
  cumYs <- cumsum(data$NUM) # y: Bug% 测试标签
  
  Xs <- cumXs/cumXs[len]
  Ys <- cumYs/cumYs[len]
  
  fix_subareas <- vector(length=len)
  fix_subareas[1] <- 0.5 * Ys[1] * Xs[1]
  fix_subareas[2:len] <- 0.5 * (Ys[1:(len-1)] + Ys[2:len]) * abs(Xs[1:(len-1)] - Xs[2:len])
  
  area <- sum(fix_subareas)
  
  return(area)
}

ComputePopt <- function(data, effortaware=TRUE, sorted=FALSE, worstcase=FALSE, bestcase=FALSE, LOCUP=FALSE, sortDef=FALSE)
{
  if (!sorted) {
    if (!sortDef) {
      data <- SortData(data=data, effortaware=effortaware, sorted=sorted, worstcase=worstcase, bestcase=bestcase, LOCUP=LOCUP)
    }else {
      data <- SortData_CBS(data=data, effortaware=effortaware, sorted=sorted)
    }
    sorted <- TRUE
  }
  
  data.mdl <- data
  data.opt <- data[order(-data$density, +data$LOC), ]
  data.wst <- data[order(+data$density, -data$LOC), ]
  
  area.mdl <- ComputeArea(data=data.mdl, sorted=sorted, worstcase=worstcase, bestcase=bestcase, LOCUP=LOCUP)
  area.opt <- ComputeArea(data=data.opt, sorted=sorted, worstcase=worstcase, bestcase=bestcase, LOCUP=LOCUP)
  area.wst <- ComputeArea(data=data.wst, sorted=sorted, worstcase=worstcase, bestcase=bestcase, LOCUP=LOCUP)
  
  Popt <- 1 - (area.opt - area.mdl)/(area.opt - area.wst)
  
  return(Popt)
}

### compute recall with 20% effort
ComputeACC <- function(data, effortaware=TRUE, sorted=FALSE, worstcase=FALSE, bestcase=FALSE, LOCUP=FALSE, sortDef=FALSE)
{
  if (!sorted) {
    if (!sortDef) {
      data <- SortData(data=data, effortaware=effortaware, sorted=sorted, worstcase=worstcase, bestcase=bestcase, LOCUP=LOCUP)
    }else {
      data <- SortData_CBS(data=data, effortaware=effortaware, sorted=sorted)
    }
    sorted <- TRUE
  }
  
  len <- nrow(data)
  ### cumulative sums of LOC or NUM
  cumXs <- cumsum(data$LOC) # x: LOC%
  cumYs <- cumsum(data$NUM) # y: Bug%
  
  
  Xs <- cumXs/cumXs[len]
  Ys <- cumYs/cumYs[len]
  
  ### recall
  pos <- min(which(Xs >= 0.2))
  
  recall <- cumYs[pos] / cumYs[len]
  
  ### precision
  precision <- cumYs[pos] / pos
  
  ### fmeasure
  if (recall+precision != 0){
    fmeasure <- 2*recall*precision / (recall+precision)
  }else{
    fmeasure <- 0;
  }
  
  ### the number of modules examined with 20% effort
  NOM <- pos / len
  
  ### the number of Initial False Alarms enconutered before finding the first defect
  IFA <-  min(which(cumYs >= 1))
  ACC = c(Recall = list(recall),
          Precision = list(precision),
          Fmeasure = list(fmeasure),
          NOM = list(NOM),
          IFA = list(IFA))
  
  return(ACC)
}