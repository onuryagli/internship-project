library(data.table)
library(dplyr)
library(forecast)
library(caTools)
library(Metrics)
library(GeomComb)
library(ISLR)
library(caret)
library(plot.matrix)
library(plotrix)

setwd("C:/Users/acer/Desktop/internship-project/wbjc")
files <- dir()
df <- fread(files , header =T , quote ="")
set.seed(101)
#

##row.names(time.fc_mat) <- c("UPC-1", "UPC-2", "UPC-3", "UPC-4", "UPC-5")
#
matnames <- c("meanf" , "croston" , "ses" , "forecast" , "thetaf")

fclist.ts <- sapply(c("Time"),function(x) NULL)
fclist.ml <- sapply(c("Machine Learning"),function(x) NULL)
fclist.all <- sapply(c("Time", "Machine Learning"),function(x) NULL)

ensemble_names <- c("comb_CLS" , "comb_EIG4" , "comb_TA")

myTimeControl <- trainControl(method = "cv",
                              allowParallel = TRUE,search = "random",
                              verboseIter = F)

upc_names <- c("4850000193" , "4180022700" , "4180020750" , "3828103017" , "3828103009") 

ensemble.frame.rmse <- data.frame()
ensemble.frame.mape <- data.frame()
fc.frame.rmse <- data.frame()
fc.frame.mape <- data.frame()
sorted.err.rmse <- data.frame()
sorted.err.mape <- data.frame()
all.data <- data.frame()

for(y in 1:length(upc_names)){
  
  aa <- filter(df ,UPC ==upc_names[y] , between(STORE,2,80) , OK==1 , PRICE!=0 , MOVE!=0) 
  train_aa <- embed(aa$MOVE , 9)
  ########################################################################
  split.fc.mat <- matrix(NA, nrow=round(nrow(train_aa)*0.7), ncol=9)
  split.test.mat <- matrix(NA, nrow=(nrow(train_aa) - nrow(split.fc.mat)), ncol=9)
  
  ########################################################################
  
  split.fc.mat <- train_aa[1:(round(nrow(train_aa))*0.7),]
  split.test.mat <- train_aa[((round(nrow(train_aa)*0.7))+1) : nrow(train_aa),]
  
  colnames(split.fc.mat) <- c("Response","V2","V3","V4","V5","V6","V7","V8","V9")
  
  fcmat <- matrix(NA, nrow=nrow(split.test.mat), ncol=5)
  colnames(fcmat) <- c("Cubist" , "Lars" , "PPR" , "Ranger" , "Glmnet")
  
  time.fc_mat <- matrix(NA, nrow=nrow(split.test.mat), ncol=ncol(fcmat))
  colnames(time.fc_mat) <- c("Meanf" , "Croston" , "Ses" , "Forecast" , "Thetaf")
  
  for(z in 1:length(matnames)){
    for(i in 1:nrow(split.test.mat)){
      time.data.test <- match.fun(matnames[z])(as.numeric(split.test.mat[i,(2:9)]) , h=1)
      time.fc_mat[i,z] <- ts(time.data.test$mean)
    }
    
  }
  fclist.ts[[y]] <- ts(time.fc_mat , start = 1)
  
  #########
  
  cubist_training.mod <- train(Response ~ .,
                               data = split.fc.mat,
                               method = "cubist",
                               trControl = myTimeControl,
                               tuneLength = 30)
  fcst.cubist_training.mod <- predict(cubist_training.mod, newdata = split.test.mat)
  ########
  
  nnls_training.mod <- train(Response ~ .,
                             data = split.fc.mat,
                             method = "nnls",
                             trControl = myTimeControl,
                             tuneLength = 30)
  fcst.nnls_training.mod <- predict(nnls_training.mod, newdata = split.test.mat)
  
  ########
  
  ppr_training.mod <- train(Response ~ .,
                            data = split.fc.mat,
                            method = "ppr",
                            trControl = myTimeControl,
                            tuneLength = 30)
  fcst.ppr_training.mod <- predict(ppr_training.mod, newdata = split.test.mat)
  
  #########
  
  ranger_training.mod <- train(Response ~ .,
                               data = split.fc.mat,
                               method = "ranger",
                               trControl = myTimeControl,
                               tuneLength = 30)
  fcst.ranger_training.mod <- predict(ranger_training.mod, newdata = split.test.mat)
  
  #########
  
  glmnet_training.mod <- train(Response ~ .,
                               data = split.fc.mat,
                               method = "glmnet",
                               trControl = myTimeControl,
                               tuneLength = 30)
  fcst.glmnet_training.mod <- predict(glmnet_training.mod, newdata = split.test.mat)
  
  
  fcmat[1:nrow(as.data.frame(fcst.cubist_training.mod)),1] <- fcst.cubist_training.mod
  fcmat[1:nrow(as.data.frame(fcst.nnls_training.mod)),2] <-  fcst.nnls_training.mod 
  fcmat[1:nrow(as.data.frame(fcst.ppr_training.mod)),3] <-  fcst.ppr_training.mod 
  fcmat[1:nrow(as.data.frame(fcst.ranger_training.mod)),4] <-  fcst.ranger_training.mod
  fcmat[1:nrow(as.data.frame(fcst.glmnet_training.mod)),5] <-  fcst.glmnet_training.mod
  
  fclist.ml[[y]] <- fcmat
  
  split.meas.ensemble.mat <- as.data.frame(split.test.mat[1:(round(nrow(split.test.mat)*0.5)),1])
  split.test.ensemble.mat <- as.data.frame(split.test.mat[((round(nrow(split.test.mat)*0.5))+1) : nrow(split.test.mat),1])
  
  
  time.fc_mat.train <- as.data.frame(time.fc_mat[1:(round(nrow(time.fc_mat)*0.5)),])
  time.fc_mat.test <- as.data.frame(time.fc_mat[((round(nrow(time.fc_mat)*0.5))+1) : nrow(time.fc_mat),])
  
  fcmat.train <- as.data.frame(fcmat[1:(round(nrow(fcmat)*0.5)),])
  fcmat.test <- as.data.frame(fcmat[((round(nrow(fcmat)*0.5))+1) : nrow(fcmat),])
  
  fc.all.train <- as.data.frame(cbind(time.fc_mat.train,fcmat.train))
  fc.all.test <- as.data.frame(cbind(time.fc_mat.test,fcmat.test))
  
  data.all.ensemble <- foreccomb( observed_vector = as.matrix(split.meas.ensemble.mat), 
                                  prediction_matrix = as.matrix(fc.all.train),
                                  newpreds = as.matrix(fc.all.test)
  )
  
  fc.ensemble.mat <- matrix(NA, nrow=nrow(split.test.ensemble.mat), ncol=length(ensemble_names))
  ########################################################################
  
  for(x in 1:length(ensemble_names)){
    time.fc_mat.ensemble <- match.fun(ensemble_names[x])(data.all.ensemble)
    fc.ensemble <- time.fc_mat.ensemble$Forecasts_Test
    fc.ensemble.mat[,x] <- fc.ensemble
  }
  ############################(RMSE Calculation)

  for (c in 1:length(ensemble_names)) {
    rmse.err <- rmse(split.test.ensemble.mat[,1],fc.ensemble.mat[,c])
    ensemble.frame.rmse[y,c] <- rmse.err
  }
  
  for (e in 1:ncol(fc.all.test)) {
    rmse.fc.err <- rmse(split.test.ensemble.mat[,1],fc.all.test[,e])
    fc.frame.rmse[y,e] <- rmse.fc.err
  }
  
  rmse.data <- as.data.frame(cbind(fc.frame.rmse , ensemble.frame.rmse))
  
  ###########################(MAPE Calculation)
  for (c in 1:length(ensemble_names)) {
    mape.err <- mape(split.test.ensemble.mat[,1],fc.ensemble.mat[,c])
    ensemble.frame.mape[y,c] <- mape.err
  }
  colnames(ensemble.frame.mape)=colnames(ensemble.frame.rmse) <- c("Comb_CLS" , "Comb_EIG4" , "Comb_TA")
  
  for (e in 1:ncol(fc.all.test)) {
    mape.fc.err <- mape(split.test.ensemble.mat[,1],fc.all.test[,e])
    fc.frame.mape[y,e] <- mape.fc.err
  }
  
  colnames(fc.frame.mape)= colnames(fc.frame.rmse) <- c("Meanf" , "Croston" , "Ses" , "Forecast" , "Thetaf" , 
                                                        "Cubist" , "Lars" , "PPR" , "Ranger" , "Glmnet")
  
  mape.data <- as.data.frame(cbind(fc.frame.mape , ensemble.frame.mape))
  rownames(mape.data)[y]= rownames(rmse.data)[y] <- upc_names[y]

  all.err <- list(as.data.frame(mape.data) , as.data.frame(rmse.data))
  names(all.err) <- c("MAPE" , "RMSE")
  
}
rownames(mape.data)= rownames(rmse.data) <- upc_names

save(list = c("mape.data","rmse.data"), file = "EnsembleError2_4.RData")

setwd("C:/Users/acer/Desktop/Datalar2")
files2 <- dir()
mape.all=rmse.all <- NULL
for (g in 1:length(files2)) {
  load(files2[g])
  mape.all <- rbind(mape.all, mape.data)
  rmse.all <- rbind(rmse.all, rmse.data)
  
}
mape.all <- 100*mape.all

names(mape.all) <- c("Mean" , "Croston" , "SES" , "ETS" , "Theta" , 
                     "Cubist" , "LARS" , "PPR" , "RF" , "GLMNET" , "CLS" , "EIG" , "TA")


names(rmse.all) <- c("Mean" , "Croston" , "SES" , "ETS" , "Theta" , 
                     "Cubist" , "LARS" , "PPR" , "RF" , "GLMNET" , "CLS" , "EIG" , "TA")

sorted <- data.frame(Best=NA)
for(k in 1:nrow(rmse.all)){
  aa=sort(rmse.all[k,])[1]
  aa = t(aa)
  colnames(aa) <- "Best"
  sorted <- rbind(sorted, aa)
  

}

sorted = na.omit(sorted)

library(RColorBrewer)
cc<-brewer.pal(7,"Pastel1")
cclegend <- c(cc[1:2],cc[7])
ccc <- c(rep(cc[1],5),rep(cc[2],5),rep(cc[7],3))

###################   Time Plots
plot.ts(mape.all[1:20,1] , col="black" , xlab = "20 UPCs" , ylab = "MAPE" , main = "Plots of Time Series Models"  , ylim=c(40,120))
lines(mape.all[1:20,2] , col="red")
lines(mape.all[1:20,3] , col="blue")
lines(mape.all[1:20,4] , col="orange")
lines(mape.all[1:20,5] , col="green")
legend("topleft", inset=.0001, title="Names of Models",
       c("Mean","Croston","Ses","ETS","Theta"), fill=c("black","red","blue","orange","green"), horiz=TRUE, cex=0.6 , bty = "n")
####################   Machine 
plot.ts(mape.all[1:20,6] , col="black" , xlab = "20 UPCs" , ylab = "MAPE" , main = "Plots of Machine Learning Models" , ylim=c(0.3,1.2))
lines(mape.all[1:20,7] , col="red")
lines(mape.all[1:20,8] , col="blue")
lines(mape.all[1:20,9] , col="orange")
lines(mape.all[1:20,10] , col="green")
legend("topleft", inset=.0001, title="Names of Models",
       c("Cubist","LARS","PPR","Random Forest","Glmnet"), fill=c("black","red","blue","orange","green"), horiz=FALSE, cex=0.6 , bty = "n")

########################   Ensemble
plot.ts(mape.all[1:20,11] , col="black" , xlab = "20 UPCs" , ylab = "MAPE" , main = "Plots of Ensemble Models" , ylim=c(0.3,1))
lines(mape.all[1:20,12] , col="red")
lines(mape.all[1:20,13] , col="blue")
legend("topleft", inset=.0001, title="Names of Models",
       c("CLS","EIG","TA"), fill=c("black","red","blue"), horiz=FALSE, cex=0.6 , bty = "n")

average <- matrix(NA, nrow=nrow(mape.all), ncol=3)

for (v in 1:nrow(mape.all)) {
  
  ########## Time Mean
  average[v,1] <- rowMeans(mape.all[v,1:5])
  
  ########## Machine mean
  average[v,2] <- rowMeans(mape.all[v,6:10])
  
  ########## Machine mean
  average[v,3] <- rowMeans(mape.all[v,11:13])
}

colnames(average) <- c("Mean of Time Series Models" , "Mean of Machine Learning Models" , "Mean of Ensemble Models")
rownames(average) <- rownames(mape.all)

average <- average[-19,]


plot.ts(average[1:19,1] , col=cc[1] , xlab = "20 UPCs" , ylab = "MAPE [%]" , main = "Average MAPE of each model family for each UPC" , ylim=c(30,80) 
        , cex.lab = 1.5 , cex.main = 1.8)
lines(average[1:19,2] , col=cc[2])
lines(average[1:19,3] , col=cc[7])
legend("topleft", inset=.0001, 
       c("Time Series Models","Machine Learning Models" , "Ensemble Models"), fill=c("black","blue","red"), horiz=FALSE , cex=0.7 , bty = "n")

########################################################################3


par(mfrow=c(2,2) , cex.lab = 1.8 , cex.main = 1.8)

plot.ts(df[5427657:5427676,4] , ylab = "Sales" , xlab = "Weeks" , main = "Sale amount for 'SUNSWEET PRUNE
        JCE W 40 OZ' for a store" , ylim = c(0,25))
plot.ts(df[5223410:5223429,4] , ylab = "Sales" , xlab = "Weeks" , main = "Sale amount for 'REALEMON PLASTIC LEM 
        4.5 OZ' for a store" , ylim = c(0,25))
plot.ts(df[1915641:1915660,4] , ylab = "Sales" , xlab = "Weeks" , main = "Sale amount for 'O S PINK GRAPEFRUIT 
        64 OZ' for a store" , ylim = c(0,25))
plot.ts(df[3532901:3532920,4] , ylab = "Sales" , xlab = "Weeks" , main = "Sale amount for 'WELCHS WHITE GRAPE J	 
        64 OZ' for a store" , ylim = c(0,25) )
########################### box plot mape



boxplot(mape.all[-19,] , ylim = c(30,95), ylab = "MAPE [%]" , xlab = "Model Names" , main = "MAPE comparison of all models" ,
        col = ccc , cex.lab = 1.5 , cex.main = 1.8)


legend("topleft", inset=.0001,
       c("Time Series Models","Machine Learning Models" , "Ensemble Models"), fill=cclegend, horiz=FALSE 
       , cex=0.6 , bty = "n" , y.intersp = 0.8)


########################### box plot rmse


boxplot(rmse.all[-19,] , ylim = c(0,18), ylab = "RMSE" , xlab = "Model Names" , main = "RMSE comparison of all models" ,
        col = ccc , cex.lab = 1.5 , cex.main = 1.8)


legend("topleft", inset=.0001,
       c("Time Series Models","Machine Learning Models" , "Ensemble Models"), fill=cclegend, horiz=FALSE 
       , cex=0.6 , bty = "n" , y.intersp = 0.8)

############################ pie plot


asd<-data.frame(method=c("CLS","RF","GLMNET        ","PPR","         EIG"), freq=c(8,3,1,5,3))

pie3D(asd$freq,labels=paste0(asd$method , c(" 40%" , " \n 15%"," \n 5%"," 25%"," 15%")),explode=0.1,
      main="Frequency of the most accurate models " , col = c(cc[7], cc[2],cc[2],cc[2],cc[7]), cex.lab = 1.8 , cex.main = 2 )

legend("topleft", inset=.0001,
       c("Machine Learning Models" , "Ensemble Models"), fill=c(cc[2],cc[7]), horiz=FALSE , cex=0.6 , bty = "n")


library(RColorBrewer)
cc<-brewer.pal(7,"Pastel1")
cclegend <- c(cc[1:2],cc[7])
ccc <- c(rep(cc[1],5),rep(cc[2],5),rep(cc[7],3))

         