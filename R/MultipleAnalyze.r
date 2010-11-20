#Data Analysis for Multiple Batches
library(stats4)
library(MASS)
library(reshape) 
#predict req. stats4 package
#confidence interval req. MASS package

MultipleAnalyze<-function(Multipledata, Uper, Lper, separateWindows=TRUE)
{
onesidedlo = FALSE
onesidedup = FALSE
twosided   = FALSE
noPX = FALSE
noPY = FALSE
noSolution = FALSE
PPX<-0
PPY<-0

filepath<-getwd()
cat("\n")
outputfile <- readline("Enter the output filename with no extension: ")
outputfile <- paste(outputfile,".txt",sep="")
cat("*******************************************************************\n\n")
cat(" Note: The output file (",outputfile,") will be created \n")
cat(" and placed at",filepath,"\n")
cat("*******************************************************************\n\n")

file.menu <- c("the one-sided lower LC analysis",
               "the one-sided upper LC analysis",
               "the two-sided analysis")
cat("\n")
pick <- menu(file.menu, title = "<< Stability analysis for Multiple batches >> ", graphics=TRUE)
if (pick == 1){
    cat("\n")
    onesidedlo = TRUE
    cat("\n ** The one-sided lower LC analysis is selected.** \n")
              }
else {
      if (pick == 2){
          cat("\n")
          onesidedup = TRUE
          cat("\n ** The one-sided upper LC analysis is selected.** \n")
                    }
      else {
            if (pick == 3){
                cat("\n")
                twosided = TRUE
                cat("\n ** The two-sided LC analysis is selected.** \n")
              }
            }
      }
         
cat("\n")
if(onesidedlo){
   cat("\n")
   lowerstr<- readline("Enter the lower limit = ____ % of label claim: ")
   Lper<-as.numeric(lowerstr)
   cat("\n")
              }
 else {
    if(onesidedup){
      upperstr<- readline("Enter the upper limit = ____ % of label claim: ")
      Uper <- as.numeric(upperstr)
      cat("\n")
                 }
     else{
      upperstr<- readline("Enter the upper limit = ____ % of label claim: ")
      Uper <- as.numeric(upperstr)
      cat("\n")
      lowerstr<- readline("Enter the lower limit = ____ % of label claim: ")
      Lper<-as.numeric(lowerstr)
      cat("\n")
           }
       }
zz <- file(outputfile, open="wt")
sink(zz)
cat("\n")
cat("------------------ stab for R v0.1.3 -------------------\n")
cat("\n developed by Hsin-ya Lee and Yung-jin Lee, 2007-2010.\n")
cat("\n generated on",date(),"\n")
cat("\n\n")
cat("<< --- List of input data --- >>\n\n")
show(Multipledata)
cat("\n\n")
cat(" Analysis settings for multiple batches:\n")
cat(" ---------------------------------------------\n")
if (onesidedlo){
cat(" The lower acceptance limit is set to",Lper,"%.\n\n")
}
else {
  if (onesidedup) {
     cat(" The upper acceptance limit is set to",Uper,"%.\n\n")
     }
  else {
     cat(" The lower acceptance limit is set to",Lper,"%.\n")
     cat(" The upper acceptance limit is set to",Uper,"%.\n\n")
    }
}

#fitting data with ANCOVA model to test poolability
cat("<<Output: ANCOVA model: batch vs. time vs. assay (%)>>\n\n")
cat("\n")
#need to make batch into a factor
ANCOVAdata <- data.frame (batch=factor(Multipledata$batch), 
                        time=Multipledata$time, 
                        assay=Multipledata$assay)

kk<- lm(assay ~ batch*time, data=ANCOVAdata)
show(anova(kk))
Pintercept <- anova(kk)[,5][1]
Pslope <- anova(kk)[,5][3]
cat("\n\n")
xtab<-data.frame(c("Intercept", "Slope"),c(Pintercept,Pslope))
colnames(xtab)<-c("Type","P values")
show(xtab)
cat("--------------------------\n")
cat("at a sig. level of 0.25.\n\n")
##testing for poolability of batches
if ((Pslope >=0.25) && (Pintercept >= 0.25)){
#pool all data to get a slope and a intercept

cat("--------------------------------------------------------------------------\n")
cat("          << ANCOVA Output: Testing for poolability of batches >>         \n")
cat("--------------------------------------------------------------------------\n")
cat("                                                                          \n")
cat(" The tests for equality of slopes and equality of intercepts are not      \n")
cat(" significant at a level of 0.25 (there is no significant difference       \n")
cat(" in slope and intercepts among the batches).                              \n")
cat("                                                                          \n")
if(onesidedlo){
cat("               <<Model #1: one-sided lower LC analysis>>                  \n")
              }
else {
   if (onesidedup){
cat("               <<Model #1: one-sided upper LC analysis>>                  \n")
                  }
   else {
cat("                  <<Model #1: two-sided LC analysis>>                     \n")
        }
}
cat("             common intercept and common slope among batches.             \n")
cat("------------------------------------------------------------------------\n\n")
cat("              <<linear regression model: Assay (%) vs. time>>             \n")
cat("\n")
show(lm(assay ~ time, data=ANCOVAdata))
show(anova(lm(assay ~ time, data=ANCOVAdata)))
cat("\n")
cat("**************************************************************************\n")
cat("                               << Output >>                               \n")
cat("--------------------------------------------------------------------------\n")
cat("                    <<Summary: linear regression model>>                \n\n")
W.split<-split(ANCOVAdata, list(ANCOVAdata$batch))
  ba<-0
     for (i in 1:(length(W.split))){
     ba[i]<-W.split[[i]][["batch"]][1]
     }
     
#collect all intercepts in a dataframe
     Intable<-data.frame(batch=ba)

#step1:get a slope and a intercept
Intercept<-coef(lm(assay ~ time, data=ANCOVAdata))[1]
Slope<-coef(lm(assay ~ time, data=ANCOVAdata))[2]

#step2: calculate description statistics
#Calculated assay
   cal<-predict(lm(assay ~ time, data=ANCOVAdata))
#extract model residuals
   Res<-residuals(lm(assay ~ time, data=ANCOVAdata))
#number of data points
    L<-length(ANCOVAdata$assay)
#Residual SE (time, assay, time*assay)
#sum of (Xi-Xmean)^2
    Sxx<-var(ANCOVAdata$time)*(L-1)
#sum of (Yi-Ymean)^2
    Syy<-var(ANCOVAdata$assay)*(L-1)
#sum of (Xi-Xmean)* (Yi-Ymean)
    Sxxyy<-0
    for(i in 1:length(ANCOVAdata$time)){
    Sxxyy[i]<-((ANCOVAdata$assay[i]-mean(ANCOVAdata$assay))*
    (ANCOVAdata$time[i]-mean(ANCOVAdata$time)))
     }
    Sxy<-sum(Sxxyy)
  #KK:(yi-(Intercept+Slope*Xi))^2 , KKK: sum of KK, SK:sqrt(KKK/(L-2))
    KK<-0
    for(i in 1:length(ANCOVAdata$time)){
    KK[i]<-(ANCOVAdata$assay[i]-(Intercept+Slope*ANCOVAdata$time[i]))^2
     }
    KKK<-sum(KK)
    SK<-sqrt(KKK/(L-2))
  #Xi: sum of (Xii)^2
    Xii<-0
    for(i in 1:length(ANCOVAdata$time)){
    Xii[i]<-(ANCOVAdata$time[i])^2
      }
    Xi<-sum(Xii)
 
  #XYi: sum of (Xi*Yi)
    XYii<-0
    for(i in 1:length(ANCOVAdata$time)){
    XYii[i]<-(ANCOVAdata$time[i])*(ANCOVAdata$assay[i])
     }
    XYi<-sum(XYii)
#95%CI,n-2 T value
    if(onesidedlo || onesidedup){
       T<-qt(0.95,L-2)}
    else {
       T<-qt(0.975,L-2)}
       
      #Slope: (Xi/(L*Sxx))*Yi+ (-mean(Singledata$time)/Sxx)*XYi
      #Intercept: (-mean(Singledata$time)/Sxx)*Yi+ (1/Sxx)*XYi
      #calculate shelf life
        if (onesidedlo) {
           #intersect 95% CI with Lower criteria
             delta<-Intercept-Lper
             a2<-(1/Sxx)-(Slope/(T*SK))^2
             b2<-2*(-mean(ANCOVAdata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
             c2<-(Xi/(L*Sxx))-(delta/(T*SK))^2
             Y1<-((-b2)+(sqrt((b2)^2-(4*a2*c2))))/(2*a2)
             Y2<-((-b2)-(sqrt((b2)^2-(4*a2*c2))))/(2*a2)
                        }
        if (onesidedup) {
           #intersect 95% CI with Upper criteria
             delta<-Intercept-Uper
             a1<-(1/Sxx)-(Slope/(T*SK))^2
             b1<-2*(-mean(ANCOVAdata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
             c1<-(Xi/(L*Sxx))-(delta/(T*SK))^2
             X1<-((-b1)+(sqrt((b1)^2-(4*a1*c1))))/(2*a1)
             X2<-((-b1)-(sqrt((b1)^2-(4*a1*c1))))/(2*a1)
                        }
        if (twosided) {
           #intersect 95% CI with Upper criteria
             delta<-Intercept-Uper
             a1<-(1/Sxx)-(Slope/(T*SK))^2
             b1<-2*(-mean(ANCOVAdata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
             c1<-(Xi/(L*Sxx))-(delta/(T*SK))^2
             X1<-((-b1)+(sqrt((b1)^2-(4*a1*c1))))/(2*a1)
             X2<-((-b1)-(sqrt((b1)^2-(4*a1*c1))))/(2*a1)
           
           #intersect 95% CI with Lower criteria
             delta<-Intercept-Lper
             a2<-(1/Sxx)-(Slope/(T*SK))^2
             b2<-2*(-mean(ANCOVAdata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
             c2<-(Xi/(L*Sxx))-(delta/(T*SK))^2
             Y1<-((-b2)+(sqrt((b2)^2-(4*a2*c2))))/(2*a2)
             Y2<-((-b2)-(sqrt((b2)^2-(4*a2*c2))))/(2*a2)
        }

#make decision
if(onesidedlo){
if ((b2)^2-(4*a2*c2)>=0){
  if ((a2>0 && b2>0 && c2>0) || (a2<0 && b2<0 && c2<0)){
       PY<-c(1000000000000)
        }
  else {
   if((a2>0 && b2>0 && c2<0) || (a2<0 && b2>0 && c2>0) || (a2>0 && b2<0 && c2<0)||
      (a2<0 && b2<0 && c2>0)){
       if ((Y1<= 0)) Y1<-c(1000000000000)
       if ((Y2<= 0)) Y2<-c(1000000000000)
       DF<-data.frame(Y=c(Y1,Y2))
       PY<-min(DF)
        }
   else {
       DF<-data.frame(Y=c(Y1,Y2))
       PY<-min(DF)
        }
     }
   }
else {
      PY<-c(1000000000000)
     }
     if ((PY==1000000000000)) noPY = TRUE
}
#######
if(onesidedup){
if ((b1)^2-(4*a1*c1)>=0){
  if ((a1>0 && b1>0 && c1>0) || (a1<0 && b1<0 && c1<0)){
       PX<-c(1000000000000)
        }
  else {
   if((a1>0 && b1>0 && c1<0) || (a1<0 && b1>0 && c1>0) || (a1>0 && b1<0 && c1<0)||
      (a1<0 && b1<0 && c1>0)){
       if ((X1<= 0)) X1<-c(1000000000000)
       if ((X2<= 0)) X2<-c(1000000000000)
       DF<-data.frame(X=c(X1,X2))
       PX<-min(DF)
        }
   else {
      DF<-data.frame(X=c(X1,X2))
      PX<-min(DF)
        }
     }
   }
else {
      PX<-c(1000000000000)
     }
     if ((PX==1000000000000)) noPX = TRUE
}
#######
if(twosided){
if ((((b1)^2-(4*a1*c1))>=0) && (((b2)^2-(4*a2*c2))>=0)){
  if ((a1>0 && b1>0 && c1>0) || (a1<0 && b1<0 && c1<0)){
      PX<-c(1000000000000)

      }
  else{
  if ((a1>0 && b1>0 && c1<0) || (a1<0 && b1>0 && c1>0) || (a1>0 && b1<0 && c1<0)||
      (a1<0 && b1<0 && c1>0)){
      if ((X1<= 0)) X1<-c(1000000000000)
      if ((X2<= 0)) X2<-c(1000000000000)
      DF<-data.frame(X=c(X1,X2))
      PX<-min(DF)

      }
  else{
    if ((a1>0 && b1<0 && c1>0) ||(a1<0 && b1>0 && c1<0)){
      DF<-data.frame(X=c(X1,X2))
      PX<-min(DF)
      
       }
      }
    }
  if ((a2>0 && b2>0 && c2>0) || (a2<0 && b2<0 && c2<0)){
       PY<-c(1000000000000)

        }
  else{
   if((a2>0 && b2>0 && c2<0) || (a2<0 && b2>0 && c2>0) || (a2>0 && b2<0 && c2<0)||
      (a2<0 && b2<0 && c2>0)){
       if ((Y1<= 0)) Y1<-c(1000000000000)
       if ((Y2<= 0)) Y2<-c(1000000000000)
       DF<-data.frame(Y=c(Y1,Y2))
       PY<-min(DF)
        
        }
   else {
      if ((a2>0 && b2<0 && c2>0) ||(a2<0 && b2>0 && c2<0)){
      DF<-data.frame(Y=c(Y1,Y2))
      PY<-min(DF)
       
       }
      }
    }
   }

else {
if ((((b1)^2-(4*a1*c1))>=0) && (((b2)^2-(4*a2*c2))<0)){
  if ((a1>0 && b1>0 && c1>0) || (a1<0 && b1<0 && c1<0)){
      PY<-c(1000000000000)
      PX<-c(1000000000000)
      
      }
  else{
  if ((a1>0 && b1>0 && c1<0) || (a1<0 && b1>0 && c1>0) || (a1>0 && b1<0 && c1<0)||
      (a1<0 && b1<0 && c1>0)){
      if ((X1<= 0)) X1<-c(1000000000000)
      if ((X2<= 0)) X2<-c(1000000000000)
      DF<-data.frame(X=c(X1,X2))
      PX<-min(DF)
      PY<-c(1000000000000)
      
      }
  else{
    if ((a1>0 && b1<0 && c1>0) ||(a1<0 && b1>0 && c1<0)){
      DF<-data.frame(X=c(X1,X2))
      PX<-min(DF)
      PY<-c(1000000000000)
      
       }
      }
    }
   }

else {
if ((((b1)^2-(4*a1*c1))< 0) && (((b2)^2-(4*a2*c2))>=0)){
  if ((a2>0 && b2>0 && c2>0) || (a2<0 && b2<0 && c2<0)){
      PY<-c(1000000000000)
      PX<-c(1000000000000)

        }
   else{
   if((a2>0 && b2>0 && c2<0) || (a2<0 && b2>0 && c2>0) || (a2>0 && b2<0 && c2<0)||
      (a2<0 && b2<0 && c2>0)){
       if ((Y1<= 0)) Y1<-c(1000000000000)
       if ((Y2<= 0)) Y2<-c(1000000000000)
       DF<-data.frame(Y=c(Y1,Y2))
       PY<-min(DF)
       PX<-c(1000000000000)

        }
   else {
      if ((a2>0 && b2<0 && c2>0) ||(a2<0 && b2>0 && c2<0)){
      DF<-data.frame(Y=c(Y1,Y2))
      PY<-min(DF)
      PX<-c(1000000000000)
       
       }
      }
     }
    }
else {
if ((((b1)^2-(4*a1*c1))< 0) && (((b2)^2-(4*a2*c2))<0)){
     PY<-c(1000000000000)
     PX<-c(1000000000000)

    }
   }
  }
 }
 if ((PY==1000000000000)) noPY = TRUE
 if ((PX==1000000000000)) noPX = TRUE
}
#########
#Output
cat("\nY =",coef(lm(ANCOVAdata$assay~ANCOVAdata$time))[1],"+(",coef(lm(ANCOVAdata$assay~ANCOVAdata$time))[2],") X\n\n")
cat("\n")
output<-data.frame(ANCOVAdata$batch,ANCOVAdata$time,ANCOVAdata$assay,cal,Res)
colnames(output)<-list("Batch#","Time","Obs. assay(%)","Cal. assay(%)","Residuals")
show(output)
cat("\n\n")

##### if one-sided low, start from here
if (onesidedlo) {
cat("                       One-sided lower LC analysis                      \n\n")
if (noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
   noSolution = TRUE
}
else {
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf life\n\n")
     cat("\n\n")
     windows(record = TRUE )  ### now start preparing to plot...
     #i<-formatC(PX,format="f",digits=2) 
     #i<-round(PX,3) #same as the above 
     shelflife<-as.integer(PY)
     main<-paste(c("Shelf life =",shelflife, "months/weeks (PgDn to switch plots)"),collapse=" ")
     x<-ANCOVAdata$time
     y<-ANCOVAdata$assay
     plot(x,y,xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Lper-10),(Lper+30)),main=main,
     xlab = "Time (months/weeks)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,(2.0*max(ANCOVAdata$time)))
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),
     level = 0.90,type="response")
     lines(newx,prd[,2],col="red",lty=2)
###
### no legend here; data have been combined all together.
#show 95 %CI
     total<-data.frame(time=newx, fit=prd[,1], Lower=prd[,2])
     cat("\n")
     show(total)
     cat("\n")
     #add criteria limit
     abline(h=Lper, col = "gray60")
     abline(v=PY, col = "gray60")
     cat("------------------------------------------------------------------\n\n")
     cat(" Drug product with lower acceptance limit of",Lper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                          \n\n")
     cat("******************************************************************\n")
     cat("\n")
      }     
}
##### end of one-sided low
##### if one-sided upper, start from here
if (onesidedup ){
cat("\n")
cat("                     One-sided upper LC analysis                        \n\n")
if (noPX){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
    noSolution = TRUE
}
else { 
     allSF<-data.frame(ba,PX)
     colnames(allSF)<-c(" batch#", "  shelf life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf life\n\n")
     cat("\n\n")
     windows(record = TRUE )  ### now start preparing to plot...
     #i<-formatC(PX,format="f",digits=2) 
     #i<-round(PX,3) #same as the above 
     shelflife<-as.integer(PX)
     main<-paste(c("Shelf life =",shelflife, "months/weeks (PgDn to switch plots)"),collapse=" ")
     x<-ANCOVAdata$time
     y<-ANCOVAdata$assay
     plot(x,y,xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Uper-30),(Uper+10)),main=main,
     xlab = "Time (months/weeks)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,(2.0*max(ANCOVAdata$time)))
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),
     level = 0.90,type="response")
     lines(newx,prd[,3],col="red",lty=2)
     
#show 95 %CI
     total<-data.frame(time=newx, fit=prd[,1], Upper=prd[,3])
     cat("\n")
     show(total)
     cat("\n")
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(v=PX, col = "gray60")
     cat("------------------------------------------------------------------\n\n")
     cat(" Drug product with upper acceptance limiy of",Uper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                           \n\n")
     cat("******************************************************************\n")
     cat("\n")
      }     
}
##### end of one-sided upper
##### if two-sided, start from here
if (twosided){
cat("                        Two-sided LC analysis                           \n\n")
if (noPX && noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
    noSolution = TRUE
}
else {
if  (PX>=PY)  {
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf life\n\n")
     cat("\n\n")
     windows(record = TRUE )  ### now start preparing to plot...
     #go to plot of single batch
     #i<-formatC(PY,format="f",digits=2)    # HY used this statement. YJ
     #i<-round(PY,3)
     shelflife<-as.integer(PY)
     main<-paste(c("Shelf life =",shelflife, "months/weeks (PgDn to switch plots)"),collapse=" ")
     x<-ANCOVAdata$time
     y<-ANCOVAdata$assay
     plot(x,y,xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months/weeks)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,(2.0*max(ANCOVAdata$time)))
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),
     level = 0.95,type="response")
     lines(newx,prd[,2],col="red",lty=2)
     lines(newx,prd[,3],col="red",lty=2)
     
     #show 95 %CI
     total<-data.frame(time=newx, fit=prd[,1], Lower=prd[,2], Upper=prd[,3])
     cat("\n")
     show(total)
     cat("\n")
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PY, col = "gray60")
     cat("------------------------------------------------------------------\n\n")
     cat(" Drug product with lower acceptance limit of",Lper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                          \n\n")
     cat("******************************************************************\n")
     cat("\n")
       }
else {
    if(PY>PX){
      allSF<-data.frame(ba,PX)
      colnames(allSF)<-c(" batch#", "  shelf life*")
      show(allSF)
      cat("-------------------------\n")
      cat("*: estimated shelf life\n\n")
      cat("\n\n")
      windows(record = TRUE )  ### now start preparing to plot...
     #i<-formatC(PX,format="f",digits=2) 
     #i<-round(PX,3) #same as the above 
     shelflife<-as.integer(PX)
     main<-paste(c("Shelf life =",shelflife, "months/weeks (PgDn to switch plots)"),collapse=" ")
     x<-ANCOVAdata$time
     y<-ANCOVAdata$assay
     plot(x,y,xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Lper-10),(Uper+10)),main=main,
     xlab = "Time (months/weeks)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,(2.0*max(ANCOVAdata$time)))
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),
     level = 0.95,type="response")
     lines(newx,prd[,2],col="red",lty=2)
     lines(newx,prd[,3],col="red",lty=2)
     
#show 95 %CI
     total<-data.frame(time=newx, fit=prd[,1], Lower=prd[,2], Upper=prd[,3])
     cat("\n")
     show(total)
     cat("\n")
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PX, col = "gray60")
     cat("----------------------------------------------------------------\n\n")
     cat(" Drug product with upper acceptance limit of",Uper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
      }     
     }
   }
}
### end of two-sided.
### do Q-Q plot only if there is at leat one solution (shelf life)
if (!noSolution) {
    qqnorm(output$Res, las=1, main = "Normal Q-Q Plot of Residuals (PgUp to switch plots)", 
           col=c(ANCOVAdata$batch))
      
         LLegend<-paste("batch#",c(Intable$batch))
         temp <- legend("topleft", legend = LLegend,
               text.width = strwidth("10000000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)
      }
    sink()
       cat("*****************************************************************************\n\n")
       cat("## Please note: The output file (",outputfile,") has been created \n")
       cat("   and placed at",filepath,"\n\n")
       cat("*****************************************************************************\n\n")     
    bye()
}

###########################################################################################

else {
  if ((Pslope >=0.25) && (Pintercept < 0.25)){
  #pool all data to get a slope with different intercepts 

cat("--------------------------------------------------------------------------\n")
cat("          << ANCOVA Output: Testing for poolability of batches >>         \n")
cat("--------------------------------------------------------------------------\n")
cat("                                                                          \n")
cat(" The test rejects the hypothesis of equality of intercepts but fails to   \n")
cat(" reject that the slopes are equal (there is a significant difference in   \n")
cat(" intercepts but no significant difference in slopes among the batches).   \n")
cat("                                                                          \n")
if(onesidedlo){
cat("               <<Model #2: one-sided lower LC analysis>>                  \n")
              }
else {
   if (onesidedup){
cat("               <<Model #2: one-sided upper LC analysis>>                  \n")
                  }
   else {
cat("                  <<Model #2: two-sided LC analysis>>                     \n")
        }
}
cat("         separate intercepts with a common slope among batches.           \n")
cat("------------------------------------------------------------------------\n\n")
cat("              <<linear regression model: Assay (%) vs. time>>             \n")
cat("\n")
show(lm(assay ~ batch+time, data=ANCOVAdata) )
show(anova(lm(assay ~ batch+time, data=ANCOVAdata) ))
cat("\n")
cat("**************************************************************************\n")
cat("                               << Output >>                               \n")
cat("--------------------------------------------------------------------------\n")
cat("                    <<Summary: linear regression model>>                \n\n")

W.split<-split(ANCOVAdata, list(ANCOVAdata$batch))
K.split<-split(Multipledata, list(Multipledata$batch)) 
#step1: catch different intercepts and a slope from lm 
   prepreIntercept<-0
     for (i in 1:(length(W.split))){
     prepreIntercept[i]<-coef(lm(assay ~ batch+time , data=ANCOVAdata) )[i]
     }
     preIntercept<-0
     for(i in 1:((length(W.split))-1)){
     preIntercept[i]<-prepreIntercept[1]+ prepreIntercept[i+1]
     }
  
   ba<-0
     for (i in 1:(length(K.split))){
     ba[i]<-K.split[[i]][["batch"]][1]
     }
    Slope<-0
    d<-length(W.split)
    Slope<-coef(lm(assay ~ batch+time, data=ANCOVAdata))[d+1]
### cat("the common slope:", Slope,"\n")           # to confirm if the slope is correct.

   #collect all intercepts in a dataframe
   ba1<- na.omit(ba)
   #show(ba1)
   Intable<-data.frame(batch=ba, Intercept=c(prepreIntercept[1],preIntercept))  

#step2: calculate description statistics
     #divide data in to different group based on batches
		     CAL<-NULL 
		     TIME<-NULL
		     RRES<-NULL 
		     OBS<-NULL
		     cal<-0
		     Res<-0 
		     L<-0
		     LL<-0
		     Sxx<-0
		     Syy<-0
		     Sxy<-0 
		     KKK<-0
		     SS<-0
		     SK<-0
		     Xi<-0
		     XYi<-0
		     T<-0 
		     delta1<-0
		     delta2<-0
		     a1<-0
		     b1<-0
		     c1<-0
		     X1<-0
		     X2<-0
		     a2<-0
		     b2<-0
		     c2<-0
		     Y1<-0
		     Y2<-0
		     PX<-0
		     PY<-0 
		     TT<-0
     for (j in 1:length(W.split)){
          Intercept<-0
          for(x in 1: length(unique(Intable$batch))){
                if (W.split[[j]][["batch"]][1]==Intable$batch[[x]]){
                  Intercept<- Intable$Intercept[[x]]}
          } 
          cal<-0
          Res<-0
          Sxxyy<-0
          KK<-0 
          Xii<-0
          XYii<-0
          for (a in 1:length(W.split[[j]][["time"]])){    
          #Calculated assay
            cal[a]<- (((W.split[[j]][["time"]][a])*Slope)+ Intercept) 
          #extract model residuals
            Res[a]<- cal[a]-(W.split[[j]][["assay"]][a])
          #sum of (Xi-Xmean)* (Yi-Ymean)
            Sxxyy[a]<-((W.split[[j]][["assay"]][a]-mean(W.split[[j]][["assay"]]))*
           (W.split[[j]][["time"]][a]-mean(W.split[[j]][["time"]])))
          #KK:(yi-(Intercept+Slope*Xi))^2 , KKK: sum 0f KK, SK:sqrt(KKK/L-2) 
            KK[a]<-(W.split[[j]][["assay"]][a]-(Intercept+Slope*W.split[[j]][["time"]][a]))^2 
          #Xi: sum of (Xi)^2  
            Xii[a]<-(W.split[[j]][["time"]][a])^2
          #XYi: sum of (Xi*Yi)
            XYii[i]<-(W.split[[j]][["time"]][a])*(W.split[[j]][["assay"]][a])
          }

       #number of data points by batch
        L[j]<-length(W.split[[j]][["time"]])
        
       #Residual SE (time, assay, time*assay)
       #sum of (Xi-Xmean)^2
        Sxx[j]<-var(W.split[[j]][["time"]])*(L[j]-1)
        
       #sum of (Yi-Ymean)^2
        Syy[j]<-var(W.split[[j]][["assay"]])*(L[j]-1)
        
       #sum of (Xi-Xmean)* (Yi-Ymean)
        Sxy[j]<-sum(Sxxyy)
        
       # unique SS for Model #2
        SS[j]<-(Syy[j]-(Slope*Sxy[j]))/(L[j]-2)
        
       #KK:(yi-(Intercept+Slope*Xi))^2 , KKK: sum 0f KK, SK:sqrt(KKK/L-2) 
        KKK[j]<-sum(KK)
        SK[j]<-sqrt(KKK[j]/(L[j]-2))
        
       #Xi: sum of (Xi)^2
        Xi[j]<-sum(Xii)
        
       #XYi: sum of (Xi*Yi)
        XYi[j]<-sum(XYii)
        
       #95%CI,n-2 T value
        if(onesidedlo || onesidedup){
           T[j]<-qt(0.95,L[j]-2)}
        else {
           T[j]<-qt(0.975,L[j]-2)}
        
        LL[j]<-length(W.split[[j]][["time"]])   
        TT[j]<-mean(W.split[[j]][["time"]])
       
#step3: calculate possible X1,X2,Y1,Y2   
      #Slope: (Xi/(L*Sxx))*Yi+ (-mean(Singledata$time)/Sxx)*XYi
      #Intercept: (-mean(Singledata$time)/Sxx)*Yi+ (1/Sxx)*XYi
      #calculate shelf life
      #intersect 95% CI with lower/upper criteria
      if(onesidedlo){
        #intersect 95% CI with Lower criteria
        delta2[j]<-Lper-Intercept
        a2[j]<-(Slope*Slope)-(T[j]*T[j]*SS[j])/Sxx[j]
        b2[j]<-2*(T[j]*T[j]*SS[j]*TT[j])/Sxx[j]-(2*Slope*delta2[j])
        c2[j]<-(delta2[j]*delta2[j])-(T[j]*T[j]*SS[j]*TT[j]*TT[j])/Sxx[j] -(T[j]*T[j]*SS[j]/LL[j])
        Y1[j]<-((-b2[j])+(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
        Y2[j]<-((-b2[j])-(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
        }
        else {
          if(onesidedup){
            #intersect 95% CI with Upper criteria
            delta1[j]<-Uper-Intercept
            a1[j]<-(Slope*Slope)-(T[j]*T[j]*SS[j])/Sxx[j]
            b1[j]<-2*(T[j]*T[j]*SS[j]*TT[j])/Sxx[j]-(2*Slope*delta1[j])
            c1[j]<-(delta1[j]*delta1[j])-(T[j]*T[j]*SS[j]*TT[j]*TT[j])/Sxx[j]-(T[j]*T[j]*SS[j]/LL[j])
            X1[j]<-((-b1[j])+(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j])
            X2[j]<-((-b1[j])-(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j])
            }
          else {
            #intersect 95% CI with Upper criteria
            delta1[j]<-Uper-Intercept
            a1[j]<-(Slope*Slope)-(T[j]*T[j]*SS[j])/Sxx[j]
            b1[j]<-2*(T[j]*T[j]*SS[j]*TT[j])/Sxx[j]-(2*Slope*delta1[j])
            c1[j]<-(delta1[j]*delta1[j])-(T[j]*T[j]*SS[j]*TT[j]*TT[j])/Sxx[j]-(T[j]*T[j]*SS[j]/LL[j])
            X1[j]<-((-b1[j])+(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j])
            X2[j]<-((-b1[j])-(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j]) 
              
            #intersect 95% CI with Lower criteria
            delta2[j]<-Lper-Intercept
            a2[j]<-(Slope*Slope)-(T[j]*T[j]*SS[j])/Sxx[j]
            b2[j]<-2*(T[j]*T[j]*SS[j]*TT[j])/Sxx[j]-(2*Slope*delta2[j])
            c2[j]<-(delta2[j]*delta2[j])-(T[j]*T[j]*SS[j]*TT[j]*TT[j])/Sxx[j] -(T[j]*T[j]*SS[j]/LL[j])
            Y1[j]<-((-b2[j])+(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
            Y2[j]<-((-b2[j])-(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
               }
         }            
   
#step4: make decision
######
if(onesidedlo){
if ((b2[j])^2-(4*a2[j]*c2[j])>=0){
  if ((a2[j]>0 && b2[j]>0 && c2[j]>0) || (a2[j]<0 && b2[j]<0 && c2[j]<0)){
       PY[j]<-c(1000000000000)
        }
  else {
   if((a2[j]>0 && b2[j]>0 && c2[j]<0) || (a2[j]<0 && b2[j]>0 && c2[j]>0) || 
      (a2[j]>0 && b2[j]<0 && c2[j]<0)|| (a2[j]<0 && b2[j]<0 && c2[j]>0)){
       if ((Y1[j]<= 0)) Y1[j]<-c(1000000000000)
       if ((Y2[j]<= 0)) Y2[j]<-c(1000000000000)
       DF<-data.frame(Y=c(Y1[j],Y2[j]))
       PY[j]<-min(DF)

        }
   else {
      DF<-data.frame(Y=c(Y1[j],Y2[j]))
      PY[j]<-min(DF)
        }
     }
   }
else {
      PY[j]<-c(1000000000000)
     }
}
#######
if(onesidedup){
if ((b1[j])^2-(4*a1[j]*c1[j])>=0){
  if ((a1[j]>0 && b1[j]>0 && c1[j]>0) || (a1[j]<0 && b1[j]<0 && c1[j]<0)){
       PX<-c(1000000000000)
        }
  else {
   if((a1[j]>0 && b1[j]>0 && c1[j]<0) || (a1[j]<0 && b1[j]>0 && c1[j]>0) || 
      (a1[j]>0 && b1[j]<0 && c1[j]<0)|| (a1[j]<0 && b1[j]<0 && c1[j]>0)){
       if ((X1[j]<= 0)) X1[j]<-c(1000000000000)
       if ((X2[j]<= 0)) X2[j]<-c(1000000000000)
       DF<-data.frame(X=c(X1[j],X2[j]))
       PX[j]<-min(DF)
        }
   else {
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-min(DF)
        }
     }
   }
else {
      PX<-c(1000000000000)
     }
}
#######
if(twosided){
if ((((b1[j])^2-(4*a1[j]*c1[j]))>=0) && (((b2[j])^2-(4*a2[j]*c2[j]))>=0)){
  if ((a1[j]>0 && b1[j]>0 && c1[j]>0) || (a1[j]<0 && b1[j]<0 && c1[j]<0)){
      PX<-c(1000000000000)

      }
  else{
  if ((a1[j]>0 && b1[j]>0 && c1[j]<0) || (a1[j]<0 && b1[j]>0 && c1[j]>0) || 
      (a1[j]>0 && b1[j]<0 && c1[j]<0)|| (a1[j]<0 && b1[j]<0 && c1[j]>0)){
      if ((X1[j]<= 0)) X1[j]<-c(1000000000000)
      if ((X2[j]<= 0)) X2[j]<-c(1000000000000)
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-min(DF)

      }
  else{
    if ((a1[j]>0 && b1[j]<0 && c1[j]>0) ||(a1[j]<0 && b1[j]>0 && c1[j]<0)){
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-min(DF)
      
       }
      }
    }
  if ((a2[j]>0 && b2[j]>0 && c2[j]>0) || (a2[j]<0 && b2[j]<0 && c2[j]<0)){
       PY[j]<-c(1000000000000)

        }
  else{
   if((a2[j]>0 && b2[j]>0 && c2[j]<0) || (a2[j]<0 && b2[j]>0 && c2[j]>0) || 
      (a2[j]>0 && b2[j]<0 && c2[j]<0)|| (a2[j]<0 && b2[j]<0 && c2[j]>0)){
       if ((Y1[j]<= 0)) Y1[j]<-c(1000000000000)
       if ((Y2[j]<= 0)) Y2[j]<-c(1000000000000)
       DF<-data.frame(Y=c(Y1[j],Y2[j]))
       PY[j]<-min(DF)
        
        }
   else {
      if ((a2[j]>0 && b2[j]<0 && c2[j]>0) ||(a2[j]<0 && b2[j]>0 && c2[j]<0)){
      DF<-data.frame(Y=c(Y1[j],Y2[j]))
      PY[j]<-min(DF)
       
       }
      }
    }
   }

else {
if ((((b1[j])^2-(4*a1[j]*c1[j]))>=0) && (((b2[j])^2-(4*a2[j]*c2[j]))<0)){
  if ((a1[j]>0 && b1[j]>0 && c1[j]>0) || (a1[j]<0 && b1[j]<0 && c1[j]<0)){
      PY[j]<-c(1000000000000)
      PX[j]<-c(1000000000000)
      
      }
  else{
  if ((a1[j]>0 && b1[j]>0 && c1[j]<0) || (a1[j]<0 && b1[j]>0 && c1[j]>0) || 
      (a1[j]>0 && b1[j]<0 && c1[j]<0)|| (a1[j]<0 && b1[j]<0 && c1[j]>0)){
      if ((X1[j]<= 0)) X1[j]<-c(1000000000000)
      if ((X2[j]<= 0)) X2[j]<-c(1000000000000)
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-min(DF)
      PY[j]<-c(1000000000000)
      
      }
  else{
    if ((a1[j]>0 && b1[j]<0 && c1[j]>0) ||(a1[j]<0 && b1[j]>0 && c1[j]<0)){
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-min(DF)
      PY[j]<-c(1000000000000)
      
       }
      }
    }
   }

else {
if ((((b1[j])^2-(4*a1[j]*c1[j]))< 0) && (((b2[j])^2-(4*a2[j]*c2[j]))>=0)){
  if ((a2[j]>0 && b2[j]>0 && c2[j]>0) || (a2[j]<0 && b2[j]<0 && c2[j]<0)){
      PY[j]<-c(1000000000000)
      PX[j]<-c(1000000000000)

        }
   else{
   if((a2[j]>0 && b2[j]>0 && c2[j]<0) || (a2[j]<0 && b2[j]>0 && c2[j]>0) ||
      (a2[j]>0 && b2[j]<0 && c2[j]<0)|| (a2[j]<0 && b2[j]<0 && c2[j]>0)){
       if ((Y1[j]<= 0)) Y1[j]<-c(1000000000000)
       if ((Y2[j]<= 0)) Y2[j]<-c(1000000000000)
       DF<-data.frame(Y=c(Y1[j],Y2[j]))
       PY[j]<-min(DF)
       PX[j]<-c(1000000000000)

        }
   else {
      if ((a2[j]>0 && b2[j]<0 && c2[j]>0) ||(a2[j]<0 && b2[j]>0 && c2[j]<0)){
      DF<-data.frame(Y=c(Y1[j],Y2[j]))
      PY[j]<-min(DF)
      PX<-c(1000000000000)
       
       }
      }
     }
    }
else {
if ((((b1[j])^2-(4*a1[j]*c1[j]))< 0) && (((b2[j])^2-(4*a2[j]*c2[j]))<0)){
     PY<-c(1000000000000)
     PX<-c(1000000000000)

    }
   }
  }
 }
}
#########
#step5: Output
cat(" --- Batch#:", ba[j],"---\n")
cat("\nY =",Intercept,"+(",Slope,") X\n\n")
cat("\n")
output<-data.frame(W.split[[j]][["time"]],W.split[[j]][["assay"]],cal,Res)
colnames(output)<-list("Time","Observed assay(%)","Calculated assay(%)","Residuals")  
show(output)
cat("\n\n")  
TIME[[j]]<-c(W.split[[j]][["time"]])
CAL[[j]]<-c(cal)
RRES[[j]]<-c(Res)
OBS[[j]]<-c(W.split[[j]][["assay"]])
}
###cat("show all X1[j]:",X1,"\n")
###cat("show all X2[j]:",X2,"\n")
###cat("show all Y1[j]:",Y1,"\n")
###cat("show all Y2[j]:",Y2,"\n\n")

AA<-melt(CAL)
ZZ<-melt(TIME)
QQ<-melt(RRES)
OO<-melt(OBS)
###c(AA$L1)
####purpose: to plot one slope and three intercepts 
###NewPred<-data.frame(batch=c(AA$L1),time=ZZ$value, PredCal=AA$value, RES=QQ$value)   
###M.split<-split(NewPred, list(NewPred$batch) ) 

#choose min PX or PY
if(onesidedlo){
DM<- data.frame(batch=ba, Lower=PY)
DM<- na.omit(DM)
PPY<-min(DM$Lower)
if ((PPY==1000000000000)) noPY = TRUE
}
else {
 if(onesidedup){
   DM<- data.frame(batch=ba, Upper=PX)
   DM<- na.omit(DM)
   PPX<-min(DM$Upper)}
   if ((PPX==1000000000000)) noPX = TRUE
 else {
   DM<- data.frame(batch=ba, Upper=PX, Lower=PY)
   DM<- na.omit(DM)
   PPX<-min(DM$Upper)
   PPY<-min(DM$Lower)
   if ((PPY==1000000000000)) noPY = TRUE
   if ((PPX==1000000000000)) noPX = TRUE
 }
}
                                       
#step6: summary and plot
cat("**************************************************************************\n")
cat("                       << Summary and plots >>                            \n")
cat("--------------------------------------------------------------------------\n")
cat("\n\n")
##### if one-sided low, start from here
if (onesidedlo) {
cat("                     One-sided lower LC analysis                        \n\n")
if (noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
   noSolution = TRUE
}
else {
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf life\n\n")
     cat("\n\n")
     windows(record = TRUE )  ### now start preparing to plot...
     #go to plot of single batch
     #i<-formatC(PPY,format="f",digits=2) 
     #i<-round(PPY,3)
     #### oultine plot box 
     shelflife<-as.integer(PPY)
     
     main<-paste(c("Shelf life =",shelflife, "months/weeks (PgDn to switch plots)"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata, xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Lper-10),(Lper+30)), main=main,
     xlab = "Time (months/weeks)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
     axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
     axis(1,at=0:100,tcl=-.2, labels=FALSE)
      
     LLegend<-paste("batch#",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend,
               text.width = strwidth("10000000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)

#plot CI
      ## newx<-data.frame(xx=seq(0,(max(ANCOVAdata$time)+10)))
      newx<-data.frame(xx=seq(0,(2.0*max(ANCOVAdata$time)))) 
      for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)
      pred<-predict(mod, newdata=newx,interval = c("confidence"),level = 0.90,type="response")
      lines(newx$xx,pred[,2],col=i,lty=2)
      total<-data.frame(time=newx$xx, fit=pred[,1], Lower=pred[,2])
      show(total) 
         }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #add criteria limit
     abline(h=Lper, col = "gray60")
     abline(v=PPY, col = "gray60")
     cat(" -----------------------------------------------------------------\n\n")
     cat(" Drug product with lower acceptance limit of ",Lper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                          \n\n")
     cat("******************************************************************\n")
     cat("\n")
  }
}
##### end of one-sided low
##### if one-sided upper, start from here
if (onesidedup ){
cat("\n")
cat("                     One-sided upper LC analysis                        \n\n")
if (noPX){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
    noSolution = TRUE
}
else { 
     allSF<-data.frame(ba,PX)
     colnames(allSF)<-c(" batch#", "  shelf life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf life\n\n")
     cat("\n\n")
     windows(record = TRUE )  ### now start preparing to plot...
     #i<-formatC(PPX,format="f",digits=2) 
     #i<-round(PX,3) same output as the above... 
     shelflife<-as.integer(PPX)
     main<-paste(c("Shelf life =",shelflife, "months/weeks (PgDn to switch plots)"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata,xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Uper-30),(Uper+10)), main=main,
     xlab = "Time (months/weeks)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
     axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
     axis(1,at=0:100,tcl=-.2, labels=FALSE)   
      
     LLegend<-paste("batch#",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend,
               text.width = strwidth("10000000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)
#plot CI
      # newx<-data.frame(xx=seq(0,(max(ANCOVAdata$time)+10)))
      newx<-data.frame(xx=seq(0,(2.0*max(ANCOVAdata$time))))
      for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)
      pred<-predict(mod,newdata=newx,interval = c("confidence"),level = 0.90,type="response")
      lines(newx$xx,pred[,3],col=i,lty=2) 
      total<-data.frame(time=newx$xx, fit=pred[,1], Upper=pred[,3])
      show(total) 
      }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(v=PPX, col = "gray60")
     
     cat(" -----------------------------------------------------------------\n\n")
     cat(" Drug product with upper acceptance limit of ",Uper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
   }
}
##### end of one-sided upper
##### if two-sided, start from here
if (twosided){
cat("                        Two-sided LC analysis                           \n\n")
if (noPX && noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
   noSolution = TRUE
}
else {
if  (PPX>=PPY)  {
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf life\n\n")
     cat("\n\n")
     windows(record = TRUE )  ### now start preparing to plot...
     #go to plot of single batch
     #i<-formatC(PPY,format="f",digits=2) 
     #i<-round(PPY,3)
     #### oultine plot box 
     shelflife<-as.integer(PPY)
     main<-paste(c("Shelf life =",shelflife, "months/weeks (PgDn to switch plots)"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata, xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months/weeks)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
       
   
     LLegend<-paste("batch#",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend,
               text.width = strwidth("10000000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)

#plot CI
      ## newx<-data.frame(xx=seq(0,(max(ANCOVAdata$time)+10))) 
      newx<-data.frame(xx=seq(0,(2.0*max(ANCOVAdata$time))))
      for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)
      pred<-predict(mod, newdata=newx,interval = c("confidence"),level = 0.95,type="response")
      lines(newx$xx,pred[,2],col=i,lty=2)
      lines(newx$xx,pred[,3],col=i,lty=2)
      total<-data.frame(time=newx$xx, fit=pred[,1], Lower=pred[,2], Upper=pred[,3])
      show(total) 
         }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PPY, col = "gray60")
     cat(" -----------------------------------------------------------------\n\n")
     cat(" Drug product with lower acceptance limit of ",Lper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
     }
else {
    if(PPY>PPX){
      allSF<-data.frame(ba,PX)
      colnames(allSF)<-c(" batch#", "  shelf life*")
      show(allSF)
      cat("-------------------------\n")
      cat("*: estimated shelf life\n\n")
      cat("\n\n")
      windows(record = TRUE )  ### now start preparing to plot...
     #i<-formatC(PPX,format="f",digits=2) 
     #i<-round(PX,3) same output as the above... 
     shelflife<-as.integer(PPX)
     main<-paste(c("Shelf life =",shelflife, "months/weeks (PgDn to switch plots)"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata,xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months/weeks)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)   
      
     LLegend<-paste("batch#",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend,
               text.width = strwidth("10000000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)
#plot CI
      ## newx<-data.frame(xx=seq(0,(max(ANCOVAdata$time)+10)))
      newx<-data.frame(xx=seq(0,(2.0*max(ANCOVAdata$time))))
      for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)
      pred<-predict(mod,newdata=newx,interval = c("confidence"),level = 0.95,type="response")
      lines(newx$xx,pred[,2],col=i,lty=2)
      lines(newx$xx,pred[,3],col=i,lty=2) 
         total<-data.frame(time=newx$xx, fit=pred[,1], Lower=pred[,2], Upper=pred[,3])
         show(total) 
         }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PPX, col = "gray60")
     
     cat(" -----------------------------------------------------------------\n\n")
     cat(" Drug product with upper acceptance limit of ",Uper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
      } 
    }
  }
 }
### end of two-sided.
### do Q-Q plot only if there is at leat one solution (shelf life)
if (!noSolution) {
    qqnorm(QQ$value, las=1, main = "Normal Q-Q Plot of Residuals (PgUp to switch plots)", col=c(QQ$L1))  
      
    LLegend<-paste("batch#",c(Intable$batch)) 
    temp <- legend("topleft", legend = LLegend,
            text.width = strwidth("10000000"),
            lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)
     }
   sink()
       cat("*****************************************************************************\n\n")
       cat("## Please note: The output file (",outputfile,") has been created \n")
       cat("   and placed at",filepath,"\n\n")
       cat("*****************************************************************************\n\n") 
   bye()
}
########################################################################################
else {
  if (((Pslope <0.25) && (Pintercept >= 0.25))||((Pslope < 0.25) && (Pintercept < 0.25))) {
  # get different slopes and intercepts   
cat("--------------------------------------------------------------------------\n")
cat("          << ANCOVA Output: Testing for poolability of batches >>         \n")
cat("--------------------------------------------------------------------------\n")
cat("                                                                          \n")
cat(" The test rejects the hypothesis of equality of slopes (there is a        \n")
cat(" significant difference in slopes and intercepts among batches).          \n")
cat("                                                                          \n")
if(onesidedlo){
cat("               <<Model #3: one-sided lower LC analysis>>                  \n")
              }
else {
   if (onesidedup){
cat("               <<Model #3: one-sided upper LC analysis>>                  \n")
                  }
   else {
cat("                  <<Model #3: two-sided LC analysis>>                     \n")
        }
}
cat("        separate intercepts and separate slopes between batches.          \n")
cat("------------------------------------------------------------------------\n\n")     
cat("              <<linear regression model: Assay (%) vs. time>>             \n")
cat("\n")
show(lm(assay ~ batch*time  , data=ANCOVAdata) )
show(anova(lm(assay ~ batch*time, data=ANCOVAdata)))
cat("\n")
cat("**************************************************************************\n")
cat("                               << Output >>                               \n")
cat("--------------------------------------------------------------------------\n")
cat("                    <<Summary: linear regression model>>                \n\n")

W.split<-split(ANCOVAdata, list(ANCOVAdata$batch))
K.split<-split(Multipledata, list(Multipledata$batch)) 
#step1: catch different intercepts and a slope from lm 
   prepreIntercept<-0
     for (i in 1:(length(W.split))){
     prepreIntercept[i]<-coef(lm(assay ~ batch*time, data=ANCOVAdata) )[i]
     }
   preIntercept<-0
     for(i in 1:((length(W.split))-1)){
     preIntercept[i]<-prepreIntercept[1]+ prepreIntercept[i+1]
     }
 
   prepreSlope<-0
     for (i in 1:((length(W.split)+length(W.split)))){
     prepreSlope[i]<-coef(lm(assay ~ batch*time , data=ANCOVAdata) )[i]
     }
   preSlope<-0
     for (i in 1:(length(W.split))){
     preSlope[i]<-prepreSlope[i+(length(W.split))] 
     }
   Slope_1<-0
     for(i in 1:((length(W.split))-1)){
     Slope_1[i]<-preSlope[1]+ preSlope[i+1]
     } 
   ba<-0 
     for (i in 1:(length(K.split))){
     ba[i]<-K.split[[i]][["batch"]][1]
     }
     
  
cat("\n")     
   #collect all intercepts in a dataframe
   Intable<-data.frame(batch=ba, Intercept=c(prepreIntercept[1],preIntercept), Slope=c(preSlope[1],Slope_1))  
    
cat("\n")
#step2: calculate description statistics
     #divide data in to different group based on batches
     CAL<-NULL 
     TIME<-NULL
     RRES<-NULL 
     OBS<-NULL
     cal<-0
     Res<-0 
     L<-0
     Sxx<-0
     Syy<-0
     Sxy<-0 
     KKK<-0
     SK<-0
     Xi<-0
     XYi<-0
     T<-0 
     delta1<-0
     delta2<-0
     a1<-0
     b1<-0
     c1<-0
     X1<-0
     X2<-0
     a2<-0
     b2<-0
     c2<-0
     Y1<-0
     Y2<-0
     PX<-0
     PY<-0
     for (j in 1:length(W.split)){
          Intercept<-0
          Slope<-0
          for(x in 1: length(unique(Intable$batch))){
                if (W.split[[j]][["batch"]][1]==Intable$batch[[x]]){
                  Intercept<- Intable$Intercept[[x]]
                 }
                if (W.split[[j]][["batch"]][1]==Intable$batch[[x]]){
                  Slope<- Intable$Slope[[x]]
                } 
          } 
          cal<-0
          Res<-0
          Sxxyy<-0
          KK<-0 
          Xii<-0
          XYii<-0
          for (a in 1:length(W.split[[j]][["time"]])){    
             #Calculated assay
               cal[a]<- (((W.split[[j]][["time"]][a])*Slope)+ Intercept) 
             #extract model residuals
               Res[a]<- cal[a]-(W.split[[j]][["assay"]][a])
             #sum of (Xi-Xmean)* (Yi-Ymean)
               Sxxyy[a]<-((W.split[[j]][["assay"]][a]-mean(W.split[[j]][["assay"]]))*
              (W.split[[j]][["time"]][a]-mean(W.split[[j]][["time"]])))
             #KK:(yi-(Intercept+Slope*Xi))^2 , KKK: sum 0f KK, SK:sqrt(KKK/L-2) 
               KK[a]<-(W.split[[j]][["assay"]][a]-(Intercept+Slope*W.split[[j]][["time"]][a]))^2 
             #Xi: sum of (Xi)^2  
               Xii[a]<-(W.split[[j]][["time"]][a])^2
             #XYi: sum of (Xi*Yi)
               XYii[i]<-(W.split[[j]][["time"]][a])*(W.split[[j]][["assay"]][a])
               }

       #number of data points
        L[j]<-length(W.split[[j]][["time"]])  
       #Residual SE (time, assay, time*assay)
        #sum of (Xi-Xmean)^2
        Sxx[j]<-var(W.split[[j]][["time"]])*(L[j]-1)
       #sum of (Yi-Ymean)^2
        Syy[j]<-var(W.split[[j]][["assay"]])*(L[j]-1) 
       #sum of (Xi-Xmean)* (Yi-Ymean)
        Sxy[j]<-sum(Sxxyy)
       #KK:(yi-(Intercept+Slope*Xi))^2 , KKK: sum 0f KK, SK:sqrt(KKK/L-2) 
        KKK[j]<-sum(KK)
        SK[j]<-sqrt(KKK[j]/(L[j]-2))
       #Xi: sum of (Xi)^2
        Xi[j]<-sum(Xii)
       #XYi: sum of (Xi*Yi)
        XYi[j]<-sum(XYii)
       #95%CI,n-2 T value
        if(onesidedlo || onesidedup){
          T[j]<-qt(0.95,L[j]-2)}
        else {
          T[j]<-qt(0.975,L[j]-2)} 
#step3: calculate possible X1,X2,Y1,Y2   
      #Slope: (Xi/(L*Sxx))*Yi+ (-mean(Singledata$time)/Sxx)*XYi
      #Intercept: (-mean(Singledata$time)/Sxx)*Yi+ (1/Sxx)*XYi
      #calculate shelf life
        if (onesidedlo) {
        #intersect 95% CI with Lower criteria
        delta2[j]<-Intercept-Lper
        a2[j]<-(1/Sxx[j])-(Slope/(T[j]*SK[j]))^2
        b2[j]<-2*(-mean(W.split[[j]][["time"]])/Sxx[j])-((2*Slope*delta2[j])/(T[j]*SK[j])^2)
        c2[j]<-(Xi[j]/(L[j]*Sxx[j]))-(delta2[j]/(T[j]*SK[j]))^2
        Y1[j]<-((-b2[j])+(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
        Y2[j]<-((-b2[j])-(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
        }
        else {
          if (onesidedup) {
          #intersect 95% CI with Upper criteria
           delta1[j]<-Intercept-Uper
           a1[j]<-(1/Sxx[j])-(Slope/(T[j]*SK[j]))^2
           b1[j]<-2*(-mean(W.split[[j]][["time"]])/Sxx[j])-((2*Slope*delta1[j])/(T[j]*SK[j])^2)
           c1[j]<-(Xi[j]/(L[j]*Sxx[j]))-(delta1[j]/(T[j]*SK[j]))^2
           X1[j]<-((-b1[j])+(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j])
           X2[j]<-((-b1[j])-(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j])
          }
          else {
           #intersect 95% CI with Upper criteria
           delta1[j]<-Intercept-Uper
           a1[j]<-(1/Sxx[j])-(Slope/(T[j]*SK[j]))^2
           b1[j]<-2*(-mean(W.split[[j]][["time"]])/Sxx[j])-((2*Slope*delta1[j])/(T[j]*SK[j])^2)
           c1[j]<-(Xi[j]/(L[j]*Sxx[j]))-(delta1[j]/(T[j]*SK[j]))^2
           X1[j]<-((-b1[j])+(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j])
           X2[j]<-((-b1[j])-(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j])
           
           #intersect 95% CI with Lower criteria
           delta2[j]<-Intercept-Lper
           a2[j]<-(1/Sxx[j])-(Slope/(T[j]*SK[j]))^2
           b2[j]<-2*(-mean(W.split[[j]][["time"]])/Sxx[j])-((2*Slope*delta2[j])/(T[j]*SK[j])^2)
           c2[j]<-(Xi[j]/(L[j]*Sxx[j]))-(delta2[j]/(T[j]*SK[j]))^2
           Y1[j]<-((-b2[j])+(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
           Y2[j]<-((-b2[j])-(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
               }
         }
    
#step4: make decision
######
if(onesidedlo){
if ((b2[j])^2-(4*a2[j]*c2[j])>=0){
  if ((a2[j]>0 && b2[j]>0 && c2[j]>0) || (a2[j]<0 && b2[j]<0 && c2[j]<0)){
       PY[j]<-c(1000000000000)
        }
  else {
   if((a2[j]>0 && b2[j]>0 && c2[j]<0) || (a2[j]<0 && b2[j]>0 && c2[j]>0) || 
      (a2[j]>0 && b2[j]<0 && c2[j]<0)|| (a2[j]<0 && b2[j]<0 && c2[j]>0)){
       if ((Y1[j]<= 0)) Y1[j]<-c(1000000000000)
       if ((Y2[j]<= 0)) Y2[j]<-c(1000000000000)
       DF<-data.frame(Y=c(Y1[j],Y2[j]))
       PY[j]<-min(DF)

        }
   else {
      DF<-data.frame(Y=c(Y1[j],Y2[j]))
      PY[j]<-min(DF)
        }
     }
   }
else {
      PY[j]<-c(1000000000000)
     }
}
#######
if(onesidedup){
if ((b1[j])^2-(4*a1[j]*c1[j])>=0){
  if ((a1[j]>0 && b1[j]>0 && c1[j]>0) || (a1[j]<0 && b1[j]<0 && c1[j]<0)){
       PX<-c(1000000000000)
        }
  else {
   if((a1[j]>0 && b1[j]>0 && c1[j]<0) || (a1[j]<0 && b1[j]>0 && c1[j]>0) || 
      (a1[j]>0 && b1[j]<0 && c1[j]<0)|| (a1[j]<0 && b1[j]<0 && c1[j]>0)){
       if ((X1[j]<= 0)) X1[j]<-c(1000000000000)
       if ((X2[j]<= 0)) X2[j]<-c(1000000000000)
       DF<-data.frame(X=c(X1[j],X2[j]))
       PX[j]<-min(DF)
        }
   else {
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-min(DF)
        }
     }
   }
else {
      PX<-c(1000000000000)
     }
}
#######
if(twosided){
if ((((b1[j])^2-(4*a1[j]*c1[j]))>=0) && (((b2[j])^2-(4*a2[j]*c2[j]))>=0)){
  if ((a1[j]>0 && b1[j]>0 && c1[j]>0) || (a1[j]<0 && b1[j]<0 && c1[j]<0)){
      PX[j]<-c(1000000000000)

      }
  else{
  if ((a1[j]>0 && b1[j]>0 && c1[j]<0) || (a1[j]<0 && b1[j]>0 && c1[j]>0) || 
      (a1[j]>0 && b1[j]<0 && c1[j]<0)|| (a1[j]<0 && b1[j]<0 && c1[j]>0)){
      if ((X1[j]<= 0)) X1[j]<-c(1000000000000)
      if ((X2[j]<= 0)) X2[j]<-c(1000000000000)
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-min(DF)

      }
  else{
    if ((a1[j]>0 && b1[j]<0 && c1[j]>0) ||(a1[j]<0 && b1[j]>0 && c1[j]<0)){
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-min(DF)
      
       }
      }
    }
  if ((a2[j]>0 && b2[j]>0 && c2[j]>0) || (a2[j]<0 && b2[j]<0 && c2[j]<0)){
       PY[j]<-c(1000000000000)

        }
  else{
   if((a2[j]>0 && b2[j]>0 && c2[j]<0) || (a2[j]<0 && b2[j]>0 && c2[j]>0) || 
      (a2[j]>0 && b2[j]<0 && c2[j]<0)|| (a2[j]<0 && b2[j]<0 && c2[j]>0)){
       if ((Y1[j]<= 0)) Y1[j]<-c(1000000000000)
       if ((Y2[j]<= 0)) Y2[j]<-c(1000000000000)
       DF<-data.frame(Y=c(Y1[j],Y2[j]))
       PY[j]<-min(DF)
        
        }
   else {
      if ((a2[j]>0 && b2[j]<0 && c2[j]>0) ||(a2[j]<0 && b2[j]>0 && c2[j]<0)){
      DF<-data.frame(Y=c(Y1[j],Y2[j]))
      PY[j]<-min(DF)
       
       }
      }
    }
   }

else {
if ((((b1[j])^2-(4*a1[j]*c1[j]))>=0) && (((b2[j])^2-(4*a2[j]*c2[j]))<0)){
  if ((a1[j]>0 && b1[j]>0 && c1[j]>0) || (a1[j]<0 && b1[j]<0 && c1[j]<0)){
      PY[j]<-c(1000000000000)
      PX[j]<-c(1000000000000)
      
      }
  else{
  if ((a1[j]>0 && b1[j]>0 && c1[j]<0) || (a1[j]<0 && b1[j]>0 && c1[j]>0) || 
      (a1[j]>0 && b1[j]<0 && c1[j]<0)|| (a1[j]<0 && b1[j]<0 && c1[j]>0)){
      if ((X1[j]<= 0)) X1[j]<-c(1000000000000)
      if ((X2[j]<= 0)) X2[j]<-c(1000000000000)
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-min(DF)
      PY[j]<-c(1000000000000)
      
      }
  else{
    if ((a1[j]>0 && b1[j]<0 && c1[j]>0) ||(a1[j]<0 && b1[j]>0 && c1[j]<0)){
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-min(DF)
      PY[j]<-c(1000000000000)
      
       }
      }
    }
   }

else {
if ((((b1[j])^2-(4*a1[j]*c1[j]))< 0) && (((b2[j])^2-(4*a2[j]*c2[j]))>=0)){
  if ((a2[j]>0 && b2[j]>0 && c2[j]>0) || (a2[j]<0 && b2[j]<0 && c2[j]<0)){
      PY[j]<-c(1000000000000)
      PX[j]<-c(1000000000000)

        }
   else{
   if((a2[j]>0 && b2[j]>0 && c2[j]<0) || (a2[j]<0 && b2[j]>0 && c2[j]>0) ||
      (a2[j]>0 && b2[j]<0 && c2[j]<0)|| (a2[j]<0 && b2[j]<0 && c2[j]>0)){
       if ((Y1[j]<= 0)) Y1[j]<-c(1000000000000)
       if ((Y2[j]<= 0)) Y2[j]<-c(1000000000000)
       DF<-data.frame(Y=c(Y1[j],Y2[j]))
       PY[j]<-min(DF)
       PX[j]<-c(1000000000000)

        }
   else {
      if ((a2[j]>0 && b2[j]<0 && c2[j]>0) ||(a2[j]<0 && b2[j]>0 && c2[j]<0)){
      DF<-data.frame(Y=c(Y1[j],Y2[j]))
      PY[j]<-min(DF)
      PX<-c(1000000000000)
       
       }
      }
     }
    }
else {
if ((((b1[j])^2-(4*a1[j]*c1[j]))< 0) && (((b2[j])^2-(4*a2[j]*c2[j]))<0)){
     PY<-c(1000000000000)
     PX<-c(1000000000000)

    }
   }
  }
 }
}
#########
#step5: Output
cat(" --- Batch#:", ba[j],"---\n")
cat("\nY =",Intercept,"+(",Slope,") X\n\n")
cat("\n")
output<-data.frame(W.split[[j]][["time"]],W.split[[j]][["assay"]],cal,Res)
colnames(output)<-list("Time","Observed assay(%)","Calculated assay(%)","Residuals")  
show(output)
TIME[[j]]<-c(W.split[[j]][["time"]])
CAL[[j]]<-c(cal)
RRES[[j]]<-c(Res)
OBS[[j]]<-c(W.split[[j]][["assay"]])
cat("\n\n")  
}

AA<-melt(CAL)
ZZ<-melt(TIME)
QQ<-melt(RRES)
OO<-melt(OBS)

#choose min PX or PY
if(onesidedlo){
DM<- data.frame(batch=ba, Lower=PY)
DM<- na.omit(DM)
PPY<-min(DM$Lower)
if ((PPY==1000000000000)) noPY = TRUE
}
else {
 if(onesidedup){
   DM<- data.frame(batch=ba, Upper=PX)
   DM<- na.omit(DM)
   PPX<-min(DM$Upper)}
   if ((PPX==1000000000000)) noPX = TRUE
 else {
   DM<- data.frame(batch=ba, Upper=PX, Lower=PY)
   DM<- na.omit(DM)
   PPX<-min(DM$Upper)
   PPY<-min(DM$Lower)
   if ((PPY==1000000000000)) noPY = TRUE
   if ((PPX==1000000000000)) noPX = TRUE
 }
}

### windows(record = TRUE ) # here not to plot yet!
#step6: summary and plot
cat("**************************************************************************\n")
cat("                       << Summary and plots >>                            \n")
cat("--------------------------------------------------------------------------\n")
cat("\n\n")
##### if one-sided low, start from here
if (onesidedlo) {
cat("                     One-sided lower LC analysis                        \n\n")
if (noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
   noSolution = TRUE
}
else {
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf life\n\n")
     cat("\n\n")
     windows(record = TRUE )  ### now start preparing to plot...
          #go to plot of single batch
     #i<-formatC(PPY,format="f",digits=2) 
     #i<-round(PPY,3)
     #### oultine plot box 
     shelflife<-as.integer(PPY)
     main<-paste(c("Shelf life =",shelflife,"months/weeks (PgDn to switch plots)"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata, xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Lper-10),(Lper+30)), main=main,
     xlab = "Time (months)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE) 
    
     LLegend<-paste("batch#",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend,
               text.width = strwidth("10000000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1) 
     
#plot CI
    newx<-data.frame(xx=seq(0,2.0*max(ANCOVAdata$time))) 
    for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)
      pred<-predict(mod, newdata=newx,interval = c("confidence"),level = 0.90,type="response")
      lines(newx$xx,pred[,2],col=i,lty=2)
      total<-data.frame(time=newx$xx, fit=pred[,1], Lower=pred[,2])
      show(total) 
         }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #add criteria limit
     abline(h=Lper, col = "gray60")
     abline(v=PPY, col = "gray60")
     cat("------------------------------------------------------------------\n")
     cat("                                                                  \n")
     cat(" Drug product with lower acceptance limit of",Lper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
   }
}
##### end of one-sided low
##### if one-sided upper, start from here
if (onesidedup ){
cat("\n")
cat("                     One-sided upper LC analysis                        \n\n")
if (noPX){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
    noSolution = TRUE
}
else { 
     allSF<-data.frame(ba,PX)
     colnames(allSF)<-c(" batch#", "  shelf life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf life\n\n")
     cat("\n\n")
     windows(record = TRUE )  ### now start preparing to plot...
     #i<-formatC(PPX,format="f",digits=2) 
     #i<-round(PX,3) ### same output as the above... 
     shelflife<-as.integer(PPX)
     main<-paste(c("Shelf life =",shelflife, "months/weeks (PgDn to switch plots)"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata,xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Uper-30),(Uper+10)), main=main,
     xlab = "Time (months/weeks)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
     axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
     axis(1,at=0:100,tcl=-.2, labels=FALSE)   
      
     LLegend<-paste("batch#",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend,
               text.width = strwidth("10000000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)
               
#plot CI
      newx<-data.frame(xx=seq(0,2.0*max(ANCOVAdata$time)))  
      for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)
      pred<-predict(mod,newdata=newx,interval = c("confidence"),level = 0.90,type="response")
      lines(newx$xx,pred[,3],col=i,lty=2) 
      total<-data.frame(time=newx$xx, fit=pred[,1], Upper=pred[,3])
      show(total) 
         }
      axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
      #add criteria limit
      abline(h=Uper, col = "gray60")
      abline(v=PPX, col = "gray60")
     
     cat("------------------------------------------------------------------\n\n")
     cat(" Drug product with upper acceptance limit of",Uper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
    } 
}
##### end of one-sided upper
##### if two-sided, start from here
if (twosided){
cat("                        Two-sided LC analysis                           \n\n")
if (noPX && noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
   noSolution = TRUE
}
else {
if  (PPX>=PPY)  {
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf life\n\n")
     cat("\n\n")
     windows(record = TRUE )  ### now start preparing to plot...
     #go to plot of single batch
     #i<-formatC(PPY,format="f",digits=2) 
     #i<-round(PPY,3)
     #### oultine plot box 
     shelflife<-as.integer(PPY)
     main<-paste(c("Shelf life =",shelflife,"months/weeks (PgDn to switch plots)"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata, xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE) 
    
     LLegend<-paste("batch#",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend,
               text.width = strwidth("10000000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1) 
     
#plot CI
     newx<-data.frame(xx=seq(0,2.0*max(ANCOVAdata$time))) 
     for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)
      pred<-predict(mod, newdata=newx,interval = c("confidence"),level = 0.95,type="response")
      lines(newx$xx,pred[,2],col=i,lty=2)
      lines(newx$xx,pred[,3],col=i,lty=2)
      total<-data.frame(time=newx$xx, fit=pred[,1], Lower=pred[,2], Upper=pred[,3])
      show(total) 
      }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PPY, col = "gray60")
     cat("------------------------------------------------------------------\n")
     cat("                                                                  \n")
     cat(" Drug product with lower acceptance limit of",Lper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
       }
else {
    if(PPY>PPX){
      allSF<-data.frame(ba,PX)
      colnames(allSF)<-c("  batch", " shelf life*")
      show(allSF)
      cat("-------------------------\n")
      cat("*: estimated shelf life\n\n")
      cat("\n\n")
      windows(record = TRUE )  ### now start preparing to plot...
     #i<-formatC(PPX,format="f",digits=2) 
     #i<-round(PX,3) ### same as the above...
     shelflife<-as.integer(PPX)
     main<-paste(c("Shelf life =",shelflife, "months/weeks (PgDn to switch plots)"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata,xlim=c(0,2.0*max(ANCOVAdata$time)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months/weeks)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)  
        
     LLegend<-paste("batch#",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend,
               text.width = strwidth("10000000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)
#plot CI
      newx<-data.frame(xx=seq(0,2.0*max(ANCOVAdata$time)))  
      for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)
      pred<-predict(mod,newdata=newx,interval = c("confidence"),level = 0.95,type="response")
      lines(newx$xx,pred[,2],col=i,lty=2)
      lines(newx$xx,pred[,3],col=i,lty=2) 
      total<-data.frame(time=newx$xx, fit=pred[,1], Lower=pred[,2], Upper=pred[,3])
      show(total) 
      }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PPX, col = "gray60")
     
     cat("------------------------------------------------------------------\n")
     cat("                                                                  \n")
     cat(" Drug product with upper acceptance limit of",Uper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
       } 
     }
   }
 }
### end of two-sided.
### do Q-Q plot only if there is at leat one solution (shelf life)
if (!noSolution) {
    qqnorm(QQ$value, las=1, main = "Normal Q-Q Plot of Residuals (PgUp to switch plots)", col=c(QQ$L1))  
      
    LLegend<-paste("batch#",c(Intable$batch)) 
    temp <- legend("topleft", legend = LLegend,
            text.width = strwidth("10000000"),
            lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)
     }
   sink()
       cat("*****************************************************************************\n\n")
       cat("## Please note: The output file (",outputfile,") has been created \n")
       cat("   and placed at",filepath,"\n\n")
       cat("*****************************************************************************\n\n") 
   bye()
     }
   }
 }
}

### end of MutilpleAnalyze