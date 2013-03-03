#Input assay data Menu for Data Analysis for a Single Batch
library(stats4)
library(MASS)
#predict req. stats4 package
#confidence interval req. MASS package
SingleAnalyze<-function(Singledata, Uper, Lper, separateWindows = TRUE)
{
onesidedlo = FALSE
onesidedup = FALSE
twosided   = FALSE
noPX = FALSE
noPY = FALSE
noSolution = FALSE

filepath<-getwd()
cat("\n")
outputfile <- readline("Enter the output file name with no extension: ")
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
cat("------------------ stab for R v0.1.4 ------------------\n")
cat("\n developed by Hsin-ya Lee and Yung-jin Lee, 2007-2011.\n")
cat("\n generated on",date(),"\n")
cat("\n\n")

cat(" Setting of this analysis:\n")
cat(" -----------------------------------------------------------\n")
if (onesidedlo){
cat(" Single batch with the lower acceptance limit of",Lper,"%.\n\n")
}
else {
  if (onesidedup) {
     cat(" Single batch with the upper acceptance limit of",Uper,"%.\n\n")
     }
  else {
     cat(" Single batch with the lower acceptance limit of",Lper,"%.\n")
     cat(" & the upper acceptance limit of",Uper,"%.\n\n")
    }
}
cat("\n\n")
cat("<< --- List of input data --- >>\n\n")
show(Singledata)
cat("\n\n")

#fitting data with linear regression model
cat("<<Output: linear regression model: assay (%) vs. time>>\n\n")
cat("                Single batch analysis                  \n\n")
show(lm(Singledata$assay~Singledata$time))
show(anova(salm<-lm( Singledata$assay~Singledata$time)))
print(summary(salm<-lm(Singledata$assay~Singledata$time)))
summary(salm<-lm(Singledata$assay~Singledata$time))
Intercept<-coef(lm(Singledata$assay~Singledata$time))[1]
Slope<-coef(lm(Singledata$assay~Singledata$time))[2]
cat("\n\n")
#Calculated assay
cal<-predict(lm(Singledata$assay~Singledata$time))

#extract model residuals
Res<-residuals(lm(Singledata$assay~Singledata$time))
 ## same as above
 #Passay<-0
 #for(i in 1:length(Singledata$time)){
 # Passay[i]<-(Singledata$assay[i]-cal)
 # }

#number of data points
L<-length(Singledata$assay)
#Residual SE (time, assay, time*assay)
 #sum of (Xi-Xmean)^2
Sxx<-var(Singledata$time)*(L-1)
 #sum of (Yi-Ymean)^2
Syy<-var(Singledata$assay)*(L-1)
  #sum of (Xi-Xmean)* (Yi-Ymean)
Sxxyy<-0
for(i in 1:length(Singledata$time)){
 Sxxyy[i]<-((Singledata$assay[i]-mean(Singledata$assay))*(Singledata$time[i]-mean(Singledata$time)))
 }
Sxy<-sum(Sxxyy)
#KK:(yi-(Intercept+Slope*Xi))^2 , KKK: sum 0f KK, SK:sqrt(KKK/L-2)
KK<-0
for(i in 1:length(Singledata$time)){
 KK[i]<-(Singledata$assay[i]-(Intercept+Slope*Singledata$time[i]))^2
 }
KKK<-sum(KK)
SK<-sqrt(KKK/(L-2))
#Xi: sum of (Xi)^2
Xii<-0
for(i in 1:length(Singledata$time)){
 Xii[i]<-(Singledata$time[i])^2
 }
Xi<-sum(Xii)

#XYi: sum of (Xi*Yi)
XYii<-0
for(i in 1:length(Singledata$time)){
 XYii[i]<-(Singledata$time[i])*(Singledata$assay[i])
 }
XYi<-sum(XYii)
#95%CI,n-2 T value
 if(onesidedlo || onesidedup){
    T<-qt(0.95,L-2)}
 else {
    T<-qt(0.975,L-2)
 }

#Slope: (Xi/(L*Sxx))*Yi+ (-mean(Singledata$time)/Sxx)*XYi
#Intercept: (-mean(Singledata$time)/Sxx)*Yi+ (1/Sxx)*XYi
if(onesidedlo){
cat("                  <<one-sided lower LC analysis>>             \n")
              }
else {
   if (onesidedup){
cat("                  <<one-sided upper LC analysis>>             \n")
                  }
   else {
cat("                     <<two-sided LC analysis>>                \n")
        }
}
cat("\n")
#calculate shelf life

if (onesidedlo) {
   #intersect 95% CI with Lower criteria
   delta<-Intercept-Lper
   a2<-(1/Sxx)-(Slope/(T*SK))^2
   b2<-2*(-mean(Singledata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
   c2<-(Xi/(L*Sxx))-(delta/(T*SK))^2
   Y1<-((-b2)+(sqrt((b2)^2-(4*a2*c2))))/(2*a2)
   Y2<-((-b2)-(sqrt((b2)^2-(4*a2*c2))))/(2*a2)
}
if (onesidedup) {
   #intersect 95% CI with Upper criteria
   delta<-Intercept-Uper
   a1<-(1/Sxx)-(Slope/(T*SK))^2
   b1<-2*(-mean(Singledata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
   c1<-(Xi/(L*Sxx))-(delta/(T*SK))^2
   X1<-((-b1)+(sqrt((b1)^2-(4*a1*c1))))/(2*a1)
   X2<-((-b1)-(sqrt((b1)^2-(4*a1*c1))))/(2*a1)
}
if (twosided) {
   #intersect 95% CI with Upper criteria
   delta<-Intercept-Uper
   a1<-(1/Sxx)-(Slope/(T*SK))^2
   b1<-2*(-mean(Singledata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
   c1<-(Xi/(L*Sxx))-(delta/(T*SK))^2
   X1<-((-b1)+(sqrt((b1)^2-(4*a1*c1))))/(2*a1)
   X2<-((-b1)-(sqrt((b1)^2-(4*a1*c1))))/(2*a1)


   #intersect 95% CI with Lower criteria
   delta<-Intercept-Lper
   a2<-(1/Sxx)-(Slope/(T*SK))^2
   b2<-2*(-mean(Singledata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
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
### windows(record = TRUE)
#Output
cat("*********************************************************************\n")
cat("                    << Output - single batch >>                      \n")
cat("---------------------------------------------------------------------\n")
cat("                <<Summary: linear regression model>>                 \n")
cat("\nY =",coef(lm(Singledata$assay~Singledata$time))[1],"+(",coef(lm(Singledata$assay~Singledata$time))[2],") X\n\n")
cat("\n")
output<-data.frame(Singledata$time,Singledata$assay,cal,Res)
colnames(output)<-list("Time","Observed assay(%)","Calculated assay(%)","Residuals")
show(output)
cat("\n")
cat("\n\n")
cat("*********************************************************************\n")
cat("                      << Summary and plots >>                        \n")
cat("---------------------------------------------------------------------\n")
cat("\n\n")

##### if one-sided low, start from here
if (onesidedlo) {
cat("                     One-sided lower LC analysis                   \n\n")
if (noPY){
cat("  No solution can be found with this model. Please try others.       \n")
cat("*********************************************************************\n")
   noSolution = TRUE
}
else {
     #go to plot of single batch
     #i<-formatC(PY,format="f",digits=2) 
     #i<-round(PY,3)
     windows(record = TRUE)  # prepare to plot now...
     cat(" The estimated shelf life =",PY,"(months/weeks)\n\n")
     shelflife<-as.integer(PY)
     main<-paste(c("Shelf life=",shelflife, "months/weeks"),collapse=" ")
     x<-Singledata$time
     y<-Singledata$assay
     plot(x,y,xlim=c(0,(PY+12)),ylim=c((Lper-10),(Lper+30)), main=main,
     xlab = "Time (months/weeks)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
       
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,1.5*max(Singledata$time))
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),
     level = 0.90,type="response")
     lines(newx,prd[,2],col="red",lty=2)
     #show 95 %CI
     total<-data.frame(time=newx, fit=prd[,1], Lower=prd[,2])
       cat("\n")
       show(total)
       cat("\n")
     #add criteria limit
     abline(h=Lper, col = "gray60")
     abline(v=PY, col = "gray60")
     cat("----------------------------------------------------------------\n\n")
     cat(" Drug product with lower acceptance limit of ",Lper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
     }
}
##### end of one-sided lower
##### if one-sided uppper, start here.
if (onesidedup){
cat("                   One-sided upper LC analysis                     \n\n")
if (noPX){
cat("  No solution can be found with this model. Please try others.       \n")
cat("*********************************************************************\n")
   noSolution = TRUE
}
else {
     #go to plot of single batch
     #i<-formatC(PX,format="f",digits=2) 
     #i<-round(PX,3)
     windows(record = TRUE)  # prepare to plot now...
     cat(" The estimated shelf life =", PX,"(months/weeks)\n\n")
     shelflife<-as.integer(PX)
     main<-paste(c("Shelf life=",shelflife, "months/weeks"),collapse=" ")
     x<-Singledata$time
     y<-Singledata$assay
     plot(x,y,xlim=c(0,(PY+12)),ylim=c((Uper-30),(Uper+10)), main=main,
     xlab = "Time (months/weeks)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
       
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,1.5*max(Singledata$time))
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),
     level = 0.90,type="response")
     lines(newx,prd[,2],col="red",lty=2)
     #show 95 %CI
     total<-data.frame(time=newx, fit=prd[,1], Upper=prd[,3])
       cat("\n")
       show(total)
       cat("\n")
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(v=PX, col = "gray60")
     cat("----------------------------------------------------------------\n\n")
     cat(" Drug product with upper acceptance limit of ",Uper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
     }
}
##### end of one-sided upper
##### if two-sided, start here
if (twosided){
cat("                       Two-sided LC analysis                       \n\n")
if (noPX && noPY){
cat("  No solution can be found with this model. Please try others.       \n")
cat("*********************************************************************\n")
    noSolution = TRUE
}
else {
if  (PX>=PY)  {
     #go to plot of single batch
     #i<-formatC(PY,format="f",digits=2) 
     #i<-round(PY,3)
     windows(record = TRUE)  # prepare to plot now...
     cat(" The estimated shelf life =",PY,"(months/weeks)\n\n")
     shelflife<-as.integer(PY)
     main<-paste(c("Shelf life=",shelflife, "months/weeks"),collapse=" ")
     x<-Singledata$time
     y<-Singledata$assay
     plot(x,y,xlim=c(0,(PY+12)),ylim=c((Lper-10),(Lper+30)), main=main,
     xlab = "Time (months/weeks)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
       
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,1.5*max(Singledata$time))
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
     abline(h=Lper, col = "gray60")
     abline(h=Uper, col = "gray60")
     abline(v=PY, col = "gray60")
     cat("----------------------------------------------------------------\n\n")
     cat(" Drug product with lower acceptance limit of ",Lper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
     }
     
else {
    #go to plot of single batch
     #i<-formatC(PX,format="f",digits=2) 
     #i<-round(PX,3)
     windows(record = TRUE)  # prepare to plot now...
     cat(" The estimated shelf life =",PX,"(months/weeks)\n\n")
     shelflife<-as.integer(PX)
     main<-paste(c("Shelf life=",shelflife, "months/weeks"),collapse=" ")
     x<-Singledata$time
     y<-Singledata$assay
     plot(x,y,xlim=c(0,(PY+12)),ylim=c((Uper-30),(Uper+10)), main=main,
     xlab = "Time (months/weeks)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
       
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,1.5*max(Singledata$time))
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
     cat(" Drug product with upper acceptance limit of ",Uper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months/weeks)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
    }     
  }
}
### end of two-sided.
### do Q-Q plot only if there is at leat one solution (shelf life)
if (!noSolution) {
    qqnorm(output$Res, las=1, main = "Normal Q-Q Plot of Residuals (PgUp to switch plots)")
    }
    sink()
       cat("*****************************************************************************\n\n")
       cat("## Please note: The output file (",outputfile,") has been created \n")
       cat("   and placed at",filepath,"\n\n")
       cat("*****************************************************************************\n\n")     
    bye()
}

### end of SingleAnalyze