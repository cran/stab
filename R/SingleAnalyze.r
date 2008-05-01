#Input assay data Menu for Data Analysis for a Single Batch
library(stats4)
library(MASS)
#predict 需要stats4 package
#confidence interval 需要 MASS package
SingleAnalyze<-function(Singledata, Uper, Lper, separateWindows=TRUE)
{
cat("\n")
cat("Drug product with upper acceptance criteria of ____ % of label claim\n")
Uper <- scan(nlines=1,quiet=TRUE)
cat("\n")
cat("Drug product with lower acceptance criteria of ____ % of label claim\n")
Lper <- scan(nlines=1,quiet=TRUE)
cat("\n")

#fitting data with linear regression model
cat("<<Output: linear regression model: time vs. assay (%)>>\n")
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
 #與上面效果一樣
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
T<-qt(0.975,L-2)
#Slope: (Xi/(L*Sxx))*Yi+ (-mean(Singledata$time)/Sxx)*XYi
#Intercept: (-mean(Singledata$time)/Sxx)*Yi+ (1/Sxx)*XYi

#calculate shelf life
  #intersect 95% CI with Upper criteria
delta<-Intercept-Uper
a1<-(1/Sxx)-(Slope/(T*SK))^2
b1<-2*(-mean(Singledata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
c1<-(Xi/(L*Sxx))-(delta/(T*SK))^2
    X1<-((-b1)+(sqrt((b1)^2-(4*a1*c1))))/(2*a1)
    X2<-((-b1)-(sqrt((b1)^2-(4*a1*c1))))/(2*a1)


#intersect 5% CI with Lower criteria
delta<-Intercept-Lper
a2<-(1/Sxx)-(Slope/(T*SK))^2
b2<-2*(-mean(Singledata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
c2<-(Xi/(L*Sxx))-(delta/(T*SK))^2
   Y1<-((-b2)+(sqrt((b2)^2-(4*a2*c2))))/(2*a2)
   Y2<-((-b2)-(sqrt((b2)^2-(4*a2*c2))))/(2*a2)

#make decision
if ((((b1)^2-(4*a1*c1))>=0) && (((b2)^2-(4*a2*c2))>=0)){
  if ((a1>0 && b1>0 && c1>0) || (a1<0 && b1<0 && c1<0)){
      PX<-c(1000000000000) 
      }
  else{
  if ((a1>0 && b1>0 && c1<0) || (a1<0 && b1>0 && c1>0) || (a1>0 && b1<0 && c1<0)|| (a1<0 && b1<0 && c1>0)){
      DF<-data.frame(X=c(X1,X2))
      PX<-max(DF)
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
   if((a2>0 && b2>0 && c2<0) || (a2<0 && b2>0 && c2>0) || (a2>0 && b2<0 && c2<0)|| (a2<0 && b2<0 && c2>0)){
       DF<-data.frame(Y=c(Y1,Y2))
       PY<-max(DF)
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
      PX<-c(1000000000000)
      PY<-c(1000000000000)
      } 
  else{
  if ((a1>0 && b1>0 && c1<0) || (a1<0 && b1>0 && c1>0) || (a1>0 && b1<0 && c1<0)|| (a1<0 && b1<0 && c1>0)){
      DF<-data.frame(X=c(X1,X2))
      PX<-max(DF)
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
      PX<-c(1000000000000)
      PY<-c(1000000000000)
        } 
   else{
   if((a2>0 && b2>0 && c2<0) || (a2<0 && b2>0 && c2>0) || (a2>0 && b2<0 && c2<0)|| (a2<0 && b2<0 && c2>0)){
       DF<-data.frame(Y=c(Y1,Y2))
       PY<-max(DF)
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
     PX<-c(1000000000000)
     PY<-c(1000000000000)
    }
   }
  }
 }   

if (separateWindows) {
       get(getOption("device"))()
          }

#Output
cat("**************************************************************************\n")
cat("                               << Output >>                               \n")
cat("--------------------------------------------------------------------------\n")
cat("                    <<Summary: linear regression model>>                  \n")
cat("\nY=", coef(lm(Singledata$assay~Singledata$time))[1],"+(",coef(lm(Singledata$assay~Singledata$time))[2],") X\n\n")
cat("\n")
output<-data.frame(Singledata$time,Singledata$assay,cal,Res)
colnames(output)<-list("time","Observed assay(%)","Calculated assay(%)","Residuals")
show(output)
cat("\n")

if  (PX>=PY)  {
     #go to plot of single batch
     i<-formatC(PY,format="f",digits=2) 
     #i<-round(PY,3)
     main<-paste(c("Shelf Life=",i, "months"),collapse=" ")
     x<-Singledata$time
     y<-Singledata$assay
     plot(x,y,xlim=c(0,(PY+10)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black")
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,(PY+10))
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),
     level = 0.95,type="response")
     lines(newx,prd[,2],col="red",lty=2)
     lines(newx,prd[,3],col="red",lty=2)
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PY, col = "gray60")
     cat("--------------------------------------------------------------------------\n")
     cat("                                                                          \n")
     cat(" Drug product with lower acceptance criteria of ",Lper,"% of label claim  \n")
     cat(" Shelf life =",PY,"months                                                 \n\n")
     cat("**************************************************************************\n")
     cat("\n")
     bye()
       }
else {
    if(PY>PX){
     i<-formatC(PX,format="f",digits=2) 
      #i<-round(PX,3) 與上述效果一樣... 
     main<-paste(c("Shelf Life=",i, "months"),collapse=" ")
     x<-Singledata$time
     y<-Singledata$assay
     plot(x,y,xlim=c(0,(PX+10)),ylim=c((Lper-10),(Uper+10)),main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black")
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,(PX+10))
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),
     level = 0.95,type="response")
     lines(newx,prd[,2],col="red",lty=2)
     lines(newx,prd[,3],col="red",lty=2)
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PX, col = "gray60")
     cat("--------------------------------------------------------------------------\n")
     cat("                                                                          \n")
     cat(" Drug product with Upper acceptance criteria of ",Uper,"% of label claim  \n")
     cat(" Shelf life =",PX,"months                                                 \n\n")
     cat("**************************************************************************\n")
     cat("\n")
     bye()
        } 
else{
  if ((PX=1000000000000) && (PY=1000000000000)){
   cat("--------------------------------------------------------------------------\n")
   cat("                    no solution                                           \n")
   cat("**************************************************************************\n")
   cat("\n")
     bye()
      }     
     }
    } 
}  