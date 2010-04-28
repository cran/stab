#Data Analysis for Multiple Batches
library(stats4)
library(MASS)
library(reshape) 
#predict 需要stats4 package
#confidence interval 需要 MASS package

MultipleAnalyze<-function(Multipledata, Uper, Lper, separateWindows=TRUE)
{
cat("\n")
cat("Drug product with upper acceptance criteria of ____ % of label claim\n")
Uper <- scan(nlines=1,quiet=TRUE)
cat("\n")
cat("Drug product with lower acceptance criteria of ____ % of label claim\n")
Lper <- scan(nlines=1,quiet=TRUE)
cat("\n")

#fitting data with ANCOVA model to test poolability
cat("<<Output: ANCOVA model: batch vs. time vs. assay (%)>>\n")
cat("\n")
#need to make batch into a factor
ANCOVAdata <- data.frame (batch=factor(Multipledata$batch), 
                        time=Multipledata$time, 
                        assay=Multipledata$assay)

kk<- lm(assay ~ batch*time, data=ANCOVAdata)
show(anova(kk))
Pintercept <- anova(kk)[,5][1]
Pslope <- anova(kk)[,5][3]
cat("\n")
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
cat("                            <<Suggestion>>                                \n")
cat("                The data from all batches can be combined.                \n")
cat("--------------------------------------------------------------------------\n")
cat("<<Output: linear regression model: time vs. assay (%)>>\n")
cat("\n")
show(lm(assay ~ time, data=ANCOVAdata))
show(anova(lm(assay ~ time, data=ANCOVAdata)))
W.split<-split(ANCOVAdata, list(ANCOVAdata$batch) )
ba<-0
     for (i in 1:(length(W.split))){
     ba[i]<-W.split[[i]][["batch"]][1]
     }
   
   #collect all intercepts in a dataframe
   Intable<-data.frame(batch=ba)

#step1:get a slope and a intercept
Intercept<-coef(lm(assay ~ time, data=ANCOVAdata))[1]
Slope<-coef(lm(assay ~ time, data=ANCOVAdata))[2]
cat("\n\n")
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
  #KK:(yi-(Intercept+Slope*Xi))^2 , KKK: sum 0f KK, SK:sqrt(KKK/L-2)
    KK<-0
    for(i in 1:length(ANCOVAdata$time)){
    KK[i]<-(ANCOVAdata$assay[i]-(Intercept+Slope*ANCOVAdata$time[i]))^2
     }
    KKK<-sum(KK)
    SK<-sqrt(KKK/(L-2))
  #Xi: sum of (Xi)^2
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
    T<-qt(0.975,L-2)
      #Slope: (Xi/(L*Sxx))*Yi+ (-mean(Singledata$time)/Sxx)*XYi
      #Intercept: (-mean(Singledata$time)/Sxx)*Yi+ (1/Sxx)*XYi
      #calculate shelf life
      #intersect 95% CI with Upper criteria
        delta<-Intercept-Uper
        a1<-(1/Sxx)-(Slope/(T*SK))^2
        b1<-2*(-mean(ANCOVAdata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
        c1<-(Xi/(L*Sxx))-(delta/(T*SK))^2
        X1<-((-b1)+(sqrt((b1)^2-(4*a1*c1))))/(2*a1)
        X2<-((-b1)-(sqrt((b1)^2-(4*a1*c1))))/(2*a1)
     
     #intersect 5% CI with Lower criteria
        delta<-Intercept-Lper
        a2<-(1/Sxx)-(Slope/(T*SK))^2
        b2<-2*(-mean(ANCOVAdata$time)/Sxx)-((2*Slope*delta)/(T*SK)^2)
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
windows(record = TRUE )

#Output
cat("**************************************************************************\n")
cat("                               << Output >>                               \n")
cat("--------------------------------------------------------------------------\n")
cat("                    <<Summary: linear regression model>>                  \n")
cat("\nY=", coef(lm(ANCOVAdata$assay~ANCOVAdata$time))[1],"+(",coef(lm(ANCOVAdata$assay~ANCOVAdata$time))[2],") X\n\n")
cat("\n")
output<-data.frame(ANCOVAdata$time,ANCOVAdata$assay,cal,Res)
colnames(output)<-list("time","Observed assay(%)","Calculated assay(%)","Residuals")
show(output)
cat("\n")

if  (PX>=PY)  {
     #go to plot of single batch
     i<-formatC(PY,format="f",digits=2) 
     #i<-round(PY,3)
     main<-paste(c("Shelf Life=",i, "months"),collapse=" ")
     x<-ANCOVAdata$time
     y<-ANCOVAdata$assay
     plot(x,y,xlim=c(0,(PY+10)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,(PY+10))
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
     cat("--------------------------------------------------------------------------\n")
     cat("                                                                          \n")
     cat(" Drug product with lower acceptance criteria of ",Lper,"% of label claim  \n")
     cat(" Shelf life =",PY,"months                                                 \n\n")
     cat("**************************************************************************\n")
     cat("\n")
       }
else {
    if(PY>PX){
     i<-formatC(PX,format="f",digits=2) 
      #i<-round(PX,3) 與上述效果一樣... 
     main<-paste(c("Shelf Life=",i, "months"),collapse=" ")
     x<-ANCOVAdata$time
     y<-ANCOVAdata$assay
     plot(x,y,xlim=c(0,(PX+10)),ylim=c((Lper-10),(Uper+10)),main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,(PX+10))
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
     cat("--------------------------------------------------------------------------\n")
     cat("                                                                          \n")
     cat(" Drug product with Upper acceptance criteria of ",Uper,"% of label claim  \n")
     cat(" Shelf life =",PX,"months                                                 \n\n")
     cat("**************************************************************************\n")
     cat("\n")
  
        } 
else{
  if ((PX=1000000000000) && (PY=1000000000000)){
   cat("--------------------------------------------------------------------------\n")
   cat("                    no solution at all!                                   \n")
   cat("**************************************************************************\n")
   cat("\n")
      }     
     }
   }
    qqnorm(output$Res, las=1, main = "Normal Q-Q Plot of Residuals", col=c(ANCOVAdata$batch))  
      
         temp <- legend("topleft", legend = c(Intable$batch),
               text.width = strwidth("1000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)     
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
cat("                            <<Suggestion>>                                \n")
cat(" The data can be combined for the purpose of estimating the common slope. \n")
cat("--------------------------------------------------------------------------\n")
  
cat("<<Output: linear regression model: time vs. assay (%)>>\n")
cat("\n")
show(lm(assay ~ batch+time  , data=ANCOVAdata) )
show(anova(lm(assay ~ batch+time  , data=ANCOVAdata) ))
W.split<-split(ANCOVAdata, list(ANCOVAdata$batch) )
K.split<-split(Multipledata, list(Multipledata$batch) ) 
#step1: catch different intercepts and a slope from lm 
   prepreIntercept<-0
     for (i in 1:(length(W.split))){
     prepreIntercept[i]<-coef(lm(assay ~ batch+time , data=ANCOVAdata) )[i]
     }
   preIntercept<-0
     for(i in 1:((length(W.split))-1)){
     preIntercept[i]<-prepreIntercept[1]+ prepreIntercept[i+1]
     }
   
   Slope<-0
    for (d in 1:(length(W.split))){
    Slope[d]<-coef(lm(assay ~ batch+time , data=ANCOVAdata))[d+1]
       }
     
   ba<-0
     for (i in 1:(length(K.split))){
     ba[i]<-K.split[[i]][["batch"]][1]
     }
  
   #collect all intercepts in a dataframe
   ba1<- na.omit(ba)
   show(ba1)
   Intable<-data.frame(batch=ba, Intercept=c(prepreIntercept[1],preIntercept))  

#step2: calculate description statistics
     #divide data in to different group based on batches
     
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

    SS<-(Syy-(Slope[d]*Sxy))/(L-2)
   #95%CI,n-2 T value
    T<-qt(0.975,L-2)    
      
     
     CAL<-NULL 
     TIME<-NULL
     RRES<-NULL 
     OBS<-NULL
     TT<-0   
     LL<-0
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
          for(x in 1: length(unique(Intable$batch))){
                if (W.split[[j]][["batch"]][1]==Intable$batch[[x]]){
                  Intercept<- Intable$Intercept[[x]]
                 }
                } 
                   
          cal<-0
          Res<-0
          
          for (a in 1:length(W.split[[j]][["time"]])){    
             #Calculated assay
               cal[a]<-(((W.split[[j]][["time"]][a])*Slope[d])+ Intercept) 
             #extract model residuals
               Res[a]<- cal[a]-(W.split[[j]][["assay"]][a])
               }
       
        #number of data points
        LL[j]<-length(W.split[[j]][["time"]])   
        TT[j]<-mean(W.split[[j]][["time"]]  )
#step3: calculate possible X1,X2,Y1,Y2   
 #Slope: (Xi/(L*Sxx))*Yi+ (-mean(Singledata$time)/Sxx)*XYi
      #Intercept: (-mean(Singledata$time)/Sxx)*Yi+ (1/Sxx)*XYi
      #calculate shelf life
      #intersect 95% CI with Upper criteria
        delta1[j]<-Uper-Intercept
        a1[j]<-(Slope[d]*Slope[d])-(T*T*SS)/Sxx
        b1[j]<-2*(T*T*SS*TT[j])/Sxx-(2*Slope[d]*delta1[j])
        c1[j]<-(delta1[j]*delta1[j])-(T*T*SS*TT[j]*TT[j])/Sxx-(T*T*SS/LL[j])
               
        X1[j]<-((-b1[j])+(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j])
        X2[j]<-((-b1[j])-(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j]) 
       
     #intersect 5% CI with Lower criteria
        delta2[j]<-Lper-Intercept
        a2[j]<-(Slope[d]*Slope[d])-(T*T*SS)/Sxx
        b2[j]<-2*(T*T*SS*TT[j])/Sxx-(2*Slope[d]*delta2[j])
        c2[j]<-(delta2[j]*delta2[j])-(T*T*SS*TT[j]*TT[j])/Sxx -(T*T*SS/LL[j])
              
        Y1[j]<-((-b2[j])+(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
        Y2[j]<-((-b2[j])-(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])      

   
#step4: make decision
if ((((b1[j])^2-(4*a1[j]*c1[j]))>=0) && (((b2[j])^2-(4*a2[j]*c2[j]))>=0)){
  if ((a1[j]>0 && b1[j]>0 && c1[j]>0) || (a1[j]<0 && b1[j]<0 && c1[j]<0)){
      PX[j]<-c(1000000000000)

      }
  else{
  if ((a1[j]>0 && b1[j]>0 && c1[j]<0) || (a1[j]<0 && b1[j]>0 && c1[j]>0) || (a1[j]>0 && b1[j]<0 && c1[j]<0)|| (a1[j]<0 && b1[j]<0 && c1[j]>0)){
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-max(DF)

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
   if((a2[j]>0 && b2[j]>0 && c2[j]<0) || (a2[j]<0 && b2[j]>0 && c2[j]>0) || (a2[j]>0 && b2[j]<0 && c2[j]<0)|| (a2[j]<0 && b2[j]<0 && c2[j]>0)){
       DF<-data.frame(Y=c(Y1[j],Y2[j]))
       PY[j]<-max(DF)
        
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
      PX[j]<-c(1000000000000)
      PY[j]<-c(1000000000000)
      
      }
  else{
  if ((a1[j]>0 && b1[j]>0 && c1[j]<0) || (a1[j]<0 && b1[j]>0 && c1[j]>0) || (a1[j]>0 && b1[j]<0 && c1[j]<0)|| (a1[j]<0 && b1[j]<0 && c1[j]>0)){
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-max(DF)
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
      PX<-c(1000000000000)
      PY<-c(1000000000000)

        }
   else{
   if((a2[j]>0 && b2[j]>0 && c2[j]<0) || (a2[j]<0 && b2[j]>0 && c2[j]>0) || (a2[j]>0 && b2[j]<0 && c2[j]<0)|| (a2[j]<0 && b2[j]<0 && c2[j]>0)){
       DF<-data.frame(Y=c(Y1[j],Y2[j]))
       PY[j]<-max(DF)
       PX[j]<-c(1000000000000)
        
        }
   else {
      if ((a2[j]>0 && b2[j]<0 && c2[j]>0) ||(a2[j]<0 && b2[j]>0 && c2[j]<0)){
      DF<-data.frame(Y=c(Y1[j],Y2[j]))
      PY[j]<-min(DF)
      PX[j]<-c(1000000000000)
       
       }
      }
     }
    }
else {
if ((((b1[j])^2-(4*a1[j]*c1[j]))< 0) && (((b2[j])^2-(4*a2[j]*c2[j]))<0)){
     PX[j]<-c(1000000000000)
     PY[j]<-c(1000000000000)

    }
   }
  }
 }


#step5: Output
cat("\n")
cat("<< Output and Linear regression model>>\n")
cat("\n")
cat("\nY=",Intercept,"+(",Slope[d],") X\n\n")
cat("\n")
output<-data.frame(W.split[[j]][["time"]],W.split[[j]][["assay"]],cal,Res)
colnames(output)<-list("time","Observed assay(%)","Calculated assay(%)","Residuals")  
show(output)
cat("\n\n")  
TIME[[j]]<-c(W.split[[j]][["time"]])
CAL[[j]]<-c(cal)
RRES[[j]]<-c(Res)
OBS[[j]]<-c(W.split[[j]][["assay"]])
}

AA<-melt(CAL)
ZZ<-melt(TIME)
QQ<-melt(RRES)
OO<-melt(OBS)
c(AA$L1)
#purpose: to plot one slope and three intercepts 
NewPred<-data.frame(batch=c(AA$L1),time=ZZ$value, PredCal=AA$value, RES=QQ$value)   
M.split<-split(NewPred, list(NewPred$batch) ) 

#choose min PX or PY
DM<-data.frame(batch=ba, Upper=PX, Lower=PY)
DM<- na.omit(DM)
PPX<-min(DM$Upper)
PPY<-min(DM$Lower)

windows(record = TRUE )
                                        
#step6: summary and plot
cat("**************************************************************************\n")
cat("                               << Output >>                               \n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
if  (PPX>=PPY)  {
     #go to plot of single batch
     i<-formatC(PPY,format="f",digits=2) 
     #i<-round(PPY,3)
     #做出plot的框架 
     main<-paste(c("Shelf Life=",i, "months"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata, xlim=c(0,(PPY+10)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
       
   
    temp <- legend("topright", legend = c(Intable$batch),
               text.width = strwidth("1000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)
   
    for(i in seq_along(M.split)){
     x<-M.split[[i]][["time"]] 
     y<-M.split[[i]][["PredCal"]]
     mod<-lm(y~x) 
     abline(mod,lwd=2, col=i)   
      }
#plot CI
   LX<-0
   LY<-0

   for(i in seq_along(W.split)){
     LX<-length(W.split[[i]]$time)
     LY<-mean(ANCOVAdata$time )

     Intercept1<-0
     for(k in 1: length(unique(Intable$batch))){
            if (W.split[[i]][["batch"]][1]==Intable$batch[[k]]){
                  Intercept1<- Intable$Intercept[[k]]
                 }
               }
               
       newx<-data.frame(xx=seq(0,(PPY+10)))
       yy<-Intercept1+newx$xx*Slope[d]
       mod1<-lm(yy~newx$xx)
       fit<-predict(mod1, newdata=newx,interval = c("confidence"),level = 0.95,type="response")
       pd<-(sqrt(1/(LX) + (((newx$xx-LY)^2)/Sxx)*SS))*T

       Lower<-fit[,1]-pd
       Upper<-fit[,1]+pd
       total<-data.frame(time=newx$xx, fit=fit[,1], Lower=Lower, Upper=Upper)
       cat("\n")
       show(total)
       cat("\n")
       lines(total$time,total$Lower,col=i,lty=2)
       lines(total$time,total$Upper,col=i,lty=2)
        }
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PPY, col = "gray60")
     cat("                                                                          \n")
     cat(" Drug product with lower acceptance criteria of ",Lper,"% of label claim  \n")
     cat(" Shelf life =",PPY,"months                                                 \n\n")
     cat("**************************************************************************\n")
     cat("\n")
       }
else {
    if(PPY>PPX){
     i<-formatC(PPX,format="f",digits=2) 
      #i<-round(PX,3) 與上述效果一樣... 
     main<-paste(c("Shelf Life=",i, "months"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata,xlim=c(0,(PPX+10)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)   
      
     temp <- legend("topright", legend = c(Intable$batch),
               text.width = strwidth("1000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)
     for(i in seq_along(M.split)){
      x<-M.split[[i]][["time"]] 
      y<-M.split[[i]][["PredCal"]]
      mod<-lm(y~x) 
      abline(mod,lwd=2, col=i)
        }
   #plot CI
   LX<-0
   LY<-0

   for(i in seq_along(W.split)){
     LX<-length(W.split[[i]]$time)
     LY<-mean(ANCOVAdata$time )

     Intercept1<-0
     for(k in 1: length(unique(Intable$batch))){
            if (W.split[[i]][["batch"]][1]==Intable$batch[[k]]){
                  Intercept1<- Intable$Intercept[[k]]
                 }
               }
       newx<-data.frame(xx=seq(0,(PPX+10)))
       yy<-Intercept1+newx$xx*Slope[d]
       mod1<-lm(yy~newx$xx)
       fit<-predict(mod1, newdata=newx,interval = c("confidence"),level = 0.95,type="response")
       pd<-(sqrt(1/(LX) + (((newx$xx-LY)^2)/Sxx)*SS))*T

       Lower<-fit[,1]-pd
       Upper<-fit[,1]+pd
       total<-data.frame(time=newx$xx, fit=fit[,1], Lower=Lower, Upper=Upper)
       show(total)
       lines(total$time,total$Lower,col=i,lty=2)
       lines(total$time,total$Upper,col=i,lty=2)
      
        }
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PPX, col = "gray60")
     
     cat("                                                                          \n")
     cat(" Drug product with Upper acceptance criteria of ",Uper,"% of label claim  \n")
     cat(" Shelf life =",PPX,"months                                                 \n\n")
     cat("**************************************************************************\n")
     cat("\n")
   
        } 
else{
  if ((PX=1000000000000) && (PY=1000000000000)){
   cat("--------------------------------------------------------------------------\n")
   cat("                    no solution at all                                    \n")
   cat("**************************************************************************\n")
   cat("\n")
    
      }     
     }
    } 
  qqnorm(QQ$value, las=1, main = "Normal Q-Q Plot of Residuals", col=c(QQ$L1))  
      
         temp <- legend("topleft", legend = c(Intable$batch),
               text.width = strwidth("1000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)   
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
cat(" significant difference in slopes among batches).                         \n")
cat("                                                                          \n")
cat("                            <<Suggestion>>                                \n")
cat("     It is not appropriate to combine the data from all batches.          \n")
cat("--------------------------------------------------------------------------\n")     
cat("<<Output: linear regression model: time vs. assay (%)>>\n")
cat("\n")
show(lm(assay ~ batch*time  , data=ANCOVAdata) )
show(anova(lm(assay ~ batch*time, data=ANCOVAdata) ))
W.split<-split(ANCOVAdata, list(ANCOVAdata$batch) )
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
        T[j]<-qt(0.975,L[j]-2) 
#step3: calculate possible X1,X2,Y1,Y2   
 #Slope: (Xi/(L*Sxx))*Yi+ (-mean(Singledata$time)/Sxx)*XYi
      #Intercept: (-mean(Singledata$time)/Sxx)*Yi+ (1/Sxx)*XYi
      #calculate shelf life
      #intersect 95% CI with Upper criteria
        delta1[j]<-Intercept-Uper
        a1[j]<-(1/Sxx[j])-(Slope/(T[j]*SK[j]))^2
        b1[j]<-2*(-mean(W.split[[j]][["time"]])/Sxx[j])-((2*Slope*delta1[j])/(T[j]*SK[j])^2)
        c1[j]<-(Xi[j]/(L[j]*Sxx[j]))-(delta1[j]/(T[j]*SK[j]))^2
        X1[j]<-((-b1[j])+(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j])
        X2[j]<-((-b1[j])-(sqrt((b1[j])^2-(4*a1[j]*c1[j]))))/(2*a1[j])
     
     #intersect 5% CI with Lower criteria
        delta2[j]<-Intercept-Lper
        a2[j]<-(1/Sxx[j])-(Slope/(T[j]*SK[j]))^2
        b2[j]<-2*(-mean(W.split[[j]][["time"]])/Sxx[j])-((2*Slope*delta2[j])/(T[j]*SK[j])^2)
        c2[j]<-(Xi[j]/(L[j]*Sxx[j]))-(delta2[j]/(T[j]*SK[j]))^2
        Y1[j]<-((-b2[j])+(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
        Y2[j]<-((-b2[j])-(sqrt((b2[j])^2-(4*a2[j]*c2[j]))))/(2*a2[j])
    
#step4: make decision
if ((((b1[j])^2-(4*a1[j]*c1[j]))>=0) && (((b2[j])^2-(4*a2[j]*c2[j]))>=0)){
  if ((a1[j]>0 && b1[j]>0 && c1[j]>0) || (a1[j]<0 && b1[j]<0 && c1[j]<0)){
      PX[j]<-c(1000000000000)

      }
  else{
  if ((a1[j]>0 && b1[j]>0 && c1[j]<0) || (a1[j]<0 && b1[j]>0 && c1[j]>0) || (a1[j]>0 && b1[j]<0 && c1[j]<0)|| (a1[j]<0 && b1[j]<0 && c1[j]>0)){
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-max(DF)

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
   if((a2[j]>0 && b2[j]>0 && c2[j]<0) || (a2[j]<0 && b2[j]>0 && c2[j]>0) || (a2[j]>0 && b2[j]<0 && c2[j]<0)|| (a2[j]<0 && b2[j]<0 && c2[j]>0)){
       DF<-data.frame(Y=c(Y1[j],Y2[j]))
       PY[j]<-max(DF)
        
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
      PX[j]<-c(1000000000000)
      PY[j]<-c(1000000000000)
      
      }
  else{
  if ((a1[j]>0 && b1[j]>0 && c1[j]<0) || (a1[j]<0 && b1[j]>0 && c1[j]>0) || (a1[j]>0 && b1[j]<0 && c1[j]<0)|| (a1[j]<0 && b1[j]<0 && c1[j]>0)){
      DF<-data.frame(X=c(X1[j],X2[j]))
      PX[j]<-max(DF)
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
      PX<-c(1000000000000)
      PY<-c(1000000000000)

        }
   else{
   if((a2[j]>0 && b2[j]>0 && c2[j]<0) || (a2[j]<0 && b2[j]>0 && c2[j]>0) || (a2[j]>0 && b2[j]<0 && c2[j]<0)|| (a2[j]<0 && b2[j]<0 && c2[j]>0)){
       DF<-data.frame(Y=c(Y1[j],Y2[j]))
       PY[j]<-max(DF)
       PX[j]<-c(1000000000000)
        
        }
   else {
      if ((a2[j]>0 && b2[j]<0 && c2[j]>0) ||(a2[j]<0 && b2[j]>0 && c2[j]<0)){
      DF<-data.frame(Y=c(Y1[j],Y2[j]))
      PY[j]<-min(DF)
      PX[j]<-c(1000000000000)
       
       }
      }
     }
    }
else {
if ((((b1[j])^2-(4*a1[j]*c1[j]))< 0) && (((b2[j])^2-(4*a2[j]*c2[j]))<0)){
     PX[j]<-c(1000000000000)
     PY[j]<-c(1000000000000)

    }
   }
  }
 }

#step5: Output
cat("\n")
cat("<< Output and Linear regression model>>\n")
cat("\n")
cat("\nY=",Intercept,"+(",Slope,") X\n\n")
cat("\n")
output<-data.frame(W.split[[j]][["time"]],W.split[[j]][["assay"]],cal,Res)
colnames(output)<-list("time","Observed assay(%)","Calculated assay(%)","Residuals")  
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
DM<-data.frame(batch=ba, Upper=PX, Lower=PY)
DM<- na.omit(DM)
PPX<-min(DM$Upper)
PPY<-min(DM$Lower)

windows(record = TRUE )
#step6: summary and plot
cat("**************************************************************************\n")
cat("                       << Summary and plots >>                            \n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
if  (PPX>=PPY)  {
     #go to plot of single batch
     i<-formatC(PPY,format="f",digits=2) 
     #i<-round(PPY,3)
     #做出plot的框架 
     main<-paste(c("Shelf Life=",i, "months (PgDn to switch plots)"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata, xlim=c(0,(PPY+10)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE) 
    
     temp <- legend("topright", legend = c(ba),
               text.width = strwidth("1000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1) 
     
      #plot CI
     newx<-data.frame(xx=seq(0,(PPY+10))) 
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
     cat("--------------------------------------------------------------------------\n")
     cat("                                                                          \n")
     cat(" Drug product with lower acceptance criteria of ",Lper,"% of label claim  \n")
     cat(" Shelf life =",PPY,"months                                                 \n\n")
     cat("**************************************************************************\n")
     cat("\n")
       }
else {
    if(PPY>PPX){
     i<-formatC(PPX,format="f",digits=2) 
      #i<-round(PX,3) 與上述效果一樣... 
     main<-paste(c("Shelf Life=",i, "months"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata,xlim=c(0,(PPX+10)),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)   
      temp <- legend("topright", legend = c(ba),
               text.width = strwidth("1000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)
      #plot CI
      newx<-data.frame(xx=seq(0,(PPY+10)))  
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
     
     cat("--------------------------------------------------------------------------\n")
     cat("                                                                          \n")
     cat(" Drug product with upper acceptance criteria of ",Uper,"% of label claim  \n")
     cat(" Shelf life =",PPX,"months                                                 \n\n")
     cat("**************************************************************************\n")
     cat("\n")
        } 
else{
  if ((PX=1000000000000) && (PY=1000000000000)){
   cat("--------------------------------------------------------------------------\n")
   cat("                    no solution at all                                    \n")
   cat("**************************************************************************\n")
   cat("\n")
   
      }     
     }
    }    
        qqnorm(QQ$value, las=1, main = "Normal QQ Plot of Residuals (PgUp to switch plots", col=c(QQ$L1))  
      
         temp <- legend("topleft", legend = c(Intable$batch),
               text.width = strwidth("1000"),
               lty=1, col=c(Intable$batch), xjust = 1, yjust = 1)     
      
      bye()
   
   }  
  }
 }  
} 



