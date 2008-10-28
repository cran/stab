#Demo file for three batch
library(stats4)
library(MASS)
demostability<-function()
{
cat("****************************************************************************\n")
cat("* Input/Edit :three Batches with Assay (%)                                 *\n")
cat("****************************************************************************\n")
cat("\n\n")

Multipledata<-data.frame(batch=c(990031,990031,990031,990031,990031,990031,990031,990022,990022,   
                        990022,990022,990022,990022,990022,990033,990033,990033,990033,990033,990033,990033), 
                        time=c(0,1,2,3,6,9,12,0,1,2,3,6,9,12,0,1,2,3,6,9,12),
                        assay=c(100,101,102,101,103,105,103,102,99,98,98,97,98,97,100,97,97,96,98,97,96))
ANCOVAdata<-data.frame(batch=factor(c(990031,990031,990031,990031,990031,990031,990031,990022,990022,   
                        990022,990022,990022,990022,990022,990033,990033,990033,990033,990033,990033,990033)), 
                        time=c(0,1,2,3,6,9,12,0,1,2,3,6,9,12,0,1,2,3,6,9,12),
                        assay=c(100,101,102,101,103,105,103,102,99,98,98,97,98,97,100,97,97,96,98,97,96))

show(ANCOVAdata)

cat("\n")
cat("Drug product with upper acceptance criteria of ____ % of label claim\n")
cat("110\n")
Uper <- 110
cat("\n")
cat("Drug product with lower acceptance criteria of ____ % of label claim\n")
Lper <- 90
cat("90\n")
cat("\n")
cat("****************************************************************************\n")
cat("* To evaluate stability data with ANCOVA                                   *\n")
cat("****************************************************************************\n")
cat("<<Output: ANCOVA model: batch vs. time vs. assay (%)>>\n")
cat("\n")
kk<- lm(assay ~ batch*time, data=ANCOVAdata)
show(anova(kk))
cat("\n")
cat("Intercept:  P<0.25\n")
cat("Slope:  P<0.25\n")
cat("\n")
cat("--------------------------------------------------------------------------\n")
cat("          << ANCOVA Output: Testing for poolability of batches >>         \n")
cat("--------------------------------------------------------------------------\n")
cat("                                                                          \n")
cat(" The test rejects the hypothesis of equality of slopes (there is a significant\n")
cat(" difference in slopes among batches).                                     \n")
cat("                                                                          \n")
cat("                            <<Suggestion>>                                \n")
cat("  It is not considered appropriate to combine the data from all batches.  \n")
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
   show(Intable)
cat("\n")
#step2: calculate description statistics
     #divide data in to different group based on batches
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
cat("<< Output and Linear regression model>>\n")
cat("\n")
cat("\nY=",Intercept,"+(",Slope,") X\n\n")
cat("\n")
output<-data.frame(W.split[[j]][["time"]],W.split[[j]][["assay"]],cal,Res)
colnames(output)<-list("time","Observed assay(%)","Calculated assay(%)","Residuals")  
show(output)
cat("\n\n")  
}


#choose min PX or PY
DM<-data.frame(batch=ba, Upper=PX, Lower=PY)
DM<- na.omit(DM)
PPX<-min(DM$Upper)
PPY<-min(DM$Lower)


get(getOption("device"))()
           
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
     go()
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
         }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #add criteria limit
     abline(h=Uper, col = "gray60")
     abline(h=Lper, col = "gray60")
     abline(v=PPX, col = "gray60")
     
     cat("--------------------------------------------------------------------------\n")
     cat("                                                                          \n")
     cat(" Drug product with Upper acceptance criteria of ",Uper,"% of label claim  \n")
     cat(" Shelf life =",PPX,"months                                                 \n\n")
     cat("**************************************************************************\n")
     cat("\n")
     go()
        } 
else{
  if ((PX=1000000000000) && (PY=1000000000000)){
   cat("--------------------------------------------------------------------------\n")
   cat("                    no solution                                           \n")
   cat("**************************************************************************\n")
   cat("\n")
     go()
      }     
     }
    }    


}            