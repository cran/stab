### Input assay data Menu for Data Analysis for a Single Batch
### library(stats4)   ### not required any more. -YJ
### library(MASS)
#predict req. stats4 package
#confidence interval req. MASS package
### SingleAnalyze<-function(Singledata, Uper, Lper, separateWindows = TRUE)
SingleAnalyze<-function(Singledata, separateWindows = TRUE)
{
stab_output_filename<-stab_output_filename
onesidedlo <- onesidedlo
onesidedup <- onesidedup
twosided   <- twosided
Lper<-Lper
Uper<-Uper
CI_percent<-CI_percent
PY<-0
PPY<-0
noPY = FALSE
noSolution = FALSE
analysis_type<-""
ltmp<-0
switch_UL= FALSE
assay<-NULL
pdf_activate=FALSE  ### set pdf device activate as FALSE at beginning

filepath<-getwd()
cat("\n")
outputfile <- stab_output_filename
output_to_txt <- paste(outputfile,".txt",sep="")
plots_to_pdf <- paste(outputfile,".pdf",sep="")
while(file.exists(output_to_txt)){
      output_to_txt<-gsub(".txt","",output_to_txt,fixed=TRUE)
      output_to_txt<-paste(output_to_txt,"_01.txt",sep="")
}
while(file.exists(plots_to_pdf)){
      plots_to_pdf<-gsub(".pdf","",plots_to_pdf,fixed=TRUE)
      plots_to_pdf<-paste(plots_to_pdf,"_01.pdf",sep="")
}
cat("*******************************************************************\n\n")
cat(" Note: The output file (",outputfile,") will be created \n")
cat(" and placed at",filepath,"\n")
cat("*******************************************************************\n\n")

### file.menu <- c("the one-sided lower LC analysis",
###                "the one-sided upper LC analysis",
###                "the two-sided analysis")
### cat("\n")
### pick <- menu(file.menu, title = "<< Stability analysis for Multiple batches >> ", graphics=TRUE)
if (onesidedlo){
    cat("\n")
    analysis_type<-"the one-sided lower LC analysis"
    onesidedlo = TRUE
    cat("\n ** The one-sided lower LC analysis is selected.** \n")
              }
else {
      if (onesidedup){
          cat("\n")
          analysis_type<-"the one-sided upper LC analysis"
          onesidedup = TRUE
          cat("\n ** The one-sided upper LC analysis is selected.** \n")
                    }
      else {
            if (twosided){
                cat("\n")
                analysis_type<-"the two-sided LC analysis"
                twosided = TRUE
                cat("\n ** The two-sided LC analysis is selected.** \n")
              }
            }
      }

             
### cat("\n")
### if(onesidedlo){
###    cat("\n")
###    set.stab<-data.frame(Parameter=c("Lower Limit (%)","Percent CI (%)"),value=c(90,95))
###    set.stab<-edit(set.stab)
###    Lper<-set.stab[1,2]
###    if(Lper<=0 ||Lper>120.) Lper<-90       ### set as default if going wrong.
###    CI_percent<- set.stab[2,2]/100
###    if(CI_percent<=0 ||CI_percent>1.) CI_percent<-0.95          ### set as default if going wrong.
###               }
###  else {
###     if(onesidedup){
###       set.stab<-data.frame(Parameter=c("Upper Limit (%)","Percent CI (%)"),value=c(110,95))
###       set.stab<-edit(set.stab)
###       Uper<-set.stab[1,2]
###       if(Uper<=0 ||Uper>150.) Uper<-110       ### set as default if going wrong.
###       CI_percent<- set.stab[2,2]/100
###       if(CI_percent<=0 ||CI_percent>1.) CI_percent<-0.95       ### set as default if going wrong.
###                  }
###      else{
###           set.stab<-data.frame(Parameter=c("Lower Limit (%)","Upper Limit (%)","Percent CI (%)"),value=c(90,110,95))
###           set.stab<-edit(set.stab)
###           Lper<-set.stab[1,2]
###           if(Lper<=0 ||Lper>100.) Lper<-90     ### set as default if going wrong.
###           Uper<-set.stab[2,2]
###           if(Uper<=0 ||Uper>150.) Uper<-110    ### set as default if going wrong.
###           CI_percent<- set.stab[3,2]/100
###           if(CI_percent<=0 ||CI_percent>1.) CI_percent<-0.95     ### set as default if going wrong.
###            }
###        }

level_one<-0
level_two<-0
level_one<-(1-(1-CI_percent)*2)      ### %CI level for predict(..., level = level_one) with one-sided study
level_two<-CI_percent                ### %CI level for predict(..., level = level_two) with two-sided study

zz <- file(output_to_txt, open="wt")
sink(zz,split=TRUE)
stab.version()

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
    if(switch_UL) {
    cat("*** You input conflict data with the Upper and Lower limits.\n")
    cat("    They have been switched automatically.\n\n")}  ### to avoid accidentally input err. -YJ
    }
}
cat("<< --- List of input data --- >>\n\n")
show(Singledata)
cat("\n\n")

#fitting data with linear regression model
cat("<<Output: linear regression model: assay (%) vs. time>>\n\n")
cat("                Single batch analysis                  \n")
show(lm(Singledata$assay~Singledata$time))
show(anova(salm<-lm( Singledata$assay~Singledata$time)))
print(summary(salm<-lm(Singledata$assay~Singledata$time)))
summary(salm<-lm(Singledata$assay~Singledata$time))
Intercept<-coef(lm(Singledata$assay~Singledata$time))[1]
Slope<-coef(lm(Singledata$assay~Singledata$time))[2]
cat("\n")
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
#95%CI, n-2 T value
 if(onesidedlo || onesidedup){
    T<-qt(CI_percent,L-2)
    x<-Singledata$time
    y<-Singledata$assay
    mylm<-lm(y~x)
    newx<-data.frame(xx=seq(0,84))
    pred<-predict(mylm,newdata=data.frame(x=newx$xx),interval = c("confidence"),level = level_one,type="response")
    if (onesidedlo) {
       total<-data.frame(time=newx$xx, Lower=pred[,2], fit=pred[,1])
       PY<-0
       for (i in 1:length(newx$xx)){if (total$Lower[i]<Lper){PY<-(i-2);break()}}
       if (PY==0||PY<0) {PY<-84} 
            }
        
    if (onesidedup) {
          total<-data.frame(time=newx$xx, fit=pred[,1], Upper=pred[,3])
          PY<-0
          for (i in 1:length(newx$xx)){if (total$Upper[i]>Uper){PY<-(i-2);break() }}
          if (PY==0||PY<0) {PY<-84}
          }
       }
 else {
    T<-qt(1-((1-CI_percent)/2),L-2)
    x<-Singledata$time
    y<-Singledata$assay
    mylm<-lm(y~x)
    newx<-data.frame(xx=seq(0,84))
    pred<-predict(mylm,newdata=data.frame(x=newx$xx),interval = c("confidence"),level = level_two,type="response")
       total<-data.frame(time=newx$xx, Lower=pred[,2], fit=pred[,1], Upper=pred[,3])
       PY<-0
       PY1<-0
       PY2<-0
       for (i in 1:length(newx$xx)){if (total$Lower[i]<Lper){PY1<-(i-2);break()}}
       if (PY1==0||PY1<0) {PY1<-84}

       for (i in 1:length(newx$xx)){if (total$Upper[i]>Uper){PY2<-(i-2);break() }}
       if (PY2==0||PY2<0) {PY2<-84}
       PY<-min(PY1,PY2)
 }

PPY<-PY             # only one 'PY' for model#1.  -YJ
if ((PPY==84)) noPY = TRUE

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
### delete many lines here; basically it's solving a quadratic equation. for finding intersect.
#########
### windows(record = TRUE)
#Output
cat("*********************************************************************\n")
cat("                    << Output - single batch >>                      \n")
cat("---------------------------------------------------------------------\n")
cat("                <<Summary: linear regression model>>                 \n\n")
cat(paste(c("   Y (assay, %) =",formatC(coef(lm(Singledata$assay~Singledata$time))[1],digits=5,format="f"),
    "+ (",formatC(coef(lm(Singledata$assay~Singledata$time))[2],digits=5,format="f"),")*Time\n\n",sep="")))
output<-data.frame(Singledata$time,Singledata$assay,cal,Res)
colnames(output)<-list("Time","Obs. assay(%)","Calc. assay(%)","Residuals")
show(output)
###
### show 95 %CI
### try to label star mark to right-hand side here -YJ
###
newx<-data.frame(xx=seq(0,84))
if (onesidedlo){
     total<-data.frame(time=newx$xx, fit=pred[,1], Lower=pred[,2], star="",stringsAsFactors=F)
     for(i in 1:(length(newx$xx)-1)){if (i>PY) total[i+1,]$star="***"}}
if (onesidedup){
     total<-data.frame(time=newx$xx, fit=pred[,1], Upper=pred[,3], star="",stringsAsFactors=F)
     for(i in 1:(length(newx$ss)-1)){if (i>PY) total[i+1,]$star="***"}}
if (twosided){
     total<-data.frame(time=newx$xx, Lower=pred[,2],fit=pred[,1], Upper=pred[,3], star="",stringsAsFactors=F)
     for(i in 1:(length(newx$xx)-1)){if (i>PY) total[i+1,]$star="***"}}
cat("\n")
cat("-- List of",CI_percent*100,"% CI for 84-month Time Interval:-\n\n")
show(total)
cat("\n ***: means the listing of expiration as defined.\n\n")
###
cat("*********************************************************************\n")
cat("                      << Summary and plots >>                        \n")
cat("---------------------------------------------------------------------\n")
cat("\n")

##### if one-sided low, start from here
if (onesidedlo) {
cat("  One-sided lower LC analysis                   \n\n")
if (noPY){
cat("  No solution can be found with this model. Please try others.       \n")
cat("*********************************************************************\n")
   noSolution = TRUE
}
else {
     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
     shelflife<-as.integer(PPY)
     main<-paste(c("estimated shelf-life = ",shelflife, " months with ",CI_percent*100,
                   "% CI\n  (",analysis_type,")"),collapse="")
     x<-Singledata$time
     y<-Singledata$assay
     plot(x,y,xlim=c(0,84),ylim=c((Lper-10),(Lper+30)), main=main,
     xlab = "Time (months)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n", frame.plot=FALSE)
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
       
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
#plot CI
     newx<-seq(0,84)
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),level = level_one,type="response")
     lines(newx,prd[,2],col="blue",lty=4,lwd=2)
###
### no legend for single batch run
###
#add criteria limit
     abline(h=Lper, col = "red")
     abline(v=PY, col = "black")
     if(pdf_activate){
        dev.copy()                      ## copy to pdf file 2nd plots to end
        dev.set(which=x11c)             ## back to graphic device now to continue...
                     }
     else{
        x11c<-dev.cur()                 ## the current graphics device
        pdf(plots_to_pdf,               ## activate pdf log file from now on... starting with ref. product
             paper="a4")
        pdf_activate=TRUE               ## set pdf_activate=TRUE from now on
        dev.set(which=x11c)             ## back to graphics device...
        dev.copy()                      ## copy the first plot from here
        dev.set(which=x11c)             ## back to graphics device
     }     
     cat("----------------------------------------------------------------   \n\n")
     cat(" Drug product with lower acceptance limit of ",Lper,"% of label claim\n")
     cat(" Shelf-life =",shelflife,"(months)                                 \n\n")
     cat("******************************************************************   \n")
     cat("\n")
     }
}
##### end of one-sided lower
##### if one-sided upper, start here.
if (onesidedup){
cat("  One-sided upper LC analysis                     \n\n")
if (noPY){
cat("  No solution can be found with this model. Please try others.       \n")
cat("*********************************************************************\n")
   noSolution = TRUE
}
else {
     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
     shelflife<-as.integer(PY)
     main<-paste(c("estimated shelf-life = ",shelflife, " months with ",CI_percent*100,
                   "% CI\n  (",analysis_type,")"),collapse="")
     x<-Singledata$time
     y<-Singledata$assay
     plot(x,y,xlim=c(0,84),ylim=c((Uper-30),(Uper+10)), main=main,
     xlab = "Time (months)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n", frame.plot=FALSE)
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
       
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,84)
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),level = level_one,type="response")
     lines(newx,prd[,3],col="blue",lty=4,lwd=2)
     #add criteria limit
     abline(h=Uper, col = "red")
     abline(v=PY, col = "black")
     if(pdf_activate){
        dev.copy()                      ## copy to pdf file 2nd plots to end
        dev.set(which=x11c)             ## back to graphic device now to continue...
                     }
     else{
        x11c<-dev.cur()                 ## the current graphics device
        pdf(plots_to_pdf,               ## activate pdf log file from now on... starting with ref. product
             paper="a4")
        pdf_activate=TRUE               ## set pdf_activate=TRUE from now on
        dev.set(which=x11c)             ## back to graphics device...
        dev.copy()                      ## copy the first plot from here
        dev.set(which=x11c)             ## back to graphics device
     }     
     cat("----------------------------------------------------------------\n\n")
     cat(" Drug product with upper acceptance limit of ",Uper,"% of label claim\n")
     cat(" Shelf life =",shelflife,"(months)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
     }
}
##### end of one-sided upper
##### if two-sided, start here
if (twosided){
cat("  Two-sided LC analysis                       \n\n")
if (noPY){
cat("  No solution can be found with this model. Please try others.       \n")
cat("*********************************************************************\n")
    noSolution = TRUE
}
else {
     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
     shelflife<-as.integer(PY)
     main<-paste(c("estimated shelf-life = ",shelflife, " months with ",CI_percent*100,
                   "% CI\n  (",analysis_type,")"),collapse="")
     x<-Singledata$time
     y<-Singledata$assay
     plot(x,y,xlim=c(0,84),ylim=c((Lper-10),(Lper+30)), main=main,
     xlab = "Time (months)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n")   
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
       
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,84)
     prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),level = level_two,type="response")
     lines(newx,prd[,2],col="blue",lty=4,lwd=2)
     lines(newx,prd[,3],col="blue",lty=4,lwd=2)
     #add criteria limit
     abline(h=Lper, col = "red")
     abline(h=Uper, col = "red")
     abline(v=PY, col = "black")
     if(pdf_activate){
        dev.copy()                      ## copy to pdf file 2nd plots to end
        dev.set(which=x11c)             ## back to graphic device now to continue...
                     }
     else{
        x11c<-dev.cur()                 ## the current graphics device
        pdf(plots_to_pdf,               ## activate pdf log file from now on... starting with ref. product
             paper="a4")
        pdf_activate=TRUE               ## set pdf_activate=TRUE from now on
        dev.set(which=x11c)             ## back to graphics device...
        dev.copy()                      ## copy the first plot from here
        dev.set(which=x11c)             ## back to graphics device
     }     
     cat("------------------------------------------------------------------\n\n")
     cat(" Drug product with upper acceptance limit of",Uper,"% of label claim\n")
     cat(" shelf-life =",shelflife,"(months)                           \n\n")
     cat("******************************************************************\n")
     cat("\n")
     }
  }

### end of two-sided.
### do Q-Q plot only if there is at least one solution (shelf-life)
if (!noSolution) {
    qqnorm(output$Res, las=1, main = "Normal Q-Q Plot of Residuals",lwd=3,col="blue",frame.plot=FALSE) 
    }
   if(pdf_activate){
      dev.copy()                      ## copy to pdf file 2nd plots to end
      dev.set(which=x11c)             ## back to graphic device now to continue...
                   }
   else{
      x11c<-dev.cur()                 ## the current graphics device
      pdf(plots_to_pdf,               ## activate pdf log file from now on... starting with ref. product
           paper="a4")
      pdf_activate=TRUE               ## set pdf_activate=TRUE from now on
      dev.set(which=x11c)             ## back to graphics device...
      dev.copy()                      ## copy the first plot from here
      dev.set(which=x11c)             ## back to graphics device
   }    
    sink()
    close(zz)
    readline(" Done. Press any key to continue...")
    if(noPY){graphics.off()}                       ## since no PY can be found only dev.off().
      else{dev.off();dev.off()}      ## req. to clode X-windows and pdf output file.
    cat("*****************************************************************************\n\n")
    cat("## Please note: The output files (",output_to_txt,") and (",plots_to_pdf,")     \n")
    cat("   have been created and placed at ",filepath,                               "\n\n")
    cat("*****************************************************************************\n\n")  
    SingleBatchmenu()
}

### end of SingleAnalyze