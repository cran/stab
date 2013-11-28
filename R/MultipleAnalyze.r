#Data Analysis for Multiple Batches
## library(stats4)        ### nor required these three lines.  -YJ
## library(MASS)
## library(reshape) 
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
PPY<-0
PY1<-0
PY2<-0
ltmp<-0
switch_UL= FALSE
assay<-NULL
pdf_activate=FALSE  ### set pdf device activate as FALSE at beginning

file.menu <- c(" the one-sided lower LC analysis  ",
               " the one-sided upper LC analysis  ",
               " the two-sided LC analysis        ")
cat("\n")
pick <- menu(file.menu, title = "<< Types of Analysis:- Multiple batches >>", graphics=TRUE)
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

Lper<-0.
Uper<-0.
CI_percent<-0.

cat("\n")
if(onesidedlo){
   cat("\n")
   set.stab<-data.frame(Parameter=c("Lower Limit (%)","Percent CI (%)"),value=c(90,95))
   set.stab<-edit(set.stab)
   Lper<-set.stab[1,2]
   if(Lper<=0 ||Lper>120.) Lper<-90       ### set as default if going wrong.
   CI_percent<- set.stab[2,2]/100
   if(CI_percent<=0 ||CI_percent>1.) CI_percent<-0.95          ### set as default if going wrong.
        }
 else {
      if(onesidedup){
      set.stab<-data.frame(Parameter=c("Upper Limit (%)","Percent CI (%)"),value=c(110,95))
      set.stab<-edit(set.stab)
      Uper<-set.stab[1,2]
      if(Uper<=0 ||Uper>150.) Uper<-110       ### set as default if going wrong.
      CI_percent<- set.stab[2,2]/100
      if(CI_percent<=0 ||CI_percent>1.) CI_percent<-0.95       ### set as default if going wrong.
           }
     else{
          set.stab<-data.frame(Parameter=c("Lower Limit (%)","Upper Limit (%)","Percent CI (%)"),value=c(90,110,95))
          set.stab<-edit(set.stab)
          Lper<-set.stab[1,2]
          if(Lper<=0 ||Lper>100.) Lper<-90     ### set as default if going wrong.
          Uper<-set.stab[2,2]
          if(Uper<=0 ||Uper>150.) Uper<-110    ### set as default if going wrong.
          CI_percent<- set.stab[3,2]/100
          if(CI_percent<=0 ||CI_percent>1.) CI_percent<-0.95     ### set as default if going wrong.
      }
  }
filepath<-getwd()
cat("\n")
outputfile <- readline("Enter the output file name (no extension!): ")
output_to_txt <- paste(outputfile,".txt",sep="")
plots_to_pdf <- paste(outputfile,".pdf",sep="")
cat("\n\n")
cat("*****************************************************************\n")
cat("*                 Analyzing the data now...                     *\n")
cat("*****************************************************************\n\n")
zz <- file(output_to_txt, open="wt")
sink(zz,split=TRUE)              ### add 'split=TRUE" here to see it on screen too. - YJ
stab.version()
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
    if(switch_UL) {
    cat("*** You input conflict data with the Upper and Lower limits.\n")
    cat("    They have been switched automatically.\n\n")}  ### to avoid accidentally input err. -YJ
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
     for (i in 1:(length(W.split))){     ### get batch# here -YJ
     ba[i]<-W.split[[i]][["batch"]][1]
     }
     
#collect all intercepts in a dataframe
     Intable<-data.frame(batch=ba)

#step1:get a slope and a intercept
Intercept<-coef(lm(assay ~ time, data=ANCOVAdata))[1]   ### first get common 'intercept' here
Slope<-coef(lm(assay ~ time, data=ANCOVAdata))[2]       ### 2nd get common 'slope' here

#step2: calculate description statistics
#Calculated assay
   cal<-predict(lm(assay ~ time, data=ANCOVAdata))
#extract model residuals
   Res<-residuals(lm(assay ~ time, data=ANCOVAdata))
#number of data points
   L<-length(ANCOVAdata$assay)
      

### 95%CI,n-2 T value
###       
### prepare to construct 95%CI & find intersect here! -YJ 
###      
cat(" --- Batch#:", ba,"---\n")   ### starting output here

if(onesidedlo || onesidedup){
       ### T<-qt(0.95,L-2)  ### for one-sided criteria; not used with this line.
       T<-qt(CI_percent,L-2)
       x<-ANCOVAdata$time            ### here pool all data together because they all share same intercept & slope (for model#1)
       y<-ANCOVAdata$assay           ### here pool all data together because they all share same intercept & slope (for model#1)
       mylm<-lm(y~x)
       newx<-data.frame(xx=seq(0,84))  ### here 7 years max. (= 84 months)  -YJ
       pred<-predict(mylm,newdata=data.frame(x=newx$xx),interval = c("confidence"),level = 0.90,type="response")
       if (onesidedlo) {
          total<-data.frame(time=newx$xx, Lower=pred[,2], fit=pred[,1])
          PY<-0
          for (i in 1:length(newx$xx)){
             if (total$Lower[i]<Lper){
                 PY<-(i-2);break()}}     ### this is shelf-life for one-sided lower; once it is found (shortest) it will stop by break(). -YJ
             if (PY==0||PY<0) {PY<-84}   ### means cannot find a solution
            }
        
       if (onesidedup) {
          total<-data.frame(time=newx$xx, fit=pred[,1], Upper=pred[,3])
          PY<-0
          for (i in 1:length(newx$xx)){
              if (total$Upper[i]>Uper){
                 PY<-(i-2);break() }}     ### this is shelf-life for one-sided lower; once it is found (shortest) it will stop by break(). -YJ
              if (PY==0||PY<0) {PY<-84}   ### means cannot find a solution; still PY = 84 (that's no solution!)
          }
       } 
else {
       ### T<-qt(0.975,L-2)              ### for two-sided criteria; this line is not used. consider to delete it.
       T<-qt(1-((1-CI_percent)/2),L-2)
       x<-ANCOVAdata$time                ### here pool all data together because they all share same intercept & slope (for model#1)
       y<-ANCOVAdata$assay
###       
### prepare to construct 95%CI here -YJ 
###      
       mylm<-lm(y~x)
       newx<-data.frame(xx=seq(0,84))
       pred<-predict(mylm,newdata=data.frame(x=newx$xx),interval = c("confidence"),level = 0.95,type="response")
       total<-data.frame(time=newx$xx, Lower=pred[,2], fit=pred[,1], Upper=pred[,3])
       PY<-0
       PY1<-0
       PY2<-0
       for (i in 1:length(newx$xx)){
             if (total$Lower[i]<Lper){
                 PY1<-(i-2);break()}}        ### this is one intersect for lower line; use break() to stop finding... --YJ (great!)
       if (PY1==0||PY1<0) {PY1<-84}          ### means cannot find a solution for lower

       for (i in 1:length(newx$xx)){
              if (total$Upper[i]>Uper){
                 PY2<-(i-2);break() }}      ### this is one intersect for upper line; use break() to stop finding... --YJ (great!)
       if (PY2==0||PY2<0) {PY2<-84}         ### means cannot find a solution for upper
       PY<-min(PY1,PY2)                     ### pick a minimum PY as shelf-life here if there are two intersects. -YJ
} 

PPY<-PY             # only one 'PY' for model#1.  -YJ
if ((PPY==84)) noPY = TRUE

#########
### Output
cat("\n Y =",coef(lm(ANCOVAdata$assay~ANCOVAdata$time))[1],"+(",coef(lm(ANCOVAdata$assay~ANCOVAdata$time))[2],") X\n\n")
cat("\n")
output<-data.frame(ANCOVAdata$batch,ANCOVAdata$time,ANCOVAdata$assay,cal,Res)     ### previous calc. only 'cal' & 'Res' should be kept. -YJ
colnames(output)<-list(" Batch#"," Time"," Obs. assay(%)"," Cal. assay(%)"," Residuals")
show(output)
cat("\n\n")
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
     cat("\n\n")
     cat("-- List of",CI_percent*100,"% CI for 84-month Time Interval:-\n\n")
     show(total)
     cat("\n\n ***: means the listing of expiration as defined.")
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
     colnames(allSF)<-c(" batch#", "  shelf-life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf-life\n\n")
     cat(" Ps. When shelf-life = 84, it means that stab\n")
     cat(" cannot find a reasonable shelf-life for the\n")
     cat(" batch with presented dataset within 84 months.\n\n")

     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
     #i<-formatC(PX,format="f",digits=2) 
     #i<-round(PX,3) #same as the above 
     shelflife<-as.integer(PPY)
     main<-paste(c("estimated shelf-life =",shelflife, "months"),collapse=" ")
     x<-ANCOVAdata$time
     y<-ANCOVAdata$assay
     plot(x,y,xlim=c(0,84),ylim=c((Lper-10),(Lper+20)),main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n", frame.plot=FALSE)
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
###
### no legend here; data have been combined all together.
### but... say something here
     leg_txt<-"All Pooled Data from Batches (Model#1)"
     legend("top",leg_txt,xjust=0,yjust=0,box.col="white")

     #plot CI
     newx<-seq(0,84)
     pred<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),level = 0.90,type="response")
     lines(newx,pred[,2],col="red",lty=4,lwd=2)    ### pred[,2] = Lower bound -YJ

     #add criteria limit
     abline(h=Lper, col = "red")
     abline(v=shelflife, col = "black")
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
     cat(" Drug product with lower acceptance limit of",Lper,"% of label claim\n")
     cat(" shelf-life =",shelflife,"(months)                          \n\n")
     cat("******************************************************************\n")
     cat("\n")
      }     
}
##### end of one-sided low ##########################################################
##### if one-sided upper, start from here
if (onesidedup ){
cat("\n")
cat("                     One-sided upper LC analysis                        \n\n")
if (noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
    noSolution = TRUE
}
else { 
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf-life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf-life\n\n")
     cat(" Ps. When shelf-life = 84, it means that stab\n")
     cat(" cannot find a reasonable shelf-life for the\n")
     cat(" batch with presented dataset within 84 months.\n\n")

     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
     shelflife<-as.integer(PPY)
     main<-paste(c("estimated shelf-life =",shelflife, "months"),collapse=" ")
     x<-ANCOVAdata$time
     y<-ANCOVAdata$assay
     plot(x,y,xlim=c(0,84),ylim=c((Uper-30),(Uper+30)),main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n", frame.plot=FALSE)
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #plot CI
     newx<-seq(0,84)
     pred<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),level = 0.90,type="response")
     lines(newx,pred[,3],col="red",lty=4,lwd=2)
###
### no legend here; data have been combined all together.
### but... say something here
     leg_txt<-"All Pooled Data from Batches (Model#1)"
     legend("top",leg_txt,xjust=0,yjust=0,box.col="white")

     #add criteria limit
     abline(h=Uper, col = "red")
     abline(v=shelflife, col = "black")
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
##### end of one-sided upper
##### if two-sided, start from here
if (twosided){
cat("                        Two-sided LC analysis                           \n\n")
if (noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
    noSolution = TRUE
}
else {
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf-life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf-life\n\n")
     cat(" Ps. When shelf-life = 84, it means that stab\n")
     cat(" cannot find a reasonable shelf-life for the\n")
     cat(" batch with presented dataset within 84 months.\n\n")

     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
     shelflife<-as.integer(PPY)
     main<-paste(c("estimated shelf-life =",shelflife, "months"),collapse=" ")
     x<-ANCOVAdata$time
     y<-ANCOVAdata$assay
     plot(x,y,xlim=c(0,84),ylim=c((Lper-20),(Uper+20)), main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2,col="black", xaxt="n", frame.plot=FALSE)
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
     mylm<-lm(y~x)
     abline(mylm,lwd=2, col="blue")
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
###
### no legend here; data have been combined all together.
### but... say something here
     leg_txt<-"All Pooled Data from Batches (Model#1)"
     legend("top",leg_txt,xjust=0,yjust=0,box.col="white")

#plot CI
     newx<-seq(0,84)
     pred<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"),level = 0.95,type="response")
     lines(newx,pred[,2],col="blue",lty=4,lwd=2)
     lines(newx,pred[,3],col="blue",lty=4,lwd=2)

     #add criteria limit
     abline(h=Uper, col = "red")
     abline(h=Lper, col = "red")
     abline(v=shelflife, col = "black")
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
     cat("                                                                      \n")
     cat(" Drug product with lower acceptance limit of",Lper,"% of label claim  \n")
     cat("          and with upper acceptance limit of",Uper,"% of label claim  \n")
     cat(" estimated shelf-life =",PPY,"months                                \n\n")
     cat("**********************************************************************\n")
     cat("\n")
   }
}
### end of two-sided.
### do Q-Q plot only if there is at least one solution (shelf-life)
if (!noSolution) {
    output<-data.frame(ANCOVAdata$batch,ANCOVAdata$time,ANCOVAdata$assay,cal,Res)
    colors<-data.frame(colx=seq(1,100))         ### for when the batch# is not coded as 1, 2, 3, ... - YJ
    qqnorm(output$Res, las=1, main = "Normal Q-Q Plot of Residuals", 
           col=c(colors$colx),lwd=3, frame.plot=FALSE) 
      
         LLegend<-paste("batch#",c(Intable$batch))
         temp <- legend("top", legend = LLegend, box.col="white",
               ### text.width = strwidth("10000000"),
               lty=1, col=c(colors$colx), xjust = 1, yjust = 1)
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
      }
    sink()
    close(zz)
    readline(" Done. Press any key to continue...")
    if(noPY){}                       ## since no PY can be found only dev.off().
      else{dev.off();dev.off()}                  ## req. to clode X-windows and pdf output file.
       cat("*****************************************************************************\n\n")
       cat("## Please note: The output files (",output_to_txt,") and (",plots_to_pdf,")     \n")
       cat("   have been created and placed at ",filepath,                               "\n\n")
       cat("*****************************************************************************\n\n")     
    go()  ### back to Top menu now. -YJ
}

##################################  END OF MODEL #1 #############################
#
################################## BEGIN OF MODEL #2 ############################

else {
  if ((Pslope >=0.25) && (Pintercept < 0.25)){
### pool all data to get a common slope with different intercepts; exactly. --YJ

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
#step1: calc. different intercepts and a slope from lm 
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
    Slope<-coef(lm(assay ~ batch+time, data=ANCOVAdata))[d+1]  ### the common slope is here!  - YJ

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
		     Sxx<-0
		     Syy<-0
		     Sxy<-0 
		     SS<-0
		     L<-0
		     LL<-0
		     pd<-0
		     T<-0 
		     delta1<-0
		     delta2<-0
		     
		     PX<-0
		     PY<-0 
		     TT<-0
		     
  for (j in 1:length(W.split)){     # precess batch by batch now... not the same with Model# 1. --YJ
          Intercept<-0
          for(x in 1: length(unique(Intable$batch))){
                if (W.split[[j]][["batch"]][1]==Intable$batch[[x]]){
                  Intercept<- Intable$Intercept[[x]]}                 ### different intercepts is here! use "Intercept" following
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
          #sum of (Xi-Xmean)^2
            Sxx[j]<-var(W.split[[j]][["time"]])*(L[j]-1)   ### will be used to calc 'pd' later --YJ
          #sum of (Yi-Ymean)^2
            Syy[j]<-var(W.split[[j]][["assay"]])*(L[j]-1)
          #sum of (Xi-Xmean)* (Yi-Ymean)
            Sxy[j]<-sum(Sxxyy)
          # unique SS for Model #2
            SS[j]<-(Syy[j]-(Slope*Sxy[j]))/(L[j]-2)        ### will be used to calc 'pd' later --YJ
        
### 95%CI,n-2 T value  ### starting form here -YJ, with batch j= 1,2,3... 
        cat(" --- Batch#:", ba[j],"---")       ### starting output here
        if(onesidedlo || onesidedup){
           T[j]<-qt(CI_percent,L[j]-2)        ### will be used to calc 'pd' later --YJ
              newx<-data.frame(xx=seq(0,84))
              yy<-Intercept+newx$xx*Slope        ### intercept should be by batch; Slope is the same. --YJ
              mod1<-lm(yy~newx$xx)
              pred<-predict(mod1, newdata=newx,interval = c("confidence"),level = 0.90,type="response")
              ### LX<-0      ### not used here.
              LY<-0
              ### for(i in seq_along(W.split)){       ### not used here for LX. -YJ
                 ### LX<-length(W.split[[i]]$time)}   ### is that "LX = L[j]" here?  No!  Use L[j]; as equal to 'n' in the textbook...  -YJ
                 
                 LY<-mean(ANCOVAdata$time )
###
### Statistical Design and Analysis in Pharmaceutical Science. 
### CHow SC, Liu LP. p.362 & 371 --> SE(x) (eq. 11.2.6) & L(x) (parts, not all!);
### looks like 'pd' is = 't(0.05, n-2)*SE(x)' where x = batch #
### --- Batch#: 1 ---
###  show LX, L[j]-->  8 8 
### 
###  Pause here..
###  show LX, L[j]-->  7 8 
### 
###  Pause here..
###  show LX, L[j]-->  8 8 
### 
### conclusion: we should use L[j], not LX here. -YJ (great!) see: drug_stability example textbook, p.617.
###            because it is within a batch, not between the batch.!  Haha~  

           ### cat(" show LX, L[j]--> ",LX,L[j],"\n\n")
           ### readline(" Pause here..")      
           ### pd<-(sqrt(1/(LX) + (((newx$xx-LY)^2)/Sxx[j])*SS[j]))*T[j]}    ### calc 'pd' value here (only for model#2); here we use T[j] finally. Haha... --YJ
           pd<-(sqrt(1/(L[j]) + (((newx$xx-LY)^2)/Sxx[j])*SS[j]))*T[j]   ### this eq. is correct; it is within the batch. --YJ
           
           
           if(onesidedlo){
              Lower<-pred[,1]-pd                                             ### calc 'Lower' =(fit-pd) with pd here
              total<-data.frame(time=newx$xx, Lower=Lower)
              PY[j]<-0
              for(i in 1:length(newx$xx)){
                 if(total$Lower[i]<Lper){PY[j]<-(i-2);break()}}
              if (PY[j]==0||PY[j]<0) {PY[j]<-84}                       ### over ranged; set it to max.
           }
           if(onesidedup){
              Upper<-pred[,1]+pd                                       ### calc 'Upper' =(fit+pd) with pd here
              total<-data.frame(time=newx$xx, Upper=Upper)
              cat("\n\n")
              PY[j]<-0
              for(i in 1:length(newx$xx)){
                 if(total$Upper[i]>Uper){PY[j]<-(i-2);break()}}
              if (PY[j]==0||PY[j]<0) {PY[j]<-84}
                 }
           }
        else {
           #### T[j]<-qt(0.975,L[j]-2)
           T[j]<-qt(1-((1-CI_percent)/2),L[j]-2)  ## two sided from here
           newx<-data.frame(xx=seq(0,84))
           yy<-Intercept+newx$xx*Slope
           mod1<-lm(yy~newx$xx)
           pred<-predict(mod1,newdata=newx,interval = c("confidence"),level = 0.95,type="response")
           LX<-0
           LY<-0
           for(i in seq_along(W.split)){
              LX<-length(W.split[[i]]$time)}
              
              LY<-mean(ANCOVAdata$time )
              
           pd<-(sqrt(1/(L[j]) + (((newx$xx-LY)^2)/Sxx[j])*SS[j]))*T[j]   ### calc 'pd' value here (only for model#2!); here we use T[j] finally. Haha... --YJ
           
           Lower<-pred[,1]-pd                                          ### calc 'Lower' =(fit-pd) with pd here
           Upper<-pred[,1]+pd                                          ### calc 'Upper' =(fit+pd) with pd here
           total<-data.frame(time=newx$xx, Lower=Lower,fit=pred[,1], Upper=Upper) 
           cat("\n\n")
           PY[j]<-0
           PY1[j]<-0
           PY2[j]<-0
           for(i in 1:length(newx$xx)){
              if(total$Lower[i]<Lper){PY1[j]<-(i-2);break()}}
           if (PY1[j]==0||PY1[j]<0) {PY1[j]<-84}
              
           for(i in 1:length(newx$xx)){
              if(total$Upper[i]>Uper){PY2[j]<-(i-2);break()}}
           if (PY2[j]==0||PY2[j]<0) {PY2[j]<-84}
              PY[j]<-min(PY1[j],PY2[j])
            }
       
        LL[j]<-length(W.split[[j]][["time"]])   
        TT[j]<-mean(W.split[[j]][["time"]])
       
###
### delete many lines here  - YJ   
### step4: make decision  ### not required any more here. -YJ
###

#########
#step5: Output
### cat(" --- Batch#:", ba[j],"---\n")   ### has been moved to front... -YJ
cat("\nY =",Intercept,"+(",Slope,") X\n\n")
cat("\n")
output<-data.frame(W.split[[j]][["time"]],W.split[[j]][["assay"]],cal,Res)
colnames(output)<-list(" Time"," Observed assay(%)"," Calculated assay(%)"," Residuals")  
show(output)
cat("\n\n")
### newx<-data.frame(xx=seq(0,84))   ### have been calc. previously.
### yy<-Intercept+newx$xx*Slope      ### original was 'Slope[d]' here; it works. - YJ
### mod1<-lm(yy~newx$xx)
### pred<-predict(mod1, newdata=newx,interval = c("confidence"),level = 0.90,type="response")
if (onesidedlo){
total<-data.frame(time=newx$xx, fit=pred[,1], Lower=Lower, star="",stringsAsFactors=F)  ### here we have to use 'Lower=Lower' adjusted with 'pd' -YJ
for(i in 1:(length(newx$xx)-1)){if(i>PY[j]) total[i+1,]$star="***"}}
if (onesidedup){
total<-data.frame(time=newx$xx, fit=pred[,1], Upper=Upper, star="",stringsAsFactors=F)  ### here we have to use 'Upper=Upper' adjusted with 'pd' -YJ
for(i in 1:(length(newx$xx)-1)){if(i>PY[j]) total[i+1,]$star="***"}}
if (twosided){
total<-data.frame(time=newx$xx, Lower=Lower, fit=pred[,1], Upper=Upper, star="",stringsAsFactors=F)  ### as above stated. -YJ
for(i in 1:(length(newx$xx)-1)){if(i>PY[j]) total[i+1,]$star="***"}}
cat("\n\n")
cat("-- List of",CI_percent*100,"% CI for 84-month Time Interval:-\n\n")
show(total)
cat("\n\n ***: means the listing of expiration as defined.")
cat("\n\n")
TIME[[j]]<-c(W.split[[j]][["time"]])
CAL[[j]]<-c(cal)
RRES[[j]]<-c(Res)
OBS[[j]]<-c(W.split[[j]][["assay"]])
}                                     ############# end of batch [j] processing here.  --YJ #################

AA<-melt(CAL)
ZZ<-melt(TIME)
QQ<-melt(RRES)
OO<-melt(OBS)
c(AA$L1)
#purpose: to plot one slope and three intercepts 
NewPred<-data.frame(batch=c(AA$L1),time=ZZ$value, PredCal=AA$value, RES=QQ$value)   
M.split<-split(NewPred, list(NewPred$batch) ) 

### choose min PX or PY  ### need to be fixed now. -YJ
### we already got real shelf-life which is PY[j] before these
### simple and clear!!

PPY<-min(PY)
if ((PPY==84)) noPY = TRUE
                                       
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
     colnames(allSF)<-c(" batch#", "  shelf-life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf-life\n\n\n")
     cat(" Ps. When shelf-life = 84, it means that stab\n")
     cat(" cannot find a reasonable shelf-life for the\n")
     cat(" batch with presented dataset within 84 months.\n\n")
     
     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
     shelflife<-as.integer(PPY)
     main<-paste(c("estimated shelf-life =",shelflife, "months"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata, xlim=c(0,84),ylim=c((Lper-10),(max(assay))), main=main,
     xlab = "Time (months)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n", frame.plot=FALSE)
     axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
     axis(1,at=0:100,tcl=-.2, labels=FALSE)

     colors<-data.frame(colx=seq(1,100))         ### for when the batch# is not coded as 1, 2, 3, ...; it works great. - YJ
     LLegend<-paste("batch# ",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend, box.col="white",
               text.width = strwidth("10000000"),
               lty=1, col=c(colors$colx), xjust = 1, yjust = 1)
               
     for(i in seq_along(M.split)){         ### pooled dataset (M.split)
         x<-M.split[[i]][["time"]] 
         y<-M.split[[i]][["PredCal"]]
         mod<-lm(y~x) 
         abline(mod,lwd=2, col=i)
      } 
      
### plot individual data points & 95% CI - YJ

      ### newx<-data.frame(xx=seq(0,84))
      for(i in seq_along(W.split)){          ### original codes use 'W.split' -YJ
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)
      } 
      
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
      newx<-data.frame(xx=seq(0,84))
      yy<-Intercept1+newx$xx*Slope
      mod1<-lm(yy~newx$xx)
      pred<-predict(mod1,newdata=newx,interval = c("confidence"),level = 0.90,type="response")
      pd<-(sqrt(1/(LX) + (((newx$xx-LY)^2)/Sxx[i])*SS[i]))*T[i]       ### use LX here, not L[i] because between batch. -YJ
      Lower<-pred[,1]-pd
      total<-data.frame(time=newx$xx, fit=pred[,1], Lower=Lower)
      lines(total$time, total$Lower,col=i,lty=4,lwd=2)
   }
      axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
      #add criteria limit
      abline(h=Lper, col = "red")
      abline(v=shelflife, col = "black")
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
      cat(" -----------------------------------------------------------------\n\n")
      cat(" Drug product with lower acceptance limit of",Lper,"% of label claimed\n")
      cat(" shelf-life =",shelflife,"(months)                          \n\n")
      cat("******************************************************************\n")
      cat("\n")
   }
}
##### end of one-sided low
##### if one-sided upper, start from here
if (onesidedup ){
cat("\n")
cat("                     One-sided upper LC analysis                        \n\n")
if (noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
    noSolution = TRUE
}
else { 
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf-life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf-life\n\n")
     cat(" Ps. When shelf-life = 84, it means that stab\n")
     cat(" cannot find a reasonable shelf-life for the\n")
     cat(" batch with presented dataset within 84 months.\n\n")
     
     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
     shelflife<-as.integer(PPY)
     main<-paste(c("estimated shelf-life =",shelflife, "months"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata,xlim=c(0,84),ylim=c((Uper-20),(Uper+20)), main=main,   ### why? because it is upper!!  -YJ
     xlab = "Time (months)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n",frame.plot=FALSE) 
     axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
     axis(1,at=0:100,tcl=-.2, labels=FALSE)   
      
     colors<-data.frame(colx=seq(1,100))         ### for when the batch# is not coded as 1, 2, 3, ...; it works great. - YJ
     LLegend<-paste("batch# ",c(Intable$batch))   
     temp <- legend("top", legend = LLegend, box.col="white",
               text.width = strwidth("10000000"),
               lty=1, col=c(colors$colx), xjust = 1, yjust = 1)

     for(i in seq_along(M.split)){         ### pooled dataset
         x<-M.split[[i]][["time"]] 
         y<-M.split[[i]][["PredCal"]]
         mod<-lm(y~x) 
         abline(mod,lwd=2, col=i)   
      }

### plot individual data points & 95% CI - YJ

      ### newx<-data.frame(xx=seq(0,84))
      for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]]    #### for plots of indiv. data points only. --YJ
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)
      }
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
      newx<-data.frame(xx=seq(0,84))
      yy<-Intercept1+newx$xx*Slope
      mod1<-lm(yy~newx$xx)
      pred<-predict(mod1,newdata=newx,interval = c("confidence"),level = 0.90,type="response")
      pd<-(sqrt(1/(LX) + (((newx$xx-LY)^2)/Sxx[i])*SS[i]))*T[i]     ### 'pd' in only for model #2!  -YJ
      Upper<-pred[,1]+pd
      total<-data.frame(time=newx$xx, fit=pred[,1], Upper=Upper)
      lines(total$time,total$Upper,col=i,lty=4,lwd=2)
   }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     ### add criteria limit line
     abline(h=Uper, col = "red")
     abline(v=shelflife, col = "black")
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
     cat(" -----------------------------------------------------------------\n\n")
     cat(" Drug product with upper acceptance limit of",Uper,"% of label claim\n")
     cat(" shelf-life =",shelflife,"(months)                        \n\n")
     cat("******************************************************************\n")
     cat("\n")
   }
}
##### end of one-sided upper
##### if two-sided, start from here
if (twosided){
cat("                        Two-sided LC analysis                           \n\n")
if (noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
   noSolution = TRUE
}
else {
     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf-life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf-life\n\n")
     cat(" Ps. When shelf-life = 84, it means that stab\n")
     cat(" cannot find a reasonable shelf-life for the\n")
     cat(" batch with presented dataset within 84 months.\n\n")
     }

### go to plot of single batch
     shelflife<-as.integer(PPY)
     main<-paste(c("estimated shelf-life=",shelflife, " months"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata, xlim=c(0,84),ylim=c((Lper-20),(Uper+20)), main=main,
     xlab = "Time (months)" , ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n", frame.plot=FALSE)
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE)
   
     colors<-data.frame(colx=seq(1,100))         ### for when the batch# is not coded as 1, 2, 3, ...; it works great. - YJ
     LLegend<-paste("batch# ",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend, box.col="white",
               text.width = strwidth("10000000"),
               lty=1, col=c(colors$colx), xjust = 1, yjust = 1)
   
    for(i in seq_along(M.split)){
     x<-M.split[[i]][["time"]] 
     y<-M.split[[i]][["PredCal"]]
     points(x,y,pch=16, col=i)
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
       newx<-data.frame(xx=seq(0,84))
       yy<-Intercept1+newx$xx*Slope
       mod1<-lm(yy~newx$xx)
       pred<-predict(mod1,newdata=newx,interval = c("confidence"),level = 0.90,type="response")
       pd<-(sqrt(1/(LX) + (((newx$xx-LY)^2)/Sxx[i])*SS[i]))*T[i]
       Lower<-pred[,1]-pd
       Upper<-pred[,1]+pd
       total<-data.frame(time=newx$xx, fit=pred[,1], Upper=Upper, Lower=Lower)
       lines(total$time,total$Lower,,col=i,lty=4,lwd=2)
       lines(total$time,total$Upper,,col=i,lty=4,lwd=2)
   }
       axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
       ### add criteria limit line
       abline(h=Uper, col = "red")
       abline(h=Lper, col = "red")
       abline(v=PPY,  col = "black")
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
       cat("                                                                      \n")
       cat(" Drug product with lower acceptance limit of",Lper,"% of label claim  \n")
       cat("          and with upper acceptance limit of",Uper,"% of label claim  \n")
       cat(" estimated shelf-life =",PPY,"months                                \n\n")
       cat("**********************************************************************\n")
       cat("\n")
 }
### end of two-sided.
### do Q-Q plot only if there is at least one solution (shelf life)
if (!noSolution) {
    colors<-data.frame(colx=seq(1,100))         ### for when the batch# is not coded as 1, 2, 3, ... - YJ
    qqnorm(QQ$value, las=1, main = "Normal Q-Q Plot of Residuals", lwd=3, col=c(colors$colx), frame.plot=FALSE)  
      
    LLegend<-paste("batch# ",c(Intable$batch)) 
    temp <- legend("top", legend = LLegend, box.col="white",
            ### text.width = strwidth("10000000"),
            lty=1, col=c(colors$colx), xjust = 1, yjust = 1)
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
     }
   sink()
   close(zz)
   readline(" Done. Press any key to continue...")
   if(noPY){}                          ## since no PY can be found only dev.off().
      else{dev.off();dev.off()}        ## req. to clode X-windows and pdf output file.
       cat("*****************************************************************************\n\n")
       cat("## Please note: The output files (",output_to_txt,") and (",plots_to_pdf,")     \n")
       cat("   have been created and placed at ",filepath,                               "\n\n")
       cat("*****************************************************************************\n\n")
   go()
}
############################# END OF MODEL #2 ##################################
#
###############################   Model #3   ###################################
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

     T<-0 
     delta1<-0
     delta2<-0
     PX<-0
     PY<-0
     
     for (j in 1:length(W.split)){
          Intercept<-0
          Slope<-0
          for(x in 1: length(unique(Intable$batch))){
                if (W.split[[j]][["batch"]][1]==Intable$batch[[x]]){
                  Intercept<- Intable$Intercept[[x]]                  ### ok, here is intercept now! use "Intercept" for batch j
                 }
                if (W.split[[j]][["batch"]][1]==Intable$batch[[x]]){
                  Slope<- Intable$Slope[[x]]                          ### ok, here is slope now! use "Slope" for batch j
                } 
          } 
          cal<-0
          Res<-0
          
          for (a in 1:length(W.split[[j]][["time"]])){    
#Calculated assay
               cal[a]<- (((W.split[[j]][["time"]][a])*Slope)+ Intercept) 
#extract model residuals
               Res[a]<- cal[a]-(W.split[[j]][["assay"]][a])
               }

### number of data points
        L[j]<-length(W.split[[j]][["time"]])  
### 95%CI,n-2 T value
###       
### prepare to construct 95%CI here -YJ 
###
        cat(" --- Batch#:", ba[j],"---")    ### starting output here
        if(onesidedlo || onesidedup){
          T[j]<-qt(CI_percent,L[j]-2)                        ### this line should be allowed to delete.  -YJ
              newx<-data.frame(xx=seq(0,84))
              xx<-W.split[[j]][["time"]]               ### here we have to get intercept & slope again from indiv. batch.
              yy<-W.split[[j]][["assay"]]              ### not the same as Model # & #2; different!!! --YJ
              mod1<-lm(yy~xx)                          ### not with 'newx$xx'!
              pred<-predict(mod1, newdata=newx,interval = c("confidence"),level = 0.90,type="response")   ### here still using 'newdata=newx'; get err if 'newdata=newx$$xx' -YJ
          if(onesidedlo){
              total<-data.frame(time=newx$xx, fit=pred[,1], Lower=pred[,2])   ### here 'newdata=newx$xx'; get err if 'newdata=newx' -YJ
              cat("\n\n")
              PY[j]<-0
              for(i in 1:length(newx$xx)){
                 if(total$Lower[i]<Lper){PY[j]<-(i-2);break()}}
              if (PY[j]==0||PY[j]<0) {PY[j]<-84}
                        }
           if(onesidedup){
              total<-data.frame(time=newx$xx, fit=pred[,1], Upper=pred[,3])
              cat("\n\n")
              PY[j]<-0
              for(i in 1:length(newx)){
                 if(total$Upper[i]>Uper){PY[j]<-(i-2);break()}}
              if (PY[j]==0||PY[j]<0) {PY[j]<-84}
                         }
               }
        else {
           T[j]<-qt(1-((1-CI_percent)/2),L[j]-2) ### this line should be allowed to delete.  -YJ
           newx<-data.frame(xx=seq(0,84))
           xx<-W.split[[j]][["time"]]               ### here we have to get intercept & slope again from indiv. batch.
           yy<-W.split[[j]][["assay"]]              ### not the same as Model # & #2; different!!! --YJ
           mod1<-lm(yy~xx)                          ### not with 'newx$xx'!
           pred<-predict(mod1,newdata=newx,interval = c("confidence"),level = 0.95,type="response")   ### here still using 'newdata=newx'; get err if 'newdata=newx$$xx' -YJ
           total<-data.frame(time=newx$xx, Lower=pred[,2],fit=pred[,1], Upper=pred[,3]) 
           cat("\n\n")
           PY[j]<-0
           PY1[j]<-0
           PY2[j]<-0
           for(i in 1:length(newx$xx)){
              if(total$Lower[i]<Lper){PY1[j]<-(i-2);break()}}
           if (PY1[j]==0||PY1[j]<0) {PY1[j]<-84}

           for(i in 1:length(newx$xx)){
              if(total$Upper[i]>Uper){PY2[j]<-(i-2);break()}}
           if (PY2[j]==0||PY2[j]<0) {PY2[j]<-84}
           PY[j]<-min(PY1[j],PY2[j])     ### pick a smallest one here - YJ
           } 
#step3: calculate possible X1,X2,Y1,Y2   ### not required any more now... -YJ
#step4: make decision                    ### not required any more now... -YJ
#########
#step5: Output
### cat(" --- Batch#:", ba[j],"---\n")   ### moved to the front
cat("\nY =",Intercept,"+(",Slope,") X\n\n")
cat("\n")
output<-data.frame(W.split[[j]][["time"]],W.split[[j]][["assay"]],cal,Res)
colnames(output)<-list(" Time"," Observed assay(%)"," Calculated assay(%)"," Residuals")  
show(output)
###
if (onesidedlo){
total<-data.frame(time=newx$xx, fit=pred[,1], Lower=pred[,2], star="",stringsAsFactors=F)  ### here we have to use 'Lower=pred[,2]' -YJ
for(i in 1:(length(newx$xx)-1)){if(i>PY[j]) total[i+1,]$star="***"}}
if (onesidedup){
total<-data.frame(time=newx$xx, fit=pred[,1], Upper=pred[,3], star="",stringsAsFactors=F)  ### here we have to use 'Upper=pred[,3]' -YJ
for(i in 1:(length(newx$xx)-1)){if(i>PY[j]) total[i+1,]$star="***"}}
if (twosided){
total<-data.frame(time=newx$xx, Lower=pred[,2], fit=pred[,1], Upper=pred[,3], star="",stringsAsFactors=F)  ### as above stated. -YJ
for(i in 1:(length(newx$xx)-1)){if(i>PY[j]) total[i+1,]$star="***"}}
cat("\n\n")
cat("-- List of",CI_percent*100,"% CI for 84-month Time Interval:-\n\n")
show(total)
cat("\n\n ***: means the listing of expiration as defined.")
cat("\n\n")
###
TIME[[j]]<-c(W.split[[j]][["time"]])
CAL[[j]]<-c(cal)
RRES[[j]]<-c(Res)
OBS[[j]]<-c(W.split[[j]][["assay"]])
cat("\n\n")  
}   ################################ end of batch processing (batch=j) here...  -YJ

AA<-melt(CAL)
ZZ<-melt(TIME)
QQ<-melt(RRES)
OO<-melt(OBS)

#choose min PX or PY

PPY<-min(PY)
if ((PPY==84)) noPY = TRUE

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
     colnames(allSF)<-c(" batch#", "  shelf-life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf-life\n\n")
     cat(" Ps. When shelf-life = 84, it means that stab\n")
     cat(" cannot find a reasonable shelf-life for the\n")
     cat(" batch with presented dataset within 84 months.\n\n")

     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
### go to plot of single batch
     shelflife<-as.integer(PPY)
     main<-paste(c("estimated shelf-life =",shelflife,"months"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata, xlim=c(0,84),ylim=c((Lper-10),(Lper+30)), main=main,
     xlab = "Time (months)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n", frame.plot=FALSE)
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE) 
    
     colors<-data.frame(colx=seq(1,100))         ### for when the batch# is not coded as 1, 2, 3, ...; it works great. - YJ
     LLegend<-paste("batch# ",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend, box.col="white",
               text.width = strwidth("10000000"),
               lty=1, col=c(colors$colx), xjust = 1, yjust = 1)
     
#plot CI
      newx<-data.frame(xx=seq(0,84)) 
      for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)    ### plot data points for each batch here -YJ
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)       ### plot each regr. line for each batch -YJ
      pred<-predict(mod, newdata=newx,interval = c("confidence"),level = 0.90,type="response")
      lines(newx$xx,pred[,2],,col=i,lty=4,lwd=2)   ### plot lower 95% CI line -YJ
         }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #add criteria limit
     abline(h=Lper, col = "red")
     abline(v=shelflife, col = "black")
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
     cat("------------------------------------------------------------------    \n")
     cat("                                                                      \n")
     cat(" Drug product with lower acceptance limit of",Lper,"% of label claim  \n")
     cat(" shelf-life =",shelflife,"(months)                            \n\n")
     cat("******************************************************************    \n")
     cat("\n")
   }
}
##### end of one-sided low
##### if one-sided upper, start from here
if (onesidedup ){
cat("\n")
cat("                     One-sided upper LC analysis                        \n\n")
if (noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
    noSolution = TRUE
}
else { 
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf-life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf-life\n\n")
     cat(" Ps. When shelf-life = 84, it means that stab\n")
     cat(" cannot find a reasonable shelf-life for the\n")
     cat(" batch with presented dataset within 84 months.\n\n")

     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
     shelflife<-as.integer(PPY)
     main<-paste(c("estimated shelf-life =",shelflife, "months"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata,xlim=c(0,84),ylim=c((Uper-30),(Uper+10)), main=main,
     xlab = "Time (months)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n", frame.plot=FALSE)
     axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
     axis(1,at=0:100,tcl=-.2, labels=FALSE)   
      
     colors<-data.frame(colx=seq(1,100))         ### for when the batch# is not coded as 1, 2, 3, ...; it works great. - YJ
     LLegend<-paste("batch# ",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend, box.col="white",
               text.width = strwidth("10000000"),
               lty=1, col=c(colors$colx), xjust = 1, yjust = 1)
               
#plot CI
      newx<-data.frame(xx=seq(0,84))  
      for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)    ### plot data points for each batch here -YJ
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)       ### plot each regr. line for each batch -YJ
      pred<-predict(mod,newdata=newx,interval = c("confidence"),level = 0.90,type="response")
      lines(newx$xx,pred[,3],,col=i,lty=4,lwd=2)    ### plot upper 95% CI line -YJ
         }
      axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
      #add criteria limit
      abline(h=Uper, col = "red")
      abline(v=shelflife, col = "black")
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
     cat(" shelf-life =",shelflife,"(months)                                \n\n")
     cat("******************************************************************  \n")
     cat("\n")
    } 
}
##### end of one-sided upper
##### if two-sided, start from here
if (twosided){
cat("                        Two-sided LC analysis                           \n\n")
if (noPY){
cat("       No solution can be found with this model. Please try others.       \n")
cat("**************************************************************************\n")
   noSolution = TRUE
}
else {
     allSF<-data.frame(ba,PY)
     colnames(allSF)<-c(" batch#", "  shelf-life*")
     show(allSF)
     cat("-------------------------\n")
     cat("*: estimated shelf-life\n\n")
     cat(" Ps. When shelf-life = 84, it means that stab\n")
     cat(" cannot find a reasonable shelf-life for the\n")
     cat(" batch with presented dataset within 84 months.\n\n")

     ### windows(record = TRUE )  ### NOT working in linux/unix any more; change to dev.new()
     dev.new()
### go to plot of single batch
### outline plot box 
     shelflife<-as.integer(PPY)
     main<-paste(c("estimated shelf-life =",shelflife,"months"),collapse=" ")    
     plot(time~assay,data=ANCOVAdata, xlim=c(0,84),ylim=c((Lper-10),(Uper+10)), main=main,
     xlab = "Time (months)", ylab = "Assay (%)", pch = 16, cex.lab = 1.5,
     lab=c(20,10,30),lty=2,lwd=2, xaxt="n", frame.plot=FALSE)
       axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
       axis(1,at=0:100,tcl=-.2, labels=FALSE) 
    
     colors<-data.frame(colx=seq(1,100))         ### for when the batch# is not coded as 1, 2, 3, ...; it works great. - YJ
     LLegend<-paste("batch# ",c(Intable$batch))   
     temp <- legend("topright", legend = LLegend, box.col="white",
               text.width = strwidth("10000000"),
               lty=1, col=c(colors$colx), xjust = 1, yjust = 1)
     
#plot CI
     newx<-data.frame(xx=seq(0,84)) 
     for(i in seq_along(W.split)){
      xx<-W.split[[i]][["time"]] 
      yy<-W.split[[i]][["assay"]]
      points(xx,yy,pch=16, col=i)    ### plot data points for each batch here -YJ
      mod<-lm(yy~xx) 
      abline(mod,lwd=2, col=i)       ### plot each regr. line for each batch -YJ
      pred<-predict(mod, newdata=newx,interval = c("confidence"),level = 0.95,type="response")
      lines(newx$xx,pred[,2],,col=i,lty=4,lwd=2)    ### plot lower/upper 95% CI line -YJ
      lines(newx$xx,pred[,3],,col=i,lty=4,lwd=2)
      }
     axis(1,tcl=-.5, tick=TRUE,labels=FALSE)
     #add criteria limit
     abline(h=Uper, col = "red")
     abline(h=Lper, col = "red")
     abline(v=shelflife, col = "black")
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
     cat("                                                                      \n")
     cat(" Drug product with lower acceptance limit of",Lper,"% of label claim  \n")
     cat("          and with upper acceptance limit of",Uper,"% of label claim  \n")
     cat(" estimated shelf-life =",PPY,"months                                \n\n")
     cat("**********************************************************************\n")
     cat("\n")
   }
 }
### end of two-sided.
### do Q-Q plot only if there is at least one solution (shelf life)
if (!noSolution) {
    colors<-data.frame(colx=seq(1,100))         ### for when the batch# is not coded as 1, 2, 3, ... - YJ
    qqnorm(QQ$value, las=1, main = "Normal Q-Q Plot of Residuals", lwd=3, col=c(QQ$L1),frame.plot=FALSE)
      
    LLegend<-paste("batch#",c(Intable$batch)) 
    temp <- legend("top", legend = LLegend, box.col="white",
            ### text.width = strwidth("10000000"),
            lty=1,col=c(colors$colx), xjust = 1, yjust = 1)
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
     }
   sink()
   close(zz)
   readline(" Done. Press any key to continue...")
   if(noPY){}                        ## since no PY can be found only dev.off().
      else{dev.off();dev.off()}                  ## req. to clode X-windows and pdf output file.
       cat("*****************************************************************************\n\n")
       cat("## Please note: The output files (",output_to_txt,") and (",plots_to_pdf,")    \n")
       cat("   have been created and placed at ",filepath,                              "\n\n")
       cat("*****************************************************************************\n\n")
   go()
     }
   }
 }
}

### end of MutilpleAnalyze