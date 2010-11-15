#Input assay data Menu for Data Analysis for a Single Batch 
SingleBatchmenu<-function(Singledata)
{
cat("\n")
cat("****************************************************************************\n")
cat("*            Step2-1: Data Analysis for a Single Batch                     *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("*                                                                          *\n")
cat("* Linear regression and statistical modeling can be used in the analysis   *\n")
cat("* of stability data that are amenable to statistical analysis for a        *\n")
cat("* quantitative attribute for which there is a proposed acceptance criterion*\n")
cat("*                                                                          *\n")
cat("****************************************************************************\n")
cat("\n")
file.menu <- c("Input/Edit Assay Data from keyboard",
               "Load Data Files (.csv)",
               "Load Data Files (.RData)",   
               "Go Back to Statistical Analysis menu",
               "Quit")
cat("\n")
pick <- menu(file.menu, title = " << Data Analysis for a Single Batch menu >> ")
if (pick == 1){
     cat("\n")
     Singledata<-data.frame (time=c(0), assay=c(0))
     colnames(Singledata)<-list("time","assay(%)")
     Singledata<-edit(Singledata)
     Singledata<- na.omit(Singledata)	
     show(Singledata)
     cat("\nSave data (y/n) ?\n")
     ans<-readline()
     cat("\n")
     if (ans == "n" | ans == "N")
        {
        return(statistical())
        }
     else {
        cat("Enter name you want to call this data\n")
        Singlename <-readline() 
        Singlename<-paste(Singlename,".RData",sep="")      
           if(file.exists(Singlename)){
           cat("\n")
           cat("******************************************\n")
           cat("* The file name have been existed.       *\n")
           cat("* Would you want to overwrite it ? (y/n) *\n")
           cat("******************************************\n")
           ans<-readline()
             if (ans == "y" | ans == "Y")
                {
                save(Singledata,file=Singlename)
                cat("\n")
                }
                else{
                cat("\nEnter name you want to call this data\n")
                Singlename <-readline() 
                Singlename<-paste(Singlename,".RData",sep="") 
                repeat{
                    if(file.exists(Singlename))
                      {
                      cat("\n")
                      cat("***********************************\n")
                      cat("* The file name have been existed *\n")
                      cat("* Enter name again, OK.           *\n")
                      cat("***********************************\n")
                      Singlename<-readline()
                      Singlename<-paste(Singlename,".RData",sep="") 
                      }
                       else{
                       break                       
                           }
                    }        
             }   
              save(Singledata,file=Singlename)   
           }
        else{
           save(Singledata,file=Singlename)
          }                            
cat("\n\n")
cat("****************************************************************************\n")
cat("*                         Now, Go to analyze the data                      *\n")
cat("****************************************************************************\n\n")   
        return(SingleAnalyze(Singledata))
      }      
    }
      
else {   
  if (pick == 2){
  
    filepath<-getwd()
    cat("R will import your data from the directory of \n")
    cat("",filepath,"\n")
        return(SingleBatchcsv())     
      }      

else {
  if (pick == 3){
    
    cat("\n\n")
    filepath<-getwd()
    cat("R will load your data from the directory of \n")
    cat("",filepath,"\n")
    cat("\n")     
    
     cat("\nEnter data file name\n") 
     Singlename <-readline()
     Singlename<-paste(Singlename,".RData",sep="")
     load(Singlename)
     Singledata<-edit(Singledata)
     Singledata<- na.omit(Singledata)
     colnames(Singledata)<-list("time","assay (%)")
     cat("\n\n")
     show(Singledata)
     save(Singledata,file=Singlename)
cat("\n\n")
cat("****************************************************************************\n")
cat("*                         Now, Go to analyze the data                      *\n")
cat("****************************************************************************\n\n")   
        return(SingleAnalyze(Singledata))
      }      
 
  else {   
  if (pick == 4){
     cat("\n\n")
     statistical()
                }
  else {
  if (pick == 5){
      cat("\nThanks for using stab for R. Bye now.\n\n")
                }  
     } 
    }
   }
  }
}      