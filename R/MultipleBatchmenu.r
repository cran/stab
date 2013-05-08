#Input assay data Menu for Data Analysis for Multiple Batches
MultipleBatchmenu<-function(Multipledata)
{
cat("\n")
cat("****************************************************************************\n")
cat("*            Step2-2: Data Analysis for Multiple Batches                   *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("*                                                                          *\n")
cat("* Linear regression, poolability tests and statistical modeling can be used*\n")
cat("* in the analysis of stability data that are amenable to statistical       *\n")
cat("* analysis for a quantitative attribute for which there is a proposed      *\n")
cat("* acceptance criterion.                                                    *\n")
cat("*                                                                          *\n")
cat("****************************************************************************\n")
cat("\n")
file.menu <- c("Input/Edit Assay Data from keyboard",
               "Load Data Files (.csv)",
               "Load Data Files (.RData)",
               "Go Back to Statistical Analysis menu",
               "Quit")
cat("\n")
pick <- menu(file.menu, title = " << Data Analysis for Multiple Batches menu >> ")
if (pick == 1){
     cat("\n")
     Multipledata<-data.frame (batch=c(0), time=c(0), assay=c(0))
     colnames(Multipledata)<-list("batch","time","assay")
     Multipledata<-edit(Multipledata)
     Multipledata<- na.omit(Multipledata)
     show(Multipledata)
     cat("\nSave data (y/n) ?\n")
     ans<-readline()
     cat("\n")
     if (ans == "n" | ans == "N")
        {
        return(statistical())
        }
     else {
        cat("\nEnter the file name to save data (no file extension!): \n")
        Multiplename <-readline()
        Multiplename<-paste(Multiplename,".RData",sep="")
           if(file.exists(Multiplename)){
           cat("\n")
           cat("******************************************\n")
           cat("* The file name have been existed.       *\n")
           cat("* Would you want to overwrite it ? (y/n) *\n")
           cat("******************************************\n")
           ans<-readline()
             if (ans == "y" | ans == "Y")
                {
                save(Multipledata,file=Multiplename)
                cat("\n")
                }
                else{
                cat("\nEnter the file name to save data (no file extension!): \n")
                Multiplename <-readline()
                Multiplename<-paste(Multiplename,".RData",sep="")
                repeat{
                    if(file.exists(Multiplename))
                      {
                      cat("\n")
                      cat("***********************************\n")
                      cat("* The file name have been existed. *\n")
                      cat("* Please try again.                *\n")
                      cat("***********************************\n")
                      Multiplename<-readline()
                      Multiplename<-paste(Multiplename,".RData",sep="")
                      }
                       else{
                       break
                           }
                    }
             }
              saveRDS(Multipledata,file=Multiplename)
           }
        else{
           saveRDS(Multipledata,file=Multiplename)
          }
cat("\n\n")
        return(MultipleAnalyze(Multipledata))
      }
    }

else {
  if (pick == 2){
  
    filepath<-getwd()
    cat("R will import your data from the directory of \n")
    cat("",filepath,"\n")
    return(MultipleBatchcsv())     
      }

else {
  if (pick == 3){
    
    cat("\n\n")
    filepath<-getwd()
    cat("R will load your data (.RData) from the directory of \n")
    cat("",filepath,"\n")
    cat("\n")
    readline(" Press Enter to continue...")
     
     ### cat("\nEnter data file name (no extension): \n")
     ### Multiplename <-readline()
     ### Multiplename<-paste(Multiplename,".RData",sep="")
     Multipledata<-readRDS(file.choose())                        
     ### load(Multiplename)
     Multipledata<-edit(Multipledata )
     Multipledata<- na.omit(Multipledata)
     colnames(Multipledata)<-list("batch","time","assay")
     cat("\n\n")
     show(Multipledata)
     
cat("\n\n")
        return(MultipleAnalyze(Multipledata))
      }

  else {
  if (pick == 4){
     cat("\n\n")
     statistical()
                }
  else {
  if (pick == 5){
      cat("\n  Thank you for using stab for R. Bye~~ \n\n")
      graphics.off()
                }
     }
    }
   }
  }
}