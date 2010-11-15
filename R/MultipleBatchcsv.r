#choose separator and decimal type
MultipleBatchcsv<-function()
{
cat("\n")
file.menu <- c("sep = comma (,) &  dec= point (.)",
               "sep = semicolon (;) &  dec= comma (,)",
               "sep = semicolon (;) &  dec= point (.)",
               "sep = {space} &  dec= comma (,)",
               "sep = {space} &  dec= point (.)",
               "sep = {tab} &  dec= comma (,)",
               "sep = {tab} &  dec= point (.)",
               "sep = colon (:) &  dec= comma (,)",
               "sep = colon (:) &  dec= point (.)",
               "Go Back to Multiple Batch menu")
cat("\n")
pick <- menu(file.menu, title = " << Separator type and Decimal type >> ")
if (pick == 1){
  cat("\n\n")
        cat("\nEnter Data file name (without extention): \n")
        Multiple.file <-readline()
        Multiple.file<-paste(Multiple.file,".csv",sep="")
        cnames<-c("batch","time","assay (%)")
        Multipledata<-read.csv(Multiple.file,header=TRUE,row.names=NULL,col.names=cnames, sep=",",dec=".")
        Multipledata<-edit(Multipledata)
        Multipledata<- na.omit(Multipledata)
        cat("\n\n")
        show(Multipledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(MultipleAnalyze(Multipledata))
     }

 else {
  if (pick == 2){
  cat("\n\n")
        cat("\nEnter Data file name (without extention): \n")
        Multiple.file <-readline()
        Multiple.file<-paste(Multiple.file,".csv",sep="")
        cnames<-c("batch","time","assay (%)")
        Multipledata<-read.csv(Multiple.file,header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=",")
        Multipledata<-edit(Multipledata)
        Multipledata<- na.omit(Multipledata)
        cat("\n\n")
        show(Multipledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(MultipleAnalyze(Multipledata))
          }
 else {
  if (pick == 3){
  cat("\n\n")
        cat("\nEnter Data file name (without extention): \n")
        Multiple.file <-readline()
        Multiple.file<-paste(Multiple.file,".csv",sep="")
        cnames<-c("batch","time","assay (%)")
        Multipledata<-read.csv(Multiple.file,header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=".")
        Multipledata<-edit(Multipledata)
        Multipledata<- na.omit(Multipledata)
        cat("\n\n")
        show(Multipledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(MultipleAnalyze(Multipledata))
          }
else {
  if (pick == 4){
  cat("\n\n")
        cat("\nEnter Data file name (without extention): \n")
        Multiple.file <-readline()
        Multiple.file<-paste(Multiple.file,".csv",sep="")
        cnames<-c("batch","time","assay (%)")
        Multipledata<-read.csv(Multiple.file,header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=",")
        Multipledata<-edit(Multipledata)
        Multipledata<- na.omit(Multipledata)
        cat("\n\n")
        show(Multipledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(MultipleAnalyze(Multipledata))
          }
else {
  if (pick == 5){
  cat("\n\n")
        cat("\nEnter Data file name (without extention): \n")
        Multiple.file <-readline()
        Multiple.file<-paste(Multiple.file,".csv",sep="")
        cnames<-c("batch","time","assay (%)")
        Multipledata<-read.csv(Multiple.file,header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=".")
        Multipledata<-edit(Multipledata)
        Multipledata<- na.omit(Multipledata)
        cat("\n\n")
        show(Multipledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(MultipleAnalyze(Multipledata))

          }
else {
  if (pick == 6){
  cat("\n\n")
        cat("\nEnter Data file name (without extention): \n")
        Multiple.file <-readline()
        Multiple.file<-paste(Multiple.file,".csv",sep="")
        cnames<-c("batch","time","assay (%)")
        Multipledata<-read.csv(Multiple.file,header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=",")
        Multipledata<-edit(Multipledata)
        Multipledata<- na.omit(Multipledata)
        cat("\n\n")
        show(Multipledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(MultipleAnalyze(Multipledata))
          }
else {
  if (pick == 7){
  cat("\n\n")
        cat("\nEnter Data file name (without extention): \n")
        Multiple.file <-readline()
        Multiple.file<-paste(Multiple.file,".csv",sep="")
        cnames<-c("batch","time","assay (%)")
        Multipledata<-read.csv(Multiple.file,header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=".")
        Multipledata<-edit(Multipledata)
        Multipledata<- na.omit(Multipledata)
        cat("\n\n")
        show(Multipledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(MultipleAnalyze(Multipledata))
          }
else {
  if (pick == 8){
  cat("\n\n")
        cat("\nEnter Data file name (without extention): \n")
        Multiple.file <-readline()
        Multiple.file<-paste(Multiple.file,".csv",sep="")
        cnames<-c("batch","time","assay (%)")
        Multipledata<-read.csv(Multiple.file,header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=",")
        Multipledata<-edit(Multipledata)
        Multipledata<- na.omit(Multipledata)
        cat("\n\n")
        show(Multipledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(MultipleAnalyze(Multipledata))
          }
else {
  if (pick == 9){
  cat("\n\n")
        cat("\nEnter Data file name (without extention): \n")
        Multiple.file <-readline()
        Multiple.file<-paste(Multiple.file,".csv",sep="")
        cnames<-c("batch","time","assay (%)")
        Multipledata<-read.csv(Multiple.file,header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=".")
        Multipledata<-edit(Multipledata)
        Multipledata<- na.omit(Multipledata)
        cat("\n\n")
        show(Multipledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(MultipleAnalyze(Multipledata))
          }
 else {
  if (pick == 10){
      return (MultipleBatchmenu())
                }
          }
         }
        }
       }
      }
     }
    }
  }
 }
}