#choose separator and decimal type
SingleBatchcsv<-function()
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
               "Go Back to Single Batch menu")
cat("\n")
pick <- menu(file.menu, title = " << Separator type and Decimal type >> ")
if (pick == 1){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extension of .csv)\n")
        ### Single.file <-readline()
        ### Single.file<-paste(Single.file,".csv",sep="")
        cnames<-c("Time","Assay")
        Singledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=",",dec=".")
        Singledata<-edit(Singledata)
        Singledata<- na.omit(Singledata)
        cat("\n\n")
        show(Singledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(SingleAnalyze(Singledata))
     }

 else {
  if (pick == 2){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extension of.csv)\n")
        ### Single.file <-readline()
        ### Single.file<-paste(Single.file,".csv",sep="")
        cnames<-c("Time","Assay")
        Singledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=",")
        Singledata<-edit(Singledata)
        Singledata<- na.omit(Singledata)
        cat("\n\n")
        show(Singledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(SingleAnalyze(Singledata))
          }
 else {
  if (pick == 3){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extension of.csv)\n")
        ### Single.file <-readline()
        ### Single.file<-paste(Single.file,".csv",sep="")
        cnames<-c("Time","Assay")
        Singledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=".")
        Singledata<-edit(Singledata)
        Singledata<- na.omit(Singledata)
        cat("\n\n")
        show(Singledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(SingleAnalyze(Singledata))
          }
else {
  if (pick == 4){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extension of.csv)\n")
        ### Single.file <-readline()
        ### Single.file<-paste(Single.file,".csv",sep="")
        cnames<-c("Time","Assay")
        Singledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=",")
        Singledata<-edit(Singledata)
        Singledata<- na.omit(Singledata)
        cat("\n\n")
        show(Singledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(SingleAnalyze(Singledata))
          }
else {
  if (pick == 5){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extension of.csv)\n")
        ### Single.file <-readline()
        ### Single.file<-paste(Single.file,".csv",sep="")
        cnames<-c("Time","Assay")
        Singledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=".")
        Singledata<-edit(Singledata)
        Singledata<- na.omit(Singledata)
        cat("\n\n")
        show(Singledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(SingleAnalyze(Singledata))

          }
else {
  if (pick == 6){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extension of.csv)\n")
        ### Single.file <-readline()
        ### Single.file<-paste(Single.file,".csv",sep="")
        cnames<-c("Time","Assay")
        Singledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=",")
        Singledata<-edit(Singledata)
        Singledata<- na.omit(Singledata)
        cat("\n\n")
        show(Singledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(SingleAnalyze(Singledata))
          }
else {
  if (pick == 7){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extension of.csv)\n")
        ### Single.file <-readline()
        ### Single.file<-paste(Single.file,".csv",sep="")
        cnames<-c("Time","Assay")
        Singledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=".")
        Singledata<-edit(Singledata)
        Singledata<- na.omit(Singledata)
        cat("\n\n")
        show(Singledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(SingleAnalyze(Singledata))
          }
else {
  if (pick == 8){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extension of.csv)\n")
        ### Single.file <-readline()
        ### Single.file<-paste(Single.file,".csv",sep="")
        cnames<-c("Time","Assay")
        Singledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=",")
        Singledata<-edit(Singledata)
        Singledata<- na.omit(Singledata)
        cat("\n\n")
        show(Singledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(SingleAnalyze(Singledata))
          }
else {
  if (pick == 9){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extension of.csv)\n")
        ### Single.file <-readline()
        ### Single.file<-paste(Single.file,".csv",sep="")
        cnames<-c("Time","Assay")
        Singledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=".")
        Singledata<-edit(Singledata)
        Singledata<- na.omit(Singledata)
        cat("\n\n")
        show(Singledata)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to analyze the data                      *\n")
        cat("****************************************************************************\n\n")
        return(SingleAnalyze(Singledata))
          }
 else {
  if (pick == 10){
      return (SingleBatchmenu())
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