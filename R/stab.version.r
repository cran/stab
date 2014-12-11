stab.version<-function(){
cat("\n")
cat("------------------ stab for R v0.1.9 ------------------\n\n")
cat(" This report was generated on",date(),"\n\n")
username<-Sys.info()[['user']]
cat(" R version:",gsub("R version ","",R.Version()[['version.string']],fixed=TRUE),"\n")
osname_version<-c(paste(Sys.info()[['sysname']],"-",Sys.info()[['version']],"\n",
                  Sys.info()[['release']],",",Sys.info()[['machine']]))
cat(" system OS:",osname_version,"\n")
cat(" user id:",username,"\n\n")
cat(" stab is developed by Hsin-ya Lee & Yung-jin Lee,\n")
cat(" and is under license of GPL-2|GPL-3.\n")
cat(" contact: Yung-jin Lee <mobilepk at gmail.com> \n\n")
cat(" citation:\n")
cat("  Lee, Hsin-ya and Lee, Yung-jin (2014). stab: data analysis of drug\n")
cat("  stability for shelf life estimation. R package version 0.1.9.\n")
cat("  <URL: http://CRAN.R-project.org/package=stab>.\n\n")
cat("-------------------------------------------------------\n\n")
}