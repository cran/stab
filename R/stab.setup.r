stab.setup<- function() {

options(warn=-1)
onesidedlo<-NULL
onesidedup<-NULL
twosided<-NULL
onesidedlo <<- TRUE
onesidedup <<- FALSE
twosided   <<- FALSE
stab_output_filename<-NULL
stab_output_filename<<-""         ### this is a must... assign a initial value for this global var.
Lper<-NULL
Uper<-NULL
CI_percent<-NULL
Lper<<-0.
Uper<<-0.
CI_percent<<-0.
req.closezz<-FALSE

if(!file.exists("stab_setup_readme.txt")){
     req.closezz<-TRUE
     zz <- file("stab_setup_readme.txt", open="wt")
     sink(zz,split=TRUE)
}  ### set 'split=TRUE' to see output on the screen simultaneously. -YJ

     cat("\n\n  How to setup stab for R:\n\n")
     cat("----------- CI and related Schemes ----------\n")
     cat(" Methods                               Values\n")
     cat("---------------------------------------------\n")
     cat(" one-sided lower LC (default).............. 0 \n")
     cat(" one-sided upper LC ....................... 1 \n")
     cat(" two-sided LC ............................. 2 \n")
     cat("--------------------------------------------- \n")
     cat(" % lower limit of acceptance (default).... 90 \n")
     cat(" % upper limit of acceptance (default)... 110 \n")
     cat(" % CI (default) .......................... 95 \n")
     cat("---------------------------------------------\n\n")

if(file.exists("stab.setup.rds")){
     stab.set<-readRDS("stab.setup.rds")
  }
 else{
     stab.set<-data.frame(Methods=c("method (0/1/2)","lower limit (%)","upper limit (%)","CI (%)"),
                          Setting=c(0,90,110,95))
     saveRDS(stab.set,"stab.setup.rds")
}
  if(req.closezz){sink();close(zz)}

  Lper<<-stab.set[2,2]
  Uper<<-stab.set[3,2]
  CI_percent<<- stab.set[4,2]/100

  method_txt<-""
    
  if(stab.set[1,2]==0) {method_txt = "  one-sided lower LC";onesidedlo<<-TRUE;onesidedup<<-FALSE;twosided<<-FALSE}
  if(stab.set[1,2]==1) {method_txt = "  one-sided upper LC";onesidedlo<<-FALSE;onesidedup<<-TRUE;twosided<<-FALSE}
  if(stab.set[1,2]>1)  {method_txt = "  two-sided LC";onesidedlo<<-FALSE;onesidedup<<-FALSE;twosided<<-TRUE}
  
  stab.set_txt<-data.frame(Methods=c("method (0/1/2)","lower limit (%)","upper limit (%)","CI (%)"),
                          Setting=c(stab.set[1,2],stab.set[2,2],stab.set[3,2],stab.set[4,2]),
                          which_is=c(method_txt,"as defined","as defined","as defined"))

  cat("\n --- Current settings ---\n\n");show(stab.set_txt);cat("\n")
  cat("(1) User can change these settings from the top menu.\n")
  cat("    Resize your R console or terminal if you cannot  \n")
  cat("    see all settings.\n")
  cat("(2) Select [**Edit setup file] from the top menu \n")
  cat("    and then scroll up this terminal to see more.\n")
  cat("(3) All these settings are not applied when running\n")
  cat("    with demo dataset.\n\n")
  
}