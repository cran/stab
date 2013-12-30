# List of Stability Menu
go<-function()
{
stab.setup()

onesidedlo <- onesidedlo
onesidedup <- onesidedup
twosided   <- twosided
Lper<-Lper
Uper<-Uper
CI_percent<-CI_percent

options(warn=-1)

cat("\n")
  file.menu <- c("Start a new project",
                 "Run demo",
                 "**Edit setup file",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << Top menu >> ")
    if (pick == 1){
      cat("\n")
      stabilitymenu()}

    else {
    if (pick == 2){
        cat("\n")
        ### demostability()
        stab_test()
        readline("  Press Enter to continue...\n\n")
        graphics.off()
        go()
       }
    if (pick == 3){
        graphics.off()
        cat("*** Please read following messages first. ***\n\n")
        cat("(1) After selecting [** Edit the setup files], users\n")
        cat("    can scroll up this terminal to see more.\n")
        cat("(2) Please close this (linux distro users click\n")
        cat("    'x', not 'Quit') directly if use defaults.\n\n")
        readline(" Press Enter to proceed...\n")
        stab.set<-readRDS("stab.setup.rds");stab.set<-edit(stab.set)
        if(stab.set[2,2]<=0 ||stab.set[2,2]>100.) stab.set[2,2]<<-90     ### set as default if going wrong.-> Lper
        if(stab.set[3,2]<=0 ||stab.set[3,2]>150.) stab.set[3,2]<<-110    ### set as default if going wrong.-> Uper
        if(stab.set[4,2]<=0 ||stab.set[4,2]>100.) stab.set[4,2]<<-95     ### set as default if going wrong.-> Lper & Uper
        saveRDS(stab.set,"stab.setup.rds")
        go()
       }
    else {
    if (pick == 4){
        cat("\n  Thanks for using stab for R. Bye now.\n\n")
        graphics.off()}
         }
   }
}