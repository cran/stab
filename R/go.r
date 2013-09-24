# List of Stability Menu
go<-function()
{
options(warn=-1)

cat("\n")
  file.menu <- c("Start a new project",
                 "Run demo",
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
        dev.off()
        go()
       }

    else {
    if (pick == 3){
        cat("\n  Thanks for using stab for R. Bye now.\n\n")
        graphics.off()}
         }
   }
}