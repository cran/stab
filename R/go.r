# List of Stability Menu
go<-function()
{
options(warn=-1)
if (noquote(unlist(format(.Platform)))[1] == "unix") {
        windows <<- function(record) {
        }
     }  

cat("\n")
  file.menu <- c("Start a new project",
                 "Demo",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << Go menu >> ")
    if (pick == 1){
      cat("\n")
      stabilitymenu()}

    else {
    if (pick == 2){
        cat("\n")
        demostability()
       }

    else {
    if (pick == 3){
        cat("\nBye~~ \n\n")}
         }
   }
}