bye<-function()
{
cat("\n")
  file.menu <- c("Back to stability menu",
                 "Quit")
  cat("\n")
  pick <- menu(file.menu, title = " << Try again or quit now. >> ")
    if (pick == 1){
     cat("\n")
     go()}
    else {
     if (pick == 2){
        cat("\nBye~~ \n\n")}
        }
  }