# List of Stability Menu
stabilitymenu<-function()
{
cat("\n")
  file.menu <- c("Decision Tree for Data Evaluation",
                 "Statistical Approaches to Stability Data Analysis",
                 "Back to Go menu",
                 "Quit")
cat("\n")
  pick <- menu(file.menu, title = " << Stability menu >> ")
    if (pick == 1){
      cat("\n")
      decisiontree()}

    else {
    if (pick == 2){
        cat("\n")
       statistical()
       }
    else {
    if (pick == 3){
        cat("\n")
        go()
        }

    else {
    if (pick == 4){
        cat("\nBye~~ \n\n")}
         }
     }
  }
}