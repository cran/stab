#Menu for Data Analysis for a Single Batch or Data Analysis for multiple Batches
statistical<-function()
{
cat("\n")
  file.menu <- c("Step 2-1: Data Analysis for a Single Batch",
                 "Step 2-2: Data Analysis for multiple Batches",
                 "Back to upper level",
                 "Back to Stability menu",
                 "Quit")
cat("\n")
  pick <- menu(file.menu, title = " << Statistical Analysis menu >> ")
    if (pick == 1){
      cat("\n")
      SingleBatchmenu()}

    else {
    if (pick == 2){
        cat("\n")
       MultipleBatchmenu()
       }
    else {
    if (pick == 3){
        cat("\n")
        decisiontree()
        }
    else {
    if (pick == 4){
        cat("\n")
        stabilitymenu()
        }
    else {
    if (pick == 5){
        cat("\nBye~~ \n\n")}
         }
     }
   }
 }
}