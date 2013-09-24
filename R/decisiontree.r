#Decision Tree for Data Evaluation for Retest Period os Shelf Life Estimation for Drug Substances or Product (excluding Frozen)
decisiontree<-function()
{
cat("\n")
cat("****************************************************************************\n")
cat("*            Step1: Decision Tree for Data Evaluation                      *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("*                                                                          *\n")   
cat("*  Decision Tree for Data Evaluation for Retest Period or Shelf Life       *\n")
cat("*  Estimation for Drug Substances or Products (excluding Frozen Products)  *\n")                                     
cat("*                                                                          *\n") 
cat("****************************************************************************\n")
cat("\n")
cat("How long Period covered by long-term data (e.g. 12 months)\n")
X <- scan(nlines=1,quiet=TRUE)
cat("\n")
cat("-->Significant change at accelerated condition within 6 months?\n")
cat("\n (y) yes /(n) no ?\n\n")
ans<-readline()
cat("\n")
 if (ans == "n" | ans == "N"){
  cat("-->Long-term data show: 1. little or no change over time and \n") 
  cat("                        2. little or no variability?       \n\n") 
  cat("\n (y) yes to both / (n) no to 1. or 2. \n")
  ans<-readline()
  cat("\n")
      if (ans == "n" | ans == "N"){
      cat("-->1. Long-term data amenable to statistical analysis and \n") 
      cat("   2. statistical analysis performed? \n\n") 
      cat("\n (y) yes to both / (n) no to 1. or 2. \n")
      ans<-readline()
      cat("\n")
           if (ans == "n" | ans == "N"){
           #way 3
           cat("--------------------------------------------------------------------------\n")
           cat("                               <<Suggestion>>                             \n")
           cat(" If backed by relevant supporting data : Proposed retest period or shelf  \n")
           cat(" life = up to",1.5*X, "months/weeks, but not exceeding",X+6,"months/weeks; or if      \n")
           cat(" refrigerated, Proposed retest period or shelf life= up to",X+3,"months/weeks   \n") 
           cat("--------------------------------------------------------------------------\n")
           cat("\n")
           statistical()
           }
           else{
           #way 2
           cat("--------------------------------------------------------------------------\n")
           cat("                              <<Suggestion>>                              \n")
           cat(" If backed by relevant supporting data : Proposed retest period or shelf  \n") 
           cat(" life = up to",2*X, "months/weeks","but not exceeding",X+12,"months/weeks; or if      \n") 
           cat(" refrigerated, Proposed retest period or shelf life = up to",1.5*X ,     "\n")
           cat(" months, but not exceeding",X+6,"months/weeks",                                "\n")
           cat("--------------------------------------------------------------------------\n") 
           cat("\n")
           statistical()
              }
            }   
      else{
      cat("-->Accelerated data show: 1. little or no change over time and \n") 
      cat("                          2. little or no variability?       \n\n") 
      cat("\n (y) yes to both / (n) no to 1. or 2. \n")
      ans<-readline()
      cat("\n") 
           if (ans == "n" | ans == "N"){
           cat("-->1. Long-term data amenable to statistical analysis and \n") 
           cat("   2. statistical analysis performed? \n\n") 
           cat("\n (y) yes to both / (n) no to 1. or 2. \n")
           ans<-readline()
           cat("\n")
                if (ans == "n" | ans == "N"){
                #way 3
                cat("--------------------------------------------------------------------------\n")
                cat("                               <<Suggestion>>                             \n")
                cat(" If backed by relevant supporting data : Proposed retest period or shelf  \n")
                cat(" life = up to",1.5*X, "months/weeks","but not exceeding",X+6,"months/weeks; or if     \n")
                cat(" refrigerated, Proposed retest period or shelf life = up to",X+3,"months/weeks  \n") 
                cat("--------------------------------------------------------------------------\n")
                cat("\n")
                statistical()
                }
                else{
                #way 2
                cat("--------------------------------------------------------------------------\n")
                cat("                              <<Suggestion>>                              \n")
                cat(" If backed by relevant supporting data : Proposed retest period or shelf  \n") 
                cat(" life = up to",2*X, "months/weeks","but not exceeding",X+12,"months/weeks; or if      \n") 
                cat(" refrigerated, Proposed retest period or shelf life = up to",1.5*X ,  "   \n")
                cat(" months, but not exceeding",X+6,"months/weeks                                   \n")
                cat("--------------------------------------------------------------------------\n") 
                cat("\n")
                statistical()
                   }
                }      
           else{      
           #way 1
           cat("--------------------------------------------------------------------------\n")
           cat("                              <<Suggestion>>                              \n")
           cat(" Statistical analysis is normally unnecessary: Proposed retest period or  \n") 
           cat(" shelf life = up to", 2*X, "months/weeks,but not exceeding",X+12,"months/weeks; or if \n") 
           cat(" refrigerated, Proposed retest period or shelf life = up to", 1.5*X,"     \n")
           cat(" months but not exceeding",X+6,"months/weeks                                    \n")
           cat("--------------------------------------------------------------------------\n") 
           cat("\n")
           statistical()
               }
           }    
         }
      
  else {
  cat("-->Intended to be stored in a refrigerator?\n")
  cat("\n (y) yes /(n) no ?\n\n")
  ans<-readline()
  cat("\n")
       if (ans == "n" | ans == "N"){
          cat("-->Significant change at intermediate condition? \n") 
          cat("\n (y) yes /(n) no ?\n\n")
          ans<-readline()
          cat("\n")
               if (ans == "n" | ans == "N"){
               cat("-->1. Long-term data amenable to statistical analysis and \n") 
               cat("   2. statistical analysis performed? \n\n") 
               cat("\n (y) yes to both / (n) no to 1. or 2. \n")
               ans<-readline()
               cat("\n")
                   if (ans == "n" | ans == "N"){
                   #way 5
                   cat("--------------------------------------------------------------------------\n")
                   cat("                              <<Suggestion>>                              \n")
                   cat(" If backed by relevant supporting data: Proposed retest period or shelf   \n") 
                   cat(" life= up to",3+X, "months/weeks                                                \n") 
                   cat("--------------------------------------------------------------------------\n")
                   cat("\n")
                   statistical()
                   }
                   else{
                   #way 4
                   cat("--------------------------------------------------------------------------\n")
                   cat("                              <<Suggestion>>                              \n")
                   cat(" If backed by statistical analysis and relevant supporting data: Proposed \n") 
                   cat(" retest period or shelf life = up to",1.5*X,"months/weeks, but not exceeding    \n")
                   cat("",X+6,"months/weeks                                                             \n")                                                         
                   cat("--------------------------------------------------------------------------\n")
                   cat("\n")
                   statistical()
                       }
                    }   
               else{
               #way6
               cat("--------------------------------------------------------------------------\n")
               cat("                              <<Suggestion>>                              \n")
               cat(" No extrapolation; shorter retest period or shelf life can be called for;  \n")
               cat(" statistical analysis if long-term data show variability                  \n") 
               cat("--------------------------------------------------------------------------\n")
               cat("\n")
               statistical()
                   }
               }    
       else{
       cat("-->Significant change at accelerated condition within 3 months? \n") 
       cat("\n (y) yes /(n) no ?\n\n")
       ans<-readline()
       cat("\n")
            if (ans == "n" | ans == "N"){
            #way 6
            cat("--------------------------------------------------------------------------\n")
            cat("                              <<Suggestion>>                              \n")
            cat(" No extrapolation; shorter retest period or shelf life can be called for; \n")
            cat(" statistical analysis if long-term data show variability                  \n") 
            cat("--------------------------------------------------------------------------\n")
            cat("\n")
            statistical()
            }
            else{
            #way 7
            cat("--------------------------------------------------------------------------\n")
            cat("                              <<Suggestion>>                              \n")
            cat(" No extrapolation; shorter retest period or shelf life and data covering  \n")
            cat(" excursions can be called for; statistical analysis if long-term data      \n")
            cat(" show variability                                                         \n")  
            cat("--------------------------------------------------------------------------\n")
            cat("\n")
            statistical()
            }
          }
      }  
 }


 