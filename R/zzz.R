# change the following line since R v.2.15.3 as .onAttach [2013/3/4 AM 06:27:15]
# .First.lib <- function(...) {

.onAttach <- function(lib, pkg)  {

# echo output to screen
packageStartupMessage("
***************************************************************************** 
                                                                              
                                  stab                                        
                                 v0.1.4                                       
                                                                              
  This package is designed to analyze drug stability data. The ICH Guideline  
  'Q1E Evaluation for Stability Data' is followed to propose a retest period  
  or shelf life.  This guideline describes when and how extrapolation should  
  be considered when proposing a retest period for a drug substance or a      
  shelf life for a drug product that extends beyond the period covered by     
  available data from the stability under the long-term storage condition.    
                                                                              
 The following steps will be performed:                                       
 ->1: Decision Tree for Data Evaluation (batch data poolability)              
 ->2: Statistical Approaches to Stability Data Analysis                       
     ->2-1: Data Analysis for a Single Batch; or                              
     ->2-2: Data Analysis for multiple Batches                                   
                                                                              
                  Please type 'go()' to get started.                          
                                                                              
*****************************************************************************")
}
