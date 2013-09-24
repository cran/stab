# change the following line since R v.2.15.3 as .onAttach [2013/3/4 AM 06:27:15]

.onAttach <- function(lib, pkg)  {

# echo output to screen

packageStartupMessage("
..........................................

                          
   stab for R
   v0.1.6
                                                                   
   Please type 'go()' to run; or
   'about.stab()' to read more.                

..........................................")
}