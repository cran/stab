dataset & test results for stab for R
---
p602.Rdata  - single batch dataset
p609.Rdata  - multiple batches dataset (for Model#1)
p613.Rdata  - multiple batches dataset (for Model#3)
p615.Rdata  - multiple batches dataset (for Model#2)
---
How to use:
copy all these dataset files to your
working directory, and load it from stab
for test run.
---

*.txt files are output results obtained from
stab for R. All tests were using lower limits 
set as 90%, as indicated in the textbook (not
stated in text but can be figured out in applied
equations shown in the textbook.

These examples were obtained from a textbook:
Ranga Velagaleti, Section 7.STABILITY AND SHELF 
LIFE OF PHARMACEUTICAL PRODUCTS, Pharmaceutical 
Manufacturing Handbook: Regulations and Quality,
edited by Shayne Cox Gad Copyright c 2008 John 
Wiley & Sons, Inc., pp.557-724.

p602 - mean was from p.602 of the textbook,
as well as other dataset.

In summary, except p615 (Model#2) has slight
different. Others are the same as the textbook.
###
dataset    textbook&   stab for R       Minitab 
                                  (stability macro)#
---------------------------------------------------
  p602       23          23             -

  p609       32          32             32

  p613       24          24             25
             23          23             21
             17*         17*            18*
             
  p615       16*         17*            17*
             22          20             21
             30          31             33
---------------------------------------------------
*: picked shelf-life.
&: using QNLS with MATLAB.
#: Commnad lines were:
   ---
   %stability c1 c2 c3;
   nograph;
   life 90.
   ---

             


