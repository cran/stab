\name{MultipleAnalyze}

\alias{MultipleAnalyze}

\title{Data Analysis for Multiple Batches}

\description{
The following steps wii be conducted as follows:

step1. ANCOVA can be employed to test the difference in slopes and intercepts of
the regression lines among factors and factor combinations.
step2. Based on the statistical results, it can divide into three parts.
-> slope (P>=0.25) and intercept (P>=0.25)
  The tests for equality of slopes and equality of intercepts do not result at a
level of significance of 0.25 (there is no significant difference in slope and
intercepts among the batches).  The data from all batches can be combined.
-> slope (P>=0.25) and intercept (P<0.25)
  The test rejects the hypothesis of equality of intercepts but fails to reject that
the slopes are equal (there is a significant difference in intercepts but no
significant difference in slopes among the batches).  The data can be combined
for the purpose of estimating the common slope.
-> (slope (P<0.25) and intercept (P>=0.25)) or (slope (P<0.25) and intercept (P<0.25))
  The result in this example shows that the test rejects the hypothesis of equality of
slopes (there is a significant difference in slopes among batches).  It is not
considered appropriate to combine the data from all batches.
step3. The shelf life will be estimated.

}

\keyword{misc}