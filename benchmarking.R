Goal: 

Given as input two lists of differring sizes: 

(1)  Data sets of increasing N
(2)  Algorithms to compute on each data set

Output: 
Plot the median time as a function of N. 

Ideas: 
(1) For each subsequent N, the first step is to run the benchmark with `times=1L`  Then take two actions.  
   (a) Compute the Total amount of time taken and have the `tt` in `times=tt` be a function of the Total time
   (b) Check for any draggers. If present, run again with Times=3L, and if they continue to drag on all three runs, flag them as no longer in the running. 
(2) The main argument to the function should be a list of unevaluated expressions. One of those should be a data=generating function. 
(3) We might have different methods for this general routine, depending on the dimension of the input. 


library(microbenchmark)


results <- microbenchmark(...... )

median.results <- data.table(results)[, median(time)/1000, by=expr]
