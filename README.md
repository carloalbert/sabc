The SABC package is an R implementation of the Approximate Bayes Computations algorithm presented in [1].

To install, you need to take the following steps:

1) Install devtools by following the instructions at http://cran.r-project.org/web/packages/devtools/README.html
This step is platform-dependent. If you are using Windows, make sure to pick the right version of Rtools depending
on which distribution of R you are using (which can be found via session_info() ). 

2) Start a new R session, load the package devtools with 
> library(devtools)

and then install SABC with
> install_github("carloalbert/sabc",subdir="SABC")

3) Load SABC and read the help file
> library(SABC)

> ?SABC



--- 
[1]  C. Albert, H. R. Kuensch and A. Scheidegger. A Simulated Annealing Approach to Approximate Bayes Computations.
     Statistics and Computing 0960-3174 (2014).
     http://dx.doi.org/10.1007/s11222-014-9507-8 
     http://arxiv.org/abs/1208.2157

