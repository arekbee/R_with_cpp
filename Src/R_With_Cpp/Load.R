
library(compiler)
library(inline)
library(Rcpp)
library(microbenchmark)
library(rbenchmark)

from.1.to.8 <- 1:8
ten.to.power <- 10^ from.1.to.8


mov.avg.compiled0 <- cmpfun(mov.avg, options=list(optimize=0))
mov.avg.compiled1 <- cmpfun(mov.avg, options=list(optimize=1))
mov.avg.compiled2 <- cmpfun(mov.avg, options=list(optimize=2))
mov.avg.compiled3 <- cmpfun(mov.avg, options=list(optimize=3))


mov.avg.inline <- cfunction(
  sig=signature(x="numeric", n="integer"),
  body="
  SEXP x2 = PROTECT(coerceVector(x, REALSXP));
  SEXP n2 = PROTECT(coerceVector(n, INTSXP));
  
  double *x_p = REAL(x2);
  int n_val = asInteger(n2);
  
  int x_len = length(x2);
  int res_len = x_len - n_val + 1;
  
  SEXP res = PROTECT(allocVector(REALSXP, res_len));
  double *res_p = REAL(res);
  for (int i = 0; i < res_len; i++) {
  res_p[i] = 0;
  }
  
  for (int j = 0; j < n_val; j++) {
  for (int k = 0; k < res_len; k++) {
  res_p[k] += x_p[j + k];
  }
  }
  for (int l = 0; l < res_len; l++) {
  res_p[l] /= n_val;
  }
  UNPROTECT(3);
  return res;"
  ,  language="C")

sourceCpp('mov_avg_Rcpp.cpp')




x <-  runif(1e3)


bench <- microbenchmark(mov.avg(x),
                        mov.avg.compiled0(x),
                        mov.avg.compiled1(x),
                        mov.avg.compiled2(x),
                        mov.avg.compiled3(x))

bench1  <- benchmark(runif(1e4), replications=10)



ys <- sapply(x, function(x) benchmark(runif(x), replications=10))

microbenchmark(runif(1e8))