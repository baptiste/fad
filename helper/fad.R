ym <- 1
v <- 1


fad <- function(gamma=1, sigma=1, x0=1, A=1, dat=readRDS("cv.rds")){
  
  x <- dat[,1]
  wv <- 1e7 / x
  
  one <- function(g, s, x0, A){
    v <- Voigt(wv,  1e7 / x0, g, s, real = FALSE)
    A*Re(v)/max(Re(v))
  }
  
  all <- mapply(one, g=gamma, s=sigma, x0=x0, A=A, SIMPLIFY = FALSE)
  
  print("all")
  print(str(all))
  d <- data.frame(do.call(cbind, all))
  names(d) <- paste0("curve", seq_along(gamma))
  ym <- rowSums(d) 
  data.frame(wavelength = x, 
             data= dat[,2]/max(dat[,2]),
             model = ym/max(ym), d)
}



model <- function(p, d){
  pm <- matrix(p, nrow=4)
  fad(gamma=pm[1,], sigma=pm[2,], x0=pm[3,], A=pm[4,], dat=d)[,3]
}

cost <- function(p, d){
  ym <- model(p, d)
  sum((ym - d[,2])^2)
}