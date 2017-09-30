

fad <- function(gamma=1, sigma=1, x0=1, A=1, dat){
  
  x <- dat[,1]
  wv <- 1e7 / x
  
  one <- function(g, s, x0, A){
    v <- Voigt(wv,  1e7 / x0, g, s, real = FALSE)
    A*Re(v)/max(Re(v))
  }
  
  all <- mapply(one, g=gamma, s=sigma, x0=x0, A=A, SIMPLIFY = FALSE)
  d <- do.call(cbind, all)
  ym <- rowSums(d) 
  
  data.frame(wavelength = x, data= dat[,2]/max(dat[,2]), model = ym/max(ym), d)
}

plot_fad <- function(d, xlim=NULL){
  palette(c("black", RColorBrewer::brewer.pal(5, "Set1")))
  lty <- c(rep(1, ncol(d)), 2, 1)
  col <- c(seq(1, ncol(d))+2, 1, 2)
  col2 <- c(1,2)
  # par(mfrow=c(1,2), mar=c(4,2,1,2))
  # matplot(d[,1], d[,-1], 
  #         lty=lty, col=col,
  #         t="l", xlab="wavelength /nm", ylab="", xlim=xlim)
  # title(main="Individual components")
  # 

}

model <- function(p, d){
  pm <- matrix(p, nrow=4)
  fad(gamma=pm[1,], sigma=pm[2,], x0=pm[3,], A=pm[4,], dat=d)[,3]
}

cost <- function(p, d){
  ym <- model(p, d)
  sum((ym - d[,2])^2)
}