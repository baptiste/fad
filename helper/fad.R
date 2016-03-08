fad <-
function(gamma=1, sigma=1, x0=1, A=1, curveColor=1, border=1, col='#569BBD', xlim=NULL, ylim=NULL, xlab='', ylab='', digits=2, axes=1, detail=999, xLab=c('number', 'symbol'), cex.axis=1, xAxisIncr=1, ...){
  
  cv <- readRDS("./helper/cv.rds")
  x <- cv$wavelength
  
  v <- Voigt(x, x0[1], gamma[1], sigma[1], real = FALSE)
  y1 <- A[1]*Re(v)/max(Re(v))
  
  v <- Voigt(x, x0[2], gamma[2], sigma[2], real = FALSE)
  y2 <- A[2]*Re(v)/max(Re(v))
 y <- (y1+y2) / max(y1+y2)
 palette(c("black", RColorBrewer::brewer.pal(5, "Set1")))
 par(mfrow=c(2,1), mar=c(2,2,1,2))
  title(main="Fit with double Voigt profile")
  matplot(x, cbind(y1, y2, y, cv$intensity/max(cv$intensity)), lty=c(1,1,1,2), col=c(2,3, 4, 1),
          t="l", xlab="x", ylab="", xlim=c(400, 700))
  
  matplot(x, cbind(y, cv$intensity/max(cv$intensity)), lty=1, col=c(4,1),
          t="l", xlab="x", ylab="")
  
  # plot(x, ri[,2], t="l",  xlab="x", ylab="Imag part", col=1)
  
}
