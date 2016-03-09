fad <-
function(gamma=1, sigma=1, x0=1, A=1, curveColor=1, border=1, col='#569BBD', xlim=NULL, ylim=NULL, xlab='', ylab='', digits=2, axes=1, detail=999, xLab=c('number', 'symbol'), cex.axis=1, xAxisIncr=1, ...){
  
  cv <- readRDS("./helper/cv.rds")
  x <- cv$wavelength
  wv <- 1e7 / x

  
    one <- function(g, s, x0, A){
      v <- Voigt(wv,  1e7 / x0, g, s, real = FALSE)
      A*Re(v)/max(Re(v))
    }

    all <- mapply(one, g=gamma, s=sigma, x0=x0, A=A, SIMPLIFY = FALSE)
    d <- do.call(cbind, all)
 y <- rowSums(d) 
 # y <- y/max(y)
 palette(c("black", RColorBrewer::brewer.pal(5, "Set1")))
 lty <- c(rep(1, length(gamma)), 2, 1)
 col <- c(seq(1, length(gamma))+2, 1, 2)
 col2 <- c(1,2)
 par(mfrow=c(1,2), mar=c(4,2,1,2))
  matplot(x, cbind(d, y/max(y), cv$intensity/max(cv$intensity)), 
          lty=lty, col=col,
          t="l", xlab="wavelength /nm", ylab="", xlim=c(400, 700))
  title(main="Individual components")

  matplot(x, cbind(y/max(y), cv$intensity/max(cv$intensity)), 
          lty=2:1, col=col2, t="l", xlab="wavelength /nm", ylab="")
  title(main="Sum of multiple Voigt profiles")

  # plot(x, ri[,2], t="l",  xlab="x", ylab="Imag part", col=1)

}
