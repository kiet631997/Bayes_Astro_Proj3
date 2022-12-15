hist_ci <- function(x, Q = c(0.025, 0.975), name = bquote(beta), 
                    xlim = mean(x) + 3*c(-1,1)*sd(x))
{
  den <- density(x = x)
  plot(x = den$x,y = den$y, type = 'l',
       xlab = name, ylab = 'density',
       xlim = xlim)
  t <- quantile(x, probs = Q)
  s <- seq(t[1], t[2], 1e-5)
  val <- approx(den$x, den$y, xout = s)$y
  polygon(x = c(s, sort(s, decreasing = T)),
          y = c(val,rep(0, length(s))),
          col = 'azure')
  if(0>min(s) | 0<max(s))
  {
    abline(v = 0,
           lty = 2, col = 'red', lwd = 2)
  }
  
}

