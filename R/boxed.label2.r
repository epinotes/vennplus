boxed.labels2 <- function (x, y = NA, labels,
          bg = ifelse(match(par("bg"), "transparent", 0), "white", par("bg")),
          border = TRUE, xpad = 1.2, ypad = 1.2, 
          srt = 0, cex = 1, adj = 0.5, xlog=FALSE, ylog=FALSE, ...) {
  
  shadowtext <- function(x, y=NULL, labels, col='black', bg='ivory',
                         theta= seq(pi/4, 2*pi, length.out=8), r=0.1, ... ) {
    
    xy <- xy.coords(x,y)
    xo <- r*strwidth('A')
    yo <- r*strheight('A')
    
    for (i in theta) {
      text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
    }
    text(xy$x, xy$y, labels, col=col, ... )
  }
  
  oldpars <- par(c("cex", "xpd"))
  par(cex = cex, xpd = TRUE)
  if (is.na(y) && is.list(x)) {
    y <- unlist(x[[2]])
    x <- unlist(x[[1]])
  }
  box.adj <- adj + (xpad - 1) * cex * (0.5 - adj)
  if (srt == 90 || srt == 270) {
    bheights <- strwidth(labels)
    theights <- bheights * (1 - box.adj)
    bheights <- bheights * box.adj
    lwidths <- rwidths <- strheight(labels) * 0.5
  }
  else {
    lwidths <- strwidth(labels)
    rwidths <- lwidths * (1 - box.adj)
    lwidths <- lwidths * box.adj
    bheights <- theights <- strheight(labels) * 0.5
  }
  args <- list(x = x, y = y, labels = labels, srt = srt, adj = adj, 
               col = ifelse(colSums(col2rgb(bg) * c(1, 1.4, 0.6)) < 350, "white", "black"))
  args <- modifyList(args, list(...))
  if(xlog){
    xpad <- xpad*2
    xr <- exp(log(x) - lwidths * xpad)
    xl <- exp(log(x) + lwidths * xpad)
  }
  else{
    xr <- x - lwidths * xpad
    xl <- x + lwidths * xpad
  }
  if(ylog){
    ypad<-ypad*2
    yb<-exp(log(y) - bheights * ypad)
    yt<-exp(log(y) + theights * ypad)
  }
  else{
    yb<-y - bheights * ypad
    yt<-y + theights * ypad
  }	
  rect(xr, yb, xl, yt, col = bg, border = border)
  do.call(shadowtext, args)
  par(cex = oldpars)
}