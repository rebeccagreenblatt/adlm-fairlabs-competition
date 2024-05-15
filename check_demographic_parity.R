check_demographic_parity <- function(dat){
  
  pvals <- rep(NA, ncol(dat) - 2)
  
  for(i in 3:ncol(dat)){
    d2 <- dat[dat[,i] > 0,]
    suppressWarnings(p <- prop.test(x = d2[,i], n = d2[,i-1], alternative = "two.sided"))
    pvals[i-2] <- p$p.value
    }
  
  sig <- ifelse(pvals < 0.05, "Fails Relative to Previous Step", "Passes Relative to Previous Step")
  
  toreturn <- data.frame(colnames(dat)[3:ncol(dat)], sig)
  colnames(toreturn) <- c("Step", "Demographic Parity")
  
  return(toreturn)
  
}



