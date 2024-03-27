check_demographic_parity <- function(dat){
  
  pvals <- rep(NA, ncol(dat) - 2)
  
  for(i in 3:ncol(dat)){
    d2 <- dat[dat[,i] > 0,]
    suppressWarnings(p <- prop.test(x = d2[,i], n = d2[,i-1], alternative = "two.sided"))
    pvals[i-2] <- p$p.value
    }
  
  sig <- ifelse(pvals < 0.05, "Fails Relative to Previous Step (p < 0.05)", "Passes Relative to Previous Step (p >= 0.05)")
  
  toreturn <- data.frame("Step" = colnames(dat)[3:ncol(dat)], "Demographic Parity" = sig)
  
  return(toreturn)
  
}



