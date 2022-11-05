################################################################################

# AMMSA Swedish Validation -- Helper Functions

################################################################################

# COMPOSITE RELIABILITY --------------------------------------------------------

# Note: `fit` argument must be a standardized solution data frame

composite.rel <- function(fit) {
  
  # Extract only loadings
  
  sub <- fit[fit$op == "=~", ]
  latent <- unique(sub$lhs)
  n.latent <- length(latent)
  
  # Set up empty vector
  
  comp.rel <- rep(NA, n.latent)
  
  # Compute reliabilities
  
  for (i in 1:n.latent) {
    
    sub.sub <- sub[sub$lhs == latent[i], ]
    
    loadings <- sub.sub$est.std
    
    res.var <- 1 - loadings^2
    
    comp.rel[i] <- sum(loadings)^2 / (sum(loadings)^2 + sum(res.var))
    
  }
  
  # Return data frame of composite reliabilities
  
  out <- data.frame(latent, comp.rel)
  
  return(out)
  
}

# MARDIA'S TESTS FOR MULTIVARIATE SKEWNESS AND KURTOSIS ------------------------

# Note: `data` argument must be a data frame containing only the variables you want to examine for multivariate normality

# This function is closely based on the mult.norm() function in the QuantPsyc package, but it has been modified to apply a correction to the covariance matrix, in accordance with common practice (and common software implementations of Mardia's tests).

mardia <- function(data) {
  
  data <- na.omit(as.matrix(data))
  
  N <- nrow(data)
  p <- ncol(data)
  
  s = cov(data) * (N - 1)/N
  dfchi <- p * (p + 1) * (p + 2)/6
  
  o <- matrix(1, N)
  I <- matrix(0, N, N)
  diag(I) <- 1
  Q <- I - 1/N * o %*% t(o)
  g <- Q %*% data %*% solve(s) %*% t(data) %*% Q
  
  b1p <- 1/(N^2) * sum(g^3)
  b2p <- sum(diag(g)^2)/N
  k1 <- (N/6) * b1p
  k2 <- (b2p - p * (p + 2))/sqrt(8 * p * (p + 2)/N)
  pskew <- 1 - pchisq(k1, dfchi)
  pkurt <- 2 * (1 - pnorm(abs(k2)))
  
  out <- data.frame(
    skewness = b1p,
    z.skew = k1,
    p.skew = pskew,
    kurtosis = b2p,
    z.kurt = k2,
    p.kurt = pkurt
  )
  
  return(out)
  
}

# LITTLE'S MCAR TEST -----------------------------------------------------------

# From https://github.com/rcst/little-test/blob/master/mcar.R

mcar <- function(x){ 
  if(!require(norm)) {
    stop("You must have norm installed to use LittleMCAR") 
  } 
  
  # if(!require(data.table)) {
  # 	stop("Please install the R-package data.table to use mcar")
  # }
  
  if(!(is.matrix(x) | is.data.frame(x))) {
    stop("Data should be a matrix or dataframe")
  }
  
  if (is.data.frame(x)){
    x <- data.matrix(x)
  }
  
  # delete rows of complete missingness
  foo <- function(x) return(any(!is.na(x)))
  dd <- apply(X = x, MARGIN = 1L, FUN = foo)
  dd <- which(!dd, arr.ind = TRUE)
  if(length(dd) > 0) 
    x <- x[-dd,]
  
  # define variables        
  n.var <- ncol(x) # number of variables
  n <- nrow(x)  #number of respondents
  var.names <- colnames(x)
  r <- 1 * is.na(x)
  
  nmis <- as.integer(apply(r, 2, sum))  #number of missing data for each variable REWRITE
  mdp <- (r %*% (2^((1:n.var - 1)))) + 1  #missing data patterns
  x.mp <- data.frame(cbind(x,mdp)) # add column indicating pattern
  colnames(x.mp) <- c(var.names,"MisPat") # set name of new column to MisPat
  n.mis.pat <- length(unique(x.mp$MisPat)) # number of missing data patterns
  p <- n.mis.pat-1 # number of Missing Data patterns minus 1 (complete data row)
  
  
  s <- prelim.norm(x)
  ll <- em.norm(s)
  fit <- getparam.norm(s = s, theta = ll)
  
  # gmean<-mlest(x)$muhat #ML estimate of grand mean (assumes Normal dist)
  gmean <- fit$mu
  # gcov<-mlest(x)$sigmahat #ML estimate of grand covariance (assumes Normal dist)
  gcov <- fit$sigma
  colnames(gcov) <- rownames(gcov) <- colnames(x)
  
  #recode MisPat variable to go from 1 through n.mis.pat
  x.mp$MisPat2 <- rep(NA,n)
  for (i in 1:n.mis.pat){ 
    x.mp$MisPat2[x.mp$MisPat == sort(unique(x.mp$MisPat), partial=(i))[i]]<- i 
  }
  
  x.mp$MisPat<-x.mp$MisPat2
  x.mp<-x.mp[ , -which(names(x.mp) %in% "MisPat2")]
  
  #make list of datasets for each pattern of missing data
  datasets <- list() 
  for (i in 1:n.mis.pat){
    datasets[[paste("DataSet",i,sep="")]]<-x.mp[which(x.mp$MisPat==i),1:n.var]
  }
  
  #degrees of freedom
  kj<-0
  for (i in 1:n.mis.pat){	
    no.na<-as.matrix(1* !is.na(colSums(datasets[[i]]))) 
    kj<-kj+colSums(no.na) 
  }
  
  df<-kj -n.var
  
  #Little's chi-square
  d2<-0
  cat("this could take a while")
  
  # this crashes at the missingness pattern where every column is missing
  # this for-loop can be handled faster with plyr-function
  for (i in 1:n.mis.pat){	
    mean <- (colMeans(datasets[[i]])-gmean) 
    mean <- mean[!is.na(mean)] 
    keep <- 1* !is.na(colSums(datasets[[i]])) 
    keep <- keep[which(keep[1:n.var]!=0)] 
    cov <- gcov 
    cov <- cov[which(rownames(cov) %in% names(keep)) , which(colnames(cov) %in% names(keep))] 
    d2 <- as.numeric(d2+(sum(x.mp$MisPat==i)*(t(mean)%*%solve(cov)%*%mean)))
  }
  
  #p-value for chi-square
  p.value<-1-pchisq(d2,df)
  
  #descriptives of missing data
  amount.missing <- matrix(nmis, 1, length(nmis))
  percent.missing <- amount.missing/n
  amount.missing <- rbind(amount.missing,percent.missing)
  colnames(amount.missing) <- var.names
  rownames(amount.missing) <- c("Number Missing", "Percent Missing")
  
  list(chi.square = d2, 
       df = df, 
       p.value = p.value, 
       missing.patterns = n.mis.pat, 
       amount.missing = amount.missing, 
       data = datasets)
}

# CORRELATION MATRIX WITH 95% CI ----------------------------------

## A function to print correlation matrices with 95% CIs

cor_ci <- function(x, n, digits = 3) {
  
  z <- .5*log((1 + x)/(1 - x))
  
  se_mat <- 1 / sqrt(n - 3)
  
  out <- paste(format(round(x, digits), nsmall = digits), " [", format(round(tanh(z - (se_mat * qnorm(.975))), digits), nsmall = digits), ", " , format(round(tanh(z + (se_mat * qnorm(.975))), digits), nsmall = digits), "]", sep = "")
  
  out <- gsub("0\\.", " \\.", out)
  
  out <- matrix(out, dim(x)[1], dim(x)[2])
  
  dimnames(out) <- dimnames(x)
  
  return(out)
  
}

# Create Scatterplots

scatter_plot <- function(xvar, xlab, xlim, yvar = "ammsa_total", ylab = "AMMSA Total", ylim = c(1, 7), data = ammsa) {
  
  plot <- ggplot(data,
                 aes(
                   y = get(yvar),
                   x = get(xvar)
                 )) +
    geom_point() +
    labs(
      x = xlab,
      y = ylab,
      title = xlab
    ) +
    scale_y_continuous(
      limits = ylim,
      breaks = seq(min(ylim), max(ylim), 1)
    ) +
    scale_x_continuous(
      limits = xlim,
      breaks = seq(min(xlim), max(xlim), 1)
    ) +
    theme_classic()
  
  return(plot)
  
}
