#' @title A illustration dataset
#' @name S_sica_1
#' @description A dataset used to illustrate the complete example with SICA in the vignette model_selection.
NULL

#' @title A illustration dataset
#' @name PE_sica_1
#' @description A dataset used to illustrate the complete example with SICA in the vignette model_selection.
NULL

#' @title A illustration dataset
#' @name FN_sica_1
#' @description A dataset used to illustrate the complete example with SICA in the vignette model_selection.
NULL

#' @title A design matrix
#' @name X
NULL

#' @title A vector of responsors
#' @name y
NULL

#' @title Generate multivariate data for model selection
#' @description Generate multivariate data \code{list(X, y)} satisfying the linear regression model, \code{y} = \code{X} \code{beta} + \code{epsilon}, for model selection
#' @param s \code{1:s} is the support set of \code{beta}, namely the indexes of true variables
#' @param n the number of samples
#' @param p the dimensionality of variables
#' @param r a correlation parameter in the structured covariance matrix of \code{X}, seen *Model_selection vignette*.
#' @param beta parameters on the support set of true variables 
#' @param sigma a standard deviation parameter in the covariance matrix of \code{epsilon}, seen *Model_selection vignette*. 
#'
#' @return a list containing design matrix \code{X} and responsors \code{y}
#' @export
#' @importFrom MASS mvrnorm
#' @importFrom stats rnorm
#' @examples
#' \dontrun{
#' test1_data <- ms_data_gen(s,10000,p,r,beta,sigma1)
#' test1_X <- test1_data[[1]]
#' test1_y <- test1_data[[2]]
#' }
ms_data_gen <- function(s, n, p, r, beta, sigma){
  #sim2_generation
  support <- 1:s
  mu <- rep(0,p)
  Sig <- outer(1:p, 1:p,FUN = function(x,y) {r^(abs(x-y))})
  X <- mvrnorm(n,mu,Sig)
  y <- X[, support] %*% beta + rnorm(n,0,sigma)
  return(list(X,y))
}

#' @title First-order derivative of SCAD penalty
#'
#' @param t variable, a real number
#' @param a parameter, a real number
#' @param lambda parameter, a real number
#'
#' @return a real number which is the first-order derivative of SCAD penalty 
#' @export
scad_deri <- function(t,a,lambda){
  return(1*I(t <= lambda) + I(t>lambda)*max(0,a*lambda-t)/((a-1)*lambda))
}

#' @title First-order derivative of MCP penalty
#'
#' @param t variable, a real number
#' @param a parameter, a real number
#' @param lambda parameter, a real number
#'
#' @return a real number which is the first-order derivative of MCP penalty 
#' @export
mcp_deri <- function(t,a,lambda){
  return(max(a*lambda-t,0)/a/lambda)
}

#' @title First-order derivative of SICA penalty
#'
#' @param t variable, a real number
#' @param a parameter, a real number
#'
#' @return a real number which is the first-order derivative of SICA penalty 
#' @export
sica_deri <- function(t,a){
  return(a*(a+1)/(a+t)^2)
}


#' @title Cross validation for model selection with SICA
#' @description Cross validation method for model selection with SICAs. Apply this and pick out the optimal parameter \code{a} in SICAs and optimal regularized parameter \code{lambda}.
#' @param X design matrix
#' @param y responsors
#' @param pentype type of penalty, choose from "SICA", "SCAD" and "MCP"
#' @param seq_a a vector of different \code{a}s, where the optimal \code{a} is chosen
#' @param seq_lambda a vector of different \code{lambda}s, where the optimal \code{lambda} is chosen
#'
#' @return a list contains optimal \code{a} and optimal \code{lambda}
#' @export
#'
#' @examples
#' \dontrun{
#' data(list = c("X", "y"))
#' seq_a <- c(7,9,11)
#' seq_lambda <- c(0.13,0.15,0.17)
#' pentype = "SCAD"
#' # Optimal a and lambda
#' ms_cv(X, y, pentype, seq_a, seq_lambda)
#' }
ms_cv <- function(X, y, pentype, seq_a, seq_lambda){
  k <- nrow(X)/5
  n_a <- length(seq_a)
  n_lambda <- length(seq_lambda)
  mses <- matrix(0, n_a, n_lambda)
  for (i in 1:n_a) {
    for (j in 1:n_lambda) {
      train_test <- sample(rep(1:5,each = k), nrow(X), replace = FALSE)
      mses[i, j] = mean(sapply(1:5, FUN = function(l){
        train_X <- X[train_test!=l,]
        train_y <- y[train_test!=l]
        
        test_X <- X[train_test==l,]
        test_y <- y[train_test==l]
        
        beta_hat <- lla(X, y, pentype, seq_a[i], seq_lambda[j])
        
        mean((test_y - test_X%*%beta_hat)^2)
      }))
    }
  }
  ind <- which(mses == min(mses),arr.ind = TRUE)
  optimal_a <- seq_a[ind[1]]
  optimal_lambda <- seq_lambda[ind[2]]
  return(list(optimal_a,optimal_lambda))
}

#' @title LLA algorithm applied on regularized least square problems
#' @description LLA, namely local linear approximation methods, will simplify our regularized least square problems to an adaptive lasso problem. In this setting, user can choose penalty from "SICA", "SCAD" or "MCP". 
#' @param X design matrix
#' @param y responsors
#' @param pentype the type of penalties, user can choose from "SCAD", "SICA" or "MCP"
#' @param a a parameter in some penalty, all three penalties for chosen possess parameter \code{a}
#' @param lambda a regularized parameter which appears in SCAD and MCP, but not in SICA since SICA has no dependence on the regularized parameter.
#' @param K the iteration numbers, default is 1
#' @import glmnet
#' @importFrom stats coef
#' @return \code{beta}, namely the estimated parameter vectors for a least square problem
#' @export
#'
#' @examples
#' \dontrun{
#' data(list = c("X", "y"))
#' a = 0.11
#' lambda = 0.15
#' pentype = "SICA"
#' 
#' beta_hat <- lla(X, y, pentype, a, lambda)
#' }
lla <- function(X, y, pentype, a, lambda, K = 1){
  #first, we may want a warm initiation with lasso
  cvfit <- cv.glmnet(X, y, family = "gaussian", alpha = 1, intercept = F)
  beta <- as.vector(coef(cvfit, s = "lambda.min"))[-1]
  
  if (pentype == "SCAD"){
    deri <- function(abs_beta){scad_deri(abs_beta, a, lambda)}
  }
  else if(pentype == "MCP"){
    deri <- function(abs_beta){mcp_deri(abs_beta, a, lambda)}
  }
  else if(pentype == "SICA"){
    deri <- function(abs_beta){sica_deri(abs_beta, a)}
  }
  else{
    return("Unknown type of penalty")
  }
  

  for (iter in 1:K){
    #Conduct adaptive lasso
    fit <- glmnet(X, y, family = "gaussian", alpha = 1, 
                  penalty.factor = deri(abs(beta)), intercept = F, lambda = lambda)
    beta <- as.vector(coef(fit, s = lambda))[-1]
    beta[is.na(beta)] <- 0
  }
  
  #Return the result
  return(beta)
}
