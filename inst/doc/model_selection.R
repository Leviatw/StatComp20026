## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(StatComp20026)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("Leviatw/StatComp20026")

## ----pressure, echo=FALSE, fig.cap="SICAs", out.width = '50%'-----------------
knitr::include_graphics("SICAs.png")

## -----------------------------------------------------------------------------
# Problem setting initialization
n <- 100; p<- 50; s <- 7; r <- 0.5; sigma1 <- 0.3; sigma2 <- 0.5
beta <- c(1,-0.5,0.7,-1.2,-0.9,0.3,0.55)
sim_times <- 100

## ---- eval=FALSE--------------------------------------------------------------
#  library(StatComp20026)

## -----------------------------------------------------------------------------
# Training data and testing data for sigma = 0.3
test1_data <- ms_data_gen(s,10000,p,r,beta,sigma1)
test1_X <- test1_data[[1]]
test1_y <- test1_data[[2]]

train1_data <- ms_data_gen(s,n*sim_times,p,r,beta,sigma1)
train1_X <- train1_data[[1]]
train1_y <- train1_data[[2]]

## -----------------------------------------------------------------------------
# Initialize the sequence of a and lambda
seq_a <- c(7,9,11)
seq_lambda <- c(0.13,0.15,0.17)

# Penalty type
pentype = "SCAD"

# Training set for one simulation
X <- train1_X[1:100,]
y <- train1_y[1:100,]

# Optimal a and lambda
op_par <- ms_cv(X, y, pentype, seq_a, seq_lambda)
knitr::kable(data.frame(t(op_par)), 
             col.names = c("a", "$\\lambda$"), 
             caption = "Parameters from CV")

## -----------------------------------------------------------------------------
beta_hat <- lla(X, y, pentype, a = op_par[[1]], lambda = op_par[[2]])

## ---- eval=FALSE--------------------------------------------------------------
#  # Initialize the sequence of a and lambda
#  seq_a <- c(0.13, 0.15, 0.17)
#  seq_lambda <- c(0.11, 0.13, 0.15)
#  
#  # Penalty type
#  pentype <- "SICA"
#  
#  # Initialize the results containing PE, #S and FN
#  PE_sica_1 <- rep(0, sim_times)
#  S_sica_1 <- rep(0, sim_times)
#  FN_sica_1 <- rep(0, sim_times)
#  
#  # Do 100 times of simulations
#  for (i in 1:sim_times){
#    # Training set in each simulation
#    X <- train1_X[(sim_times*(i-1)+1):(sim_times*i),]
#    y <- train1_y[(sim_times*(i-1)+1):(sim_times*i),]
#  
#    # Search for optimal a and lambda using cross validation
#    op_res <- ms_cv(X,y,pentype,seq_a,seq_lambda)
#    op_a <- op_res[[1]]
#    op_lambda <- op_res[[2]]
#  
#    # Estimate beta using LLA methods
#    beta_hat <- lla(X,y,pentype,op_a,op_lambda)
#  
#    # Output results
#    S_sica_1[i] <- sum(beta_hat!=0)
#    FN_sica_1[i] <- sum(beta_hat[1:7] == 0)
#    PE_sica_1[i] <- mean((test1_y - test1_X %*% beta_hat) ^2)
#  }

## ---- echo=FALSE--------------------------------------------------------------
data(list = c("S_sica_1", "PE_sica_1", "FN_sica_1"))

## -----------------------------------------------------------------------------
knitr::kable(data.frame(t(c(median(S_sica_1), median(FN_sica_1), round(median(PE_sica_1), 4) )), 
                        row.names = "Median"), 
             col.names = c("#S", "FN", "PE"), caption = "Results of example")
boxplot(S_sica_1, main = "Boxplot of #S with SICA")
boxplot(PE_sica_1, main = "Boxplot of PE with SICA")

