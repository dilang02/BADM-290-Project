rm(list=ls())
graphics.off() # Clear workspace and import datasets
library(readxl)
dg_data <- read_excel("D:/My Drive/Spring 2023 Semester/BADM-290  Global Business Experience/EMBA Finca Flichman Results.xlsx", 
                      sheet = "Analysis", range = "F1:N40")
dg_data <- as.matrix(dg_data) # Transform database into numeric matrix
for (i in 1:3){
  cat("Variable",i,":\n")
  show(summary(dg_data[,i]))
}
for (i in 4:7) { # Create histograms and report summary data for experimental data
  hist(dg_data[,i],breaks=25)
  cat("Model",i-3,":\n")
  show(summary(dg_data[,i]))
}
for (i in 8:9){ # Create histograms and report summary data for demographic data
  if (i == 8){
    cat("Age:\n")
    hist(dg_data[,i],breaks=25,main="Age")
  }
  else{
    cat("Household Income:\n")
    hist(dg_data[,i],breaks=50,main="Household Income")
  }
  show(summary(dg_data[,i]))
}

hist(dg_data[,5]-dg_data[,4],breaks=25,main="Experiment 1: Bivariate Difference") # Visualize experiment hypotheses
show(t.test(dg_data[,5],dg_data[,4],paired=TRUE,alternative="greater")) # Report statistical analyses of hypotheses
hist(dg_data[,7]-dg_data[,6],breaks=25,main="Experiment 2: Bivariate Difference")
show(t.test(dg_data[,7],dg_data[,6],paired=TRUE,alternative="greater"))

w_sim <- c() # Optimizing corrective transformation for Shapiro-Wilk testing
for (i in 1:9){
  for (w in 1:100){
    y_new <- (dg_data[,i])^(w/100)
    w_sim <- append(w_sim,shapiro.test(y_new)$p.value)
  }
}
w_matrix <- matrix(w_sim,100,9)
w_max <- c()
for (i in 1:9){
  #plot(c(1:100),w_matrix[,i])
  w_max <- append(w_max,which.max(w_matrix[,i])/100)
  #abline(v=w_max[i]*100)
}

hist(dg_data[,1],breaks=25) # Create histograms for consumer data
hist(dg_data[,2])
hist(dg_data[,3],breaks=25)

p_results <- c() # Determine which interactions are significant
for (i in 1:9){
  for (j in 1:9){
    p_results <- append(p_results,summary(lm(dg_data[,i]~dg_data[,j]))$coefficients[2,4])
  }
}
p_matrix <- matrix(p_results,9,9)
round(p_matrix,2)
sig_test <- p_matrix < 0.05
sig_test

for (i in 1:9){ # Normalize residuals and optimize regression through polynomial testing
  for (j in 1:9){ # Create plot/fit line for each significant interaction
    if (p_matrix[i,j] < 0.05 & i != j){
      y_max <- (dg_data[,j])^(w_max[j])
      title <- paste("[",i,j,"]")
      print(title)
      plot(dg_data[,i],y_max,main=paste("Regression Model: [",i,j,"]"),xlab=paste("Predictor: Column",i),ylab=paste("Dependent: Column",j)) # Determine significant correlation
      x_2 <- dg_data[,i]^2
      x_3 <- dg_data[,i]^3
      if (summary(lm(y_max~dg_data[,i]+x_2+x_3))$coefficients[4,4] < 0.05){
        abline(lm(y_max~dg_data[,i]+x_2+x_3),col="red")
        #plot(lm(y_max~dg_data[,j]+x_2+x_3),which=1)
        show(summary(lm(y_max~dg_data[,i]+x_2+x_3)))
      } else if (summary(lm(y_max~dg_data[,i]+x_2))$coefficients[3,4] < 0.05) {
        abline(lm(y_max~dg_data[,i]+x_2),col="red")
        #plot(lm(y_max~dg_data[,j]+x_2),which=1)
        show(summary(lm(y_max~dg_data[,i]+x_2)))
      } else {
        abline(lm(y_max~dg_data[,i]),col="red")
        #plot(lm(y_max~dg_data[,j]),which=1)
        show(summary(lm(y_max~dg_data[,i])))
      }
      
    }
  }
}

prediction <- function(x,i,j){ # Create extrapolation algorithm for user
  y_max <- (dg_data[,j])^(w_max[j])
  x_2 <- dg_data[,i]^2
  x_3 <- dg_data[,i]^3
  if (summary(lm(y_max~dg_data[,i]+x_2+x_3))$coefficients[4,4] < 0.05){
    fit <- lm(y_max~dg_data[,i]+x_2+x_3)
    result <- (coef(fit)[1]+(coef(fit)[2]*x)+(coef(fit)[3]*x^2)+(coef(fit)[4]*x^3))^(1/w_max[j])
  } else if (summary(lm(y_max~dg_data[,i]+x_2))$coefficients[3,4] < 0.05) {
    fit <- lm(y_max~dg_data[,i]+x_2)
    result <- (coef(fit)[1]+(coef(fit)[2]*x)+(coef(fit)[3]*x^2))^(1/w_max[j])
  } else {
    fit <- lm(y_max~dg_data[,i])
    result <- (coef(fit)[1]+(coef(fit)[2]*x))^(1/w_max[j])
  }
  
  if(p_matrix[i,j] < 0.05){
    result_sig <- "95% SIGNIFICANT"
  } else if(p_matrix[i,j] < 0.1){
    result_sig <- "90% SIGNIFICANT"
  } else {
    result_sig <- "INSIGNIFICANT"
  }
  show(fit)
  rsq <- summary(fit)$adj.r.squared
  paste("Predicted Y-value:",round(result,2),"| Adj. R^2:",round(rsq*100,2),"% |",result_sig)
}
