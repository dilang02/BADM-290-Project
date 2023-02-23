graphics.off() # Clear workspace and import datasets
library(readxl)
data <- read_excel("D:/My Drive/Spring 2023 Semester/BADM-290  Global Business Experience/MTurk Finca Flichman Results.xlsx",sheet = "Experiments")
#View(data)
dg_data <- read_excel("D:/My Drive/Spring 2023 Semester/BADM-290  Global Business Experience/MTurk Finca Flichman Results.xlsx", 
sheet = "Analysis", range = "G1:O102")
data.m <- as.matrix(data) # Transform database into numeric matrix
dg_data <- as.matrix(dg_data)
data.m_opt <- data.m[-101,] # Omit outlier and NA data points
na.omit(data.m_opt)
for (i in 1:4) { # Create histograms and report summary data for experimental data
  hist(data.m_opt[,i],breaks=25)
  cat("Model",i,":\n")
  show(summary(data.m_opt[,i]))
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

hist(data.m_opt[,2]-data.m_opt[,1],breaks=25,main="Experiment 1: Bivariate Difference") # Visualize experiment hypotheses
show(t.test(data.m_opt[,1],data.m_opt[,2],paired=TRUE,alternative="less")) # Report statistical analyses of hypotheses
hist(data.m_opt[,4]-data.m_opt[,3],breaks=25,main="Experiment 2: Bivariate Difference")
show(t.test(data.m_opt[,3],data.m_opt[,4],paired=TRUE,alternative="two.sided"))

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

for (i in 1:9){ # Normalize residuals and optimize regression through polynomial testing
  for (j in 1:9){ # Create plot/fit line for each significant interaction
    if (p_matrix[i,j] < 0.05 & i != j){
      y_max <- (dg_data[,i])^(w_max[i])
      plot(dg_data[,j],y_max,main=cat("[",i,j,"]")) # Determine significant correlation
      x_2 <- dg_data[,j]^2
      x_3 <- dg_data[,j]^3
      if (summary(lm(y_max~dg_data[,j]+x_2+x_3))$coefficients[4,4] < 0.05){
        abline(lm(y_max~dg_data[,j]+x_2+x_3),col="red")
        plot(lm(y_max~dg_data[,j]+x_2+x_3),which=1)
        show(summary(lm(y_max~dg_data[,j]+x_2+x_3)))
      } else if (summary(lm(y_max~dg_data[,j]+x_2))$coefficients[3,4] < 0.05) {
        abline(lm(y_max~dg_data[,j]+x_2),col="red")
        plot(lm(y_max~dg_data[,j]+x_2),which=1)
        show(summary(lm(y_max~dg_data[,j]+x_2)))
      } else {
        abline(lm(y_max~dg_data[,j]),col="red")
        plot(lm(y_max~dg_data[,j]),which=1)
        show(summary(lm(y_max~dg_data[,j])))
      }

    }
  }
}
