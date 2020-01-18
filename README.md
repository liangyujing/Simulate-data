rm(list=ls())
set.seed(1000)
# 1. Simulate all the data
## 1.1. Simulate columns for Minimum acceptance level and digit ratio
### Number of participants for each condition
ns <- 95
ne <- 95
ny <- 95
### Mean and standard deviation of Minimum acceptance level
mus <- 2.73                  
mue <- 3.2                
muy <- 3.54             
sds <- 0.32                  
sde <- 0.33                
sdy <- 0.27
### Mean and sd of Digit ratio
mu_d <- 0.947                  
sd_d <- 0.029
### Correlations
rda_s <- .40
rda_e <- -.30
rda_y <- -0.36
### Draw data from normal distribution
#Sys.setlocale(locale="C")
sexy <- MASS::mvrnorm(ns, 
                c(mus, mu_d),
                matrix(c(sds^2,(rda_s)*sds*sd_d,(rda_s)*sds*sd_d,sd_d^2),2,2))
newsexy <- cbind(sexy[,2],sexy[,1])
plot(newsexy, 
     pch=21, 
     ylim=c(0, 6), xlim=c(0.85,1.05), 
     ylab="Minimum acceptance level", xlab="Digit ratio", main = "Sexy Condition",
     bg = "gold")

elderly <- MASS::mvrnorm(ne, 
                   c(mue, mu_d), 
                   matrix(c(sde^2,(rda_e)*sde*sd_d,(rda_e)*sde*sd_d,sd_d^2),2,2))
newelderly <- cbind(elderly[,2],elderly[,1])
plot(newelderly, 
     pch=21, 
     ylim=c(0, 6), xlim=c(0.85,1.05), 
     ylab="Minimum acceptance level", xlab="Digit ratio", main = "Elderly Condition",
     bg = "lightblue")

young <- MASS::mvrnorm(ny, 
                 c(muy, mu_d), 
                 matrix(c(sdy^2,(rda_y)*sdy*sd_d,(rda_y)*sdy*sd_d,sd_d^2),2,2))
newyoung <- cbind(young[,2],young[,1])
plot(newyoung, 
     pch=21, 
     ylim=c(0, 6), xlim=c(0.85,1.05), 
     ylab="Minimum acceptance level", xlab="Digit ratio", main = "Young Condition",
     bg = "lightgreen")

### Combine 3 conditions
combine_data <- rbind(sexy,elderly,young)
colnames(combine_data, do.NULL = FALSE) 
colnames(combine_data) <- c("Acceptance","DigitRatio")

## 1.2. Simulate picture conditions column
picture<-factor(c(rep("sexy",ns), rep("elderly",ne), rep("young",ny)))

## 1.3. Simulate subjects column
subjects <- factor(c(seq(1,ns), seq(1,ne), seq(1,ny)))

# 1.4. Simulate manipulation check: suppose a 9 point scale, mean comes from "Physical Attractiveness, Age, And Body Type (1988)", sd added our own
sexy_rating = rnorm(n = ns, mean = 6.35, sd = 1)
elderly_rating = rnorm(n = ne, mean = 4.40, sd = 1)
young_rating = rnorm(n = ny, mean = 4.02, sd = 1)

## Combine 3 conditions for column manipulation check
manipulation_check <- c(sexy_rating,elderly_rating,young_rating)

# 2. Combine ALL COLUMNS to create Simulated_Data data set
Simulated_Data_all <- cbind.data.frame(picture, subjects, combine_data, manipulation_check)
#write.table (Alldata,file = "Simulated_Data",sep=",") --> If you want to write output file for the simulated data

# 3.Simulate missing data + Exclude: assuming 5 of them failed attention check, 2 of them not complete experiments --> randomly remove 7 rows in total
Simulated_Data_exclude <- Simulated_Data_all[-sample(1:nrow(Simulated_Data_all), 7), ]
summary(Simulated_Data_exclude)
#write.table (Simulated_Data_exclude, file = "Simulated_Data_Exclude",sep=",") -->  If you want to output to a csv file 

# 4. Outliers: use boxplot to detect outliers
##acceptance outliers: none
boxplot(Simulated_Data_exclude$Acceptance, 
        main = 'boxplot acceptance level')
outliers_acceptance <- boxplot(Simulated_Data_exclude$Acceptance)$out
outliers_acceptance
##digit outliers: one that equals 1.03657952109468
boxplot(Simulated_Data_exclude$DigitRatio,
        main = 'boxplot digit ratio')
outliers_digit <- boxplot(Simulated_Data_exclude$DigitRatio)$out
outliers_digit
## Remove outlier(s) from dataset
Simulated_Data <- Simulated_Data_exclude[-which(Simulated_Data_exclude$Acceptance %in% outliers_acceptance),]
Simulated_Data <- Simulated_Data[-which(Simulated_Data_exclude$DigitRatio %in% outliers_digit),]
#write.table (Simulated_Data.new,file = "Simulated_Data_withoutoutlier",sep=",") --> if want to write csv file

# Make variables (easier further calcualtions)
picture <- Simulated_Data$picture
acceptance <- Simulated_Data$Acceptance
digit <-Simulated_Data$Digit
manipulation_check <- Simulated_Data$manipulation_check
```
