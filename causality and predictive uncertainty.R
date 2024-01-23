install.packages("simcausal")
library(ranger)
library(randomForest)
library(mlbench)
#library("caret")
library(e1071)
library(tidyverse)
library(simcausal)
library(Metrics)
library(knitr)
library(wesanderson)
library(partykit)
library(party)
library(ggpubr)
library(corrplot)
library(correlation)
library(psych)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(mgcv)
library(tidymv)

######## Example for model misspecification ######
X <- rnorm(1000, 2, 3)
Y <- cos(0.5*X)
plot(X, Y)
data <- as.data.frame(cbind(X, Y))

###### visualisieren ####
model_misspecification <- ggplot(data, aes(X, Y)) + geom_point()+
  geom_smooth(method='lm', formula = y ~ x) +
  ggtitle("Relationship between X and Y")+
  xlab("X") + ylab("Y") + 
  theme(axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        axis.text=element_text(size = 24),
        plot.title = element_text(size = 28, face = "bold"))
model_misspecification
## fit the model
mod_lm <- lm(Y~X)
summary(mod_lm)

##### Residual vs Fitted Plot ####
plot(mod_lm, which = 1, cex.axis = 1, cex.caption = 1.3,
     cex = 1, cex.lab = 1.1, cex.sub = 0.5, sub = "", mgp= c(2.5,0.8,0))



################################ Experiment ####################################
#### create a vector of the exogenous variable house_price that
############## Data Simulation #################
#### create exogenous variable house_price that is not associated with any other variables ########
# Set seed for reproducibility
set.seed(122)
# Generate synthetic data with predictor variables
num_samples <- 1000
living_area <- rnorm(num_samples, mean = 120, sd = 20)
num_rooms <- rnorm(num_samples, mean = 4, sd = 1)
floor <- sample(1:10, num_samples, replace = TRUE)
location_factor <- rnorm(num_samples, mean = 0, sd = 1)
# Simulate house prices using a linear regression model
house_prices <- 500000 + 2000 * living_area + 30000 * num_rooms +
  5000 * floor + 10000 * location_factor + rnorm(num_samples, mean = 0, sd = 50000)
house_prices




######### simulate data with 6 variables ##############
D <- DAG.empty()
set.seed(100)
D <- D +
  node("SleepQuality", # Confounder variable C
       distr = "rnorm", mean = 50, sd = 5) +
  node("Intelligence", # exposure variable
       distr = "rnorm", mean = 1.2 * SleepQuality + 50 , sd = 5) +
  node("StudyTime", # Mediator
       distr = "rnorm" , mean = - 0.3 * Intelligence + 50, sd = 4) +
  node("ExamDifficulty", # another exposure
       distr = "rcat.b1", probs = c(0.05, 0.05, 0.1, 0.15, 0.2, 0.3, 0.1, 0.025, 0.025)) +
  node("ExamPerformance", # Outcome Variable 
       distr = "rnorm", mean = ((27 * StudyTime + 1.2 * Intelligence +
                                   22 * SleepQuality + 50 * as.numeric(ExamDifficulty)) / 3 ) - 200, sd = 1) + 
  node("Confidence", #collider
       distr = "rnorm", mean = 0.3 * ExamPerformance +
         1.2 * Intelligence, sd = 10) 

D <- set.DAG(D)
dat1 <- sim(D, n = 1000)
dat1$ExamDifficulty <- as.numeric(dat1$ExamDifficulty)
#### combine simulated data with vector house_price ####
dat1 <- cbind(dat1, house_prices)
summary(dat1)

## see correlation between variables in data
cor(dat1, use="complete.obs")



##### ExamPerformance classify ####
dat <- dat1
dat$EPClass <- dat$ExamPerformance
dat$EPClass[dat$EPClass < 410.8] <- 4
dat$EPClass[dat$EPClass >= 410.8 & dat$EPClass < 447.5 ] <- 3
dat$EPClass[dat$EPClass >= 447.5 & dat$EPClass < 483.8 ] <- 2
dat$EPClass[dat$EPClass >= 483.8] <- 1
dat$EPClass <- as.factor(dat$EPClass)
#### correlation plot ####

colnames(dat1) <- c("ID", "Sleep Quality", "Intelligence", "Study Time",
                    "Exam Difficulty", "Exam Performance", "Confidence",
                    "House Price")
p.mat <- cor.mtest(dat1[-1])
head(p.mat$p)

corrplot(cor(dat1[-1]),
         method = "square", outline = T, addgrid.col = "darkgray",
         type = "lower",
         p.mat = p.mat$p, sig.level = 0.01,mar = c(0,0,2,1),
         tl.cex = 0.8, cex.main = 1)


#### create the catagorical variable EPClass in data by classifying exam performance in 4 classes ####

### see data structure
summary(dat)



################## create DAGs ############
####  DAG with 3 nodes: I, ED and EP ####
plotDAG(D, xjitter = 0.05, yjitter = 0.05, edge_attrs = 
          list(width = 1.2, arrow.width = 1, arrow.size = 0.5),
        vertex_attrs = list(size = 80, label.cex = 1.4),
        excludeattrs = c("SleepQuality", "StudyTime", "Confidence"),
        customvlabs = c("Intelligence", "Exam Difficulty",
                        "Exam Performance"))

####  DAG with 3 nodes: SQ, I and EP ####
plotDAG(D, xjitter = 0.05, yjitter = 0.05, edge_attrs = 
          list(width = 1.2, arrow.width = 1, arrow.size = 0.5),
        vertex_attrs = list(size = 120, label.cex = 2.5),
        excludeattrs = c("StudyTime","ExamDifficulty", "Confidence"),
        customvlabs = c ("Sleep Quality (C)", "Intelligence (X)",
                         "Exam Performance (Y)"))
####  DAG with 3 nodes: ED, I and EP ####
plotDAG(D, xjitter = 0.05, yjitter = 0.05, edge_attrs = 
          list(width = 1.2, arrow.width = 1, arrow.size = 0.5),
        vertex_attrs = list(size = 120, label.cex = 2.5),
        excludeattrs = c ("StudyTime","SleepQuality", "Confidence"),
        customvlabs = c ("Intelligence (X)","Exam Difficulty (Y)", 
                         "Exam Performance (Z)"))

####  DAG with 3 nodes: ED, I and EP ####
plotDAG(D, xjitter = 0.05, yjitter = 0.05, edge_attrs = 
          list(width = 1.2, arrow.width = 1, arrow.size = 0.5),
        vertex_attrs = list(size = 120, label.cex = 2.5),
        excludeattrs = c ("ExamDifficulty","SleepQuality", "Confidence"),
        customvlabs = c ("Intelligence (X)","Study Time (M)", 
                         "Exam Performance (Y)"))
####  DAG with all 6 nodes ####
plotDAG(D, xjitter = 0.6, yjitter = 0.3, edge_attrs = 
          list(width = 1.5, arrow.width = 1, arrow.size = 0.4),
        vertex_attrs = list(size = 65, label.cex = 1.5),
        customvlabs = c("Sleep Quality (C)","Intelligence (X1)","Study Time (M)",
                        "Exam Difficulty (X2)", "Exam Performance (Y)", "Confidence (Z)"))







################################ Build Random Forest Models ########################

### split data to training and test data ####
set.seed(101)
train_indices <- sample(nrow(dat), 0.7 * nrow(dat))
train_data <- dat[train_indices, ]
test_data <- dat[-train_indices, ]

### Find optimal mtry ######
mtry <- tuneRF(dat[-c(1, 6, 7, 8, 9)], dat$EPClass, ntreeTry = 500,
               stepFactor = 0.5, improve = 0.05, trace = TRUE, plot = TRUE)
print(mtry)

###### 1. initial rf model(X = (Intelligence, StudyTime, SleepQuality, ExamDifficulty)) ####
set.seed(123)
rf_model <- randomForest(EPClass ~ StudyTime + Intelligence +
                           SleepQuality + ExamDifficulty,
                         data = train_data, ntree = 500, mtry = 2,
                         importance = TRUE)
oob_rf_model <- mean(rf_model$err.rate[, 1])
print(rf_model)
plot(rf_model)
summary(rf_model)

predction_rf<- predict(rf_model, newdata = test_data,
                       type = "prob", predict.all = TRUE)
preds_rf <- predction_rf[["aggregate"]]

###### 2. rf_c model without confounder ########
rf_model_c <- randomForest(EPClass ~ StudyTime + Intelligence + ExamDifficulty,
                         data = train_data, ntree = 500, mtry = 2,
                         importance = TRUE)
oob_rf_model_c <- mean(rf_model_c$err.rate[, 1])

predction_rf_c <- predict(rf_model_c, newdata = test_data,
                       type = "prob", predict.all = TRUE)
preds_rf_c <- predction_rf_c[["aggregate"]]

###### 3. rf_ed model without ExamDifficulty ####
rf_model_ed <- randomForest(EPClass ~ SleepQuality + Intelligence + StudyTime,
                           data = train_data, ntree = 500, mtry = 2,
                           importance = TRUE)
oob_rf_model_ed <- mean(rf_model_ed$err.rate[, 1])

predction_rf_ed <- predict(rf_model_ed, newdata = test_data,
                          type = "prob", predict.all = TRUE)
preds_rf_ed <- predction_rf_ed[["aggregate"]]
###### 4. rf_m model without mediator ####
rf_model_m <- randomForest(EPClass ~ SleepQuality + Intelligence + ExamDifficulty,
                           data = train_data, ntree = 500, mtry = 2,
                           importance = TRUE)
oob_rf_model_m <- mean(rf_model_m$err.rate[, 1])

predction_rf_m <- predict(rf_model_m, newdata = test_data,
                          type = "prob", predict.all = TRUE)
preds_rf_m <- predction_rf_m[["aggregate"]]

###### 5. rf_confi model with confidence ####
rf_model_confi <- randomForest(EPClass ~ SleepQuality + Intelligence + ExamDifficulty +
                                 Confidence + StudyTime,
                           data = train_data, ntree = 500, mtry = 2,
                           importance = TRUE)
oob_rf_model_confi <- mean(rf_model_confi$err.rate[, 1])

predction_rf_confi <- predict(rf_model_confi, newdata = test_data,
                          type = "prob", predict.all = TRUE)
preds_rf_confi <- predction_rf_confi[["aggregate"]]

###### 6. rf_h model with house_prices ####
rf_model_h <- randomForest(EPClass ~ StudyTime + SleepQuality + Intelligence +
                             ExamDifficulty + house_prices,
                               data = train_data, ntree = 500, mtry = 2,
                               importance = TRUE)
oob_rf_model_h <- mean(rf_model_h$err.rate[, 1])

predction_rf_h <- predict(rf_model_h, newdata = test_data,
                              type = "prob", predict.all = TRUE)
preds_rf_h <- predction_rf_h[["aggregate"]]

###### 7. rf_h_only model with only house_prices ####
rf_model_h_only <- randomForest(EPClass ~  house_prices,
                           data = train_data, ntree = 500, mtry = 1,
                           importance = TRUE)
oob_rf_model_h_only <- mean(rf_model_h_only$err.rate[, 1])

predction_rf_h_only <- predict(rf_model_h_only, newdata = test_data,
                          type = "prob", predict.all = TRUE)
preds_rf_h_only <- predction_rf_h_only[["aggregate"]]
###### 8. rf_all ####
rf_model_all <- randomForest(EPClass ~ StudyTime + SleepQuality + Intelligence +
                             ExamDifficulty + house_prices + Confidence,
                           data = train_data, ntree = 500, mtry = 2,
                           importance = TRUE)
oob_rf_model_all <- mean(rf_model_all$err.rate[, 1])

predction_rf_all <- predict(rf_model_all, newdata = test_data,
                          type = "prob", predict.all = TRUE)
preds_rf_all <- predction_rf_all[["aggregate"]]
###### 9. rf_h model with house_prices instead of ed ####
rf_model_h_insteadED <- randomForest(EPClass ~ StudyTime + SleepQuality +
                                       Intelligence + house_prices,
                           data = train_data, ntree = 500, mtry = 2,
                           importance = TRUE)
oob_rf_model_h_insteadED <- mean(rf_model_h_insteadED$err.rate[, 1])

predction_rf_h_insteadED <- predict(rf_model_h_insteadED, newdata = test_data,
                          type = "prob", predict.all = TRUE)
preds_rf_h_insteadED <- predction_rf_h_insteadED[["aggregate"]]

############################# rf uncertainty quantification using entropy ##################
###### 1. initial rf model ####
# Entropie (Konvention: log zur Basis 2)
compute_entropy <- function(x) -sum(x[x != 0] * log(x[x != 0], base = 2))
total_uncertainty_rf <- vector(mode = "numeric", length = nrow(test_data))
  
  for (i in 1:nrow(test_data)) {
    total_uncertainty_rf[i] <- compute_entropy(preds_rf[i, ])
  }
total_uncertainty_rf
sum(total_uncertainty_rf)

###### 2. entropy of rf model with out confounder ####
total_uncertainty_rf_c <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  total_uncertainty_rf_c[i] <- compute_entropy(preds_rf_c[i, ])
}
total_uncertainty_rf_c
sum(total_uncertainty_rf_c)

###### 3. entropy of rf model without mediator ####
total_uncertainty_rf_m <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  total_uncertainty_rf_m[i] <- compute_entropy(preds_rf_m[i, ])
}
total_uncertainty_rf_m
sum(total_uncertainty_rf_m)

###### 4. entropy of rf model without ExamDifficulty ####
total_uncertainty_rf_ed <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  total_uncertainty_rf_ed[i] <- compute_entropy(preds_rf_ed[i, ])
}
total_uncertainty_rf_ed
sum(total_uncertainty_rf_ed)

###### 5. entropy of rf model with confidence ####
total_uncertainty_rf_confi <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  total_uncertainty_rf_confi[i] <- compute_entropy(preds_rf_confi[i, ])
}
total_uncertainty_rf_confi
sum(total_uncertainty_rf_confi)

###### 6. entropy of rf model with house_price ####
total_uncertainty_rf_h <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  total_uncertainty_rf_h[i] <- compute_entropy(preds_rf_h[i, ])
}
total_uncertainty_rf_h
sum(total_uncertainty_rf_h)

###### 7. entropy of rf model with only house_price ####
total_uncertainty_rf_h_only <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  total_uncertainty_rf_h_only[i] <- compute_entropy(preds_rf_h_only[i, ])
}
total_uncertainty_rf_h_only
sum(total_uncertainty_rf_h_only)

###### 8. entropy of rf model all ####
total_uncertainty_rf_all <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  total_uncertainty_rf_all[i] <- compute_entropy(preds_rf_all[i, ])
}
total_uncertainty_rf_all
sum(total_uncertainty_rf_all)
###### 9. entropy of rf model with house_price instead of ed ####
total_uncertainty_rf_h_insteadED <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  total_uncertainty_rf_h_insteadED[i] <- compute_entropy(preds_rf_h_insteadED[i, ])
}
total_uncertainty_rf_h_insteadED
sum(total_uncertainty_rf_h_insteadED)


############################# random forest model using ranger #######################
###### 1. initial rg model (X = (Intelligence, StudyTime, SleepQuality, ExamDifficulty)) ####
rg <- ranger(
  EPClass ~ StudyTime + Intelligence + SleepQuality + ExamDifficulty, 
  data = train_data, 
  num.trees = 500, 
  mtry = 2,
  probability = TRUE # nur hier fuers Beispiel, damit probabilities < 1 rauskommen
)
print(rg)
### 
pred_dat <- predict(rg, data = test_data, predict.all = TRUE)
preds <- pred_dat[["predictions"]]
View(preds)

###### 2. rg_c model without confounder ####
rg_c <- ranger(
  EPClass ~ StudyTime + Intelligence + ExamDifficulty, 
  data = train_data, 
  num.trees = 500, 
  mtry = 2,
  probability = TRUE # nur hier fuers Beispiel, damit probabilities < 1 rauskommen
)
print(rg_c)

pred_dat_c <- predict(rg_c, data = test_data, predict.all = TRUE)
preds_c <- pred_dat_c[["predictions"]]

###### 3. rg_m model without mediator ####
rg_m <- ranger(
  EPClass ~ SleepQuality + Intelligence + ExamDifficulty, 
  data = train_data, 
  num.trees = 500, 
  mtry = 2,
  probability = TRUE
)
print(rg_m)

pred_dat_m <- predict(rg_m, data = test_data, predict.all = TRUE)
preds_m <- pred_dat_m[["predictions"]]

###### 4. rg_ed model without ExamDifficulty ####
rg_ed <- ranger(
  EPClass ~ SleepQuality + Intelligence + StudyTime, 
  data = train_data, 
  num.trees = 500, 
  mtry = 2,
  probability = TRUE
)
print(rg_ed)

pred_dat_ed <- predict(rg_ed, data = test_data, predict.all = TRUE)
preds_ed <- pred_dat_ed[["predictions"]]


###### 5. rg_confi model with confidence ####
rg_confi <- ranger(
  EPClass ~ StudyTime + Intelligence + ExamDifficulty +
    Confidence + SleepQuality, 
  data = train_data, 
  num.trees = 500, 
  mtry = 2,
  probability = TRUE
)
print(rg_confi)

pred_dat_confi <- predict(rg_confi, data = test_data, predict.all = TRUE)
preds_confi <- pred_dat_confi[["predictions"]]
###### 6. rg_h model with house_price ####
rg_h <- ranger(
  EPClass ~ StudyTime + Intelligence + ExamDifficulty +
    house_prices + SleepQuality, 
  data = train_data, 
  num.trees = 500, 
  mtry = 2,
  probability = TRUE
)
print(rg_h)

pred_dat_h<- predict(rg_h, data = test_data, predict.all = TRUE)
preds_h <- pred_dat_h[["predictions"]]
###### 7. rg_h model only with house_price ####
rg_h_only <- ranger(
  EPClass ~ house_prices, 
  data = train_data, 
  num.trees = 500, 
  mtry = 1,
  probability = TRUE
)
print(rg_h_only)

pred_dat_h_only<- predict(rg_h_only, data = test_data, predict.all = TRUE)
preds_h_only <- pred_dat_h_only[["predictions"]]
###### 8. rg_confi model with confidence ####
rg_h_insteadED <- ranger(
  EPClass ~ StudyTime + Intelligence + house_prices + SleepQuality, 
  data = train_data, 
  num.trees = 500, 
  mtry = 2,
  probability = TRUE
)
print(rg_h_insteadED)

pred_dat_h_insteadED <- predict(rg_h_insteadED, data = test_data, predict.all = TRUE)
preds_h_insteadED<- pred_dat_h_insteadED[["predictions"]]

################# rg uncertainty quantification using entropy ##################
####### 1. initial model ####
# Entropie (Konvention: log zur Basis 2)
compute_entropy <- function(x) -sum(x[x != 0] * log(x[x != 0], base = 2))
total_uncertainty <- vector(mode = "numeric", length = nrow(test_data))
aleatoric_uncertainty <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
# 1. total uncertainty: entropy der average prediction
avg_pred <- rowMeans(preds[i, , ])
total_uncertainty[i] <- compute_entropy(avg_pred)

# 2. aleatoric uncertainty: average entropy der einzelnen predictions
individual_entropies  <- apply(preds[i, , ], 2, compute_entropy)
aleatoric_uncertainty[i]  <- mean(individual_entropies)
}

# 3. epistemic uncertainty = total_uncertainty - aleatoric_uncertainty
epistemic_uncertainty  <- total_uncertainty - aleatoric_uncertainty

total_uncertainty
aleatoric_uncertainty
epistemic_uncertainty

### create a dataframe with all tree uncertainties
uncertainties <- as.data.frame(cbind(aleatoric_uncertainty,
                    epistemic_uncertainty,
                    total_uncertainty))

(sum(aleatoric_uncertainty) + sum(epistemic_uncertainty)) == sum(total_uncertainty)
anteil_a <- sum(aleatoric_uncertainty) / sum(total_uncertainty)
anteil_e <- sum(epistemic_uncertainty) / sum(total_uncertainty)

####### 2. model without confounder ####
# Entropie (Konvention: log zur Basis 2)
total_uncertainty_c <- vector(mode = "numeric", length = nrow(test_data))
aleatoric_uncertainty_c <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  # 1. total uncertainty: entropy der average prediction
  avg_pred_c <- rowMeans(preds_c[i, , ])
  total_uncertainty_c[i] <- compute_entropy(avg_pred_c)
  
  # 2. aleatoric uncertainty: average entropy der einzelnen predictions
  individual_entropies_c  <- apply(preds_c[i, , ], 2, compute_entropy)
  aleatoric_uncertainty_c[i]  <- mean(individual_entropies_c)
}

# 3. epistemic uncertainty = total_uncertainty - aleatoric_uncertainty
epistemic_uncertainty_c  <- total_uncertainty_c - aleatoric_uncertainty_c

total_uncertainty_c
aleatoric_uncertainty_c
epistemic_uncertainty_c

### create a dataframe with all tree uncertainties 
uncertainties_c <- as.data.frame(cbind(aleatoric_uncertainty_c,
                                     epistemic_uncertainty_c,
                                     total_uncertainty_c))

(sum(aleatoric_uncertainty_c) + sum(epistemic_uncertainty_c)) == sum(total_uncertainty_c)
anteil_a_c <- sum(aleatoric_uncertainty_c) / sum(total_uncertainty_c)
anteil_e_c <- sum(epistemic_uncertainty_c) / sum(total_uncertainty_c)
anteil_a_c + anteil_e_c 

####### 3. model without mediator ####
# Entropie (Konvention: log zur Basis 2)
total_uncertainty_m <- vector(mode = "numeric", length = nrow(test_data))
aleatoric_uncertainty_m <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  # 1. total uncertainty: entropy der average prediction
  avg_pred_m <- rowMeans(preds_m[i, , ])
  total_uncertainty_m[i] <- compute_entropy(avg_pred_m)
  
  # 2. aleatoric uncertainty: average entropy der einzelnen predictions
  individual_entropies_m  <- apply(preds_m[i, , ], 2, compute_entropy)
  aleatoric_uncertainty_m[i]  <- mean(individual_entropies_m)
}

# 3. epistemic uncertainty = total_uncertainty - aleatoric_uncertainty
epistemic_uncertainty_m  <- total_uncertainty_m - aleatoric_uncertainty_m

total_uncertainty_m
aleatoric_uncertainty_m
epistemic_uncertainty_m

### create a dataframe with all tree uncertainties
uncertainties_m <- as.data.frame(cbind(aleatoric_uncertainty_m,
                                       epistemic_uncertainty_m,
                                       total_uncertainty_m))

anteil_a_m <- sum(aleatoric_uncertainty_m) / sum(total_uncertainty_m)
anteil_e_m <- sum(epistemic_uncertainty_m) / sum(total_uncertainty_m)
anteil_a_m + anteil_e_m

####### 4. model without ExamDifficulty ####
total_uncertainty_ed <- vector(mode = "numeric", length = nrow(test_data))
aleatoric_uncertainty_ed <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  # 1. total uncertainty: entropy der average prediction
  avg_pred_ed <- rowMeans(preds_ed[i, , ])
  total_uncertainty_ed[i] <- compute_entropy(avg_pred_ed)
  
  # 2. aleatoric uncertainty: average entropy der einzelnen predictions
  individual_entropies_ed  <- apply(preds_ed[i, , ], 2, compute_entropy)
  aleatoric_uncertainty_ed[i]  <- mean(individual_entropies_ed)
}

# 3. epistemic uncertainty = total_uncertainty - aleatoric_uncertainty
epistemic_uncertainty_ed  <- total_uncertainty_ed - aleatoric_uncertainty_ed

total_uncertainty_ed
aleatoric_uncertainty_ed
epistemic_uncertainty_ed

### create a dataframe with all tree uncertainties
uncertainties_ed <- as.data.frame(cbind(aleatoric_uncertainty_ed,
                                        epistemic_uncertainty_ed,
                                        total_uncertainty_ed))

anteil_a_ed <- sum(aleatoric_uncertainty_ed) / sum(total_uncertainty_ed)
anteil_e_ed <- sum(epistemic_uncertainty_ed) / sum(total_uncertainty_ed)
anteil_a_ed + anteil_e_ed
####### 5. model with confidence ####
# Entropie (Konvention: log zur Basis 2)
total_uncertainty_confi <- vector(mode = "numeric", length = nrow(test_data))
aleatoric_uncertainty_confi <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  # 1. total uncertainty: entropy der average prediction
  avg_pred_confi <- rowMeans(preds_confi[i, , ])
  total_uncertainty_confi[i] <- compute_entropy(avg_pred_confi)
  
  # 2. aleatoric uncertainty: average entropy der einzelnen predictions
  individual_entropies_confi  <- apply(preds_confi[i, , ], 2, compute_entropy)
  aleatoric_uncertainty_confi[i]  <- mean(individual_entropies_confi)
}

# 3. epistemic uncertainty = total_uncertainty - aleatoric_uncertainty
epistemic_uncertainty_confi  <- total_uncertainty_confi - aleatoric_uncertainty_confi

total_uncertainty_confi
aleatoric_uncertainty_confi
epistemic_uncertainty_confi


####### 6. model with house_price ####
total_uncertainty_h <- vector(mode = "numeric", length = nrow(test_data))
aleatoric_uncertainty_h <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  # 1. total uncertainty: entropy der average prediction
  avg_pred_h <- rowMeans(preds_h[i, , ])
  total_uncertainty_h[i] <- compute_entropy(avg_pred_h)
  
  # 2. aleatoric uncertainty: average entropy der einzelnen predictions
  individual_entropies_h  <- apply(preds_h[i, , ], 2, compute_entropy)
  aleatoric_uncertainty_h[i]  <- mean(individual_entropies_h)
}

# 3. epistemic uncertainty = total_uncertainty - aleatoric_uncertainty
epistemic_uncertainty_h  <- total_uncertainty_h - aleatoric_uncertainty_h

total_uncertainty_h
aleatoric_uncertainty_h
epistemic_uncertainty_h
####### 7. model with only house_price ####
total_uncertainty_h_only <- vector(mode = "numeric", length = nrow(test_data))
aleatoric_uncertainty_h_only <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  # 1. total uncertainty: entropy der average prediction
  avg_pred_h_only <- rowMeans(preds_h_only[i, , ])
  total_uncertainty_h_only[i] <- compute_entropy(avg_pred_h_only)
  
  # 2. aleatoric uncertainty: average entropy der einzelnen predictions
  individual_entropies_h_only  <- apply(preds_h_only[i, , ], 2, compute_entropy)
  aleatoric_uncertainty_h_only[i]  <- mean(individual_entropies_h_only)
}
epistemic_uncertainty_h_only = total_uncertainty_h_only - aleatoric_uncertainty_h_only
####### 8. model with house_price ####
total_uncertainty_h_insteadED <- vector(mode = "numeric", length = nrow(test_data))
aleatoric_uncertainty_h_insteadED  <- vector(mode = "numeric", length = nrow(test_data))

for (i in 1:nrow(test_data)) {
  # 1. total uncertainty: entropy der average prediction
  avg_pred_h_insteadED  <- rowMeans(preds_h_insteadED [i, , ])
  total_uncertainty_h_insteadED [i] <- compute_entropy(avg_pred_h_insteadED )
  
  # 2. aleatoric uncertainty: average entropy der einzelnen predictions
  individual_entropies_h_insteadED   <- apply(preds_h_insteadED [i, , ], 2, compute_entropy)
  aleatoric_uncertainty_h_insteadED [i]  <- mean(individual_entropies_h_insteadED )
}

# 3. epistemic uncertainty = total_uncertainty - aleatoric_uncertainty
epistemic_uncertainty_h_insteadED   <- total_uncertainty_h_insteadED  - aleatoric_uncertainty_h_insteadED 

total_uncertainty_h_insteadED 
aleatoric_uncertainty_h_insteadED 
epistemic_uncertainty_h_insteadED 

#################### table of rf total uncertainty ########
table_rf <- data.frame(col1 = c(mean(total_uncertainty_rf),
                                mean(total_uncertainty_rf_c),
                                mean(total_uncertainty_rf_m),
                                mean(total_uncertainty_rf_ed),
                                mean(total_uncertainty_rf_confi),
                                mean(total_uncertainty_rf_h),
                                mean(total_uncertainty_rf_h_only),
                                mean(total_uncertainty_rf_all),
                                mean(total_uncertainty_rf_h_insteadED)),
                      col2 = c(oob_rf_model,
                               oob_rf_model_c,
                               oob_rf_model_m,
                               oob_rf_model_ed,
                               oob_rf_model_confi,
                               oob_rf_model_h,
                               oob_rf_model_h_only,
                               oob_rf_model_all,
                               oob_rf_model_h_insteadED))
colnames(table_rf) <- c("average_total_uncertainty", "average_oob")
rownames(table_rf) <- c('rf', 'rf_c', 'rf_m','rf_ed', 'rf_confi', 'rf_h',
                        'rf_h_only', 'rf_all', 'rf_h_insteadED')
table_rf
result_rf <- as.data.frame(table_rf)
result_rf$model <- rownames(table_rf)
#### barplot rf uncertainties ####
barplot(height = table_rf$average_total_uncertainty,
        names = rownames(table_rf),
        ylim = c(0, 2),
        main = "total uncertainty of different RF models", 
        xlab = "model",
        ylab = "total uncertainty",
        col = rgb(0.2,0.4,0.6,0.6),
        font.axis= 1,
        cex.axis = 1)

# create dataframe with 4 columns and 4 rows
table_percentage <- data.frame(col1=c(anteil_a, anteil_e), col2 = c(anteil_a_c, anteil_e_c),
                   col3 = c(anteil_a_m, anteil_e_m), col3 = c(anteil_a_ed, anteil_e_ed))
colnames(table_percentage) = c('model', 'model_c', 'model_m', 'model_ed')
table_percentage

#################### table of rg total uncertainty ########
table_rg <- data.frame(
  col1 = c(
    mean(total_uncertainty),
    mean(total_uncertainty_c),
    mean(total_uncertainty_m),
    mean(total_uncertainty_ed),
    mean(total_uncertainty_confi),
    mean(total_uncertainty_h),
    mean(total_uncertainty_h_only),
    mean(total_uncertainty_h_insteadED)),
  col2 = c(
    mean(epistemic_uncertainty),
    mean(epistemic_uncertainty_c),
    mean(epistemic_uncertainty_m),
    mean(epistemic_uncertainty_ed),
    mean(epistemic_uncertainty_confi),
    mean(epistemic_uncertainty_h),
    mean(epistemic_uncertainty_h_only),
    mean(epistemic_uncertainty_h_insteadED)),
  col3 = c(
    mean(aleatoric_uncertainty),
    mean(aleatoric_uncertainty_c),
    mean(aleatoric_uncertainty_m),
    mean(aleatoric_uncertainty_ed),
    mean(aleatoric_uncertainty_confi),
    mean(aleatoric_uncertainty_h),
    mean(aleatoric_uncertainty_h_only),
    mean(aleatoric_uncertainty_h_insteadED)),
  col4 = c(
    rg$prediction.error,
    rg_c$prediction.error,
    rg_m$prediction.error,
    rg_ed$prediction.error,
    rg_confi$prediction.error,
    rg_h$prediction.error,
    rg_h_only$prediction.error,
    rg_h_insteadED$prediction.error)
  )

colnames(table_rg) <- c("average_total_uncertainty",
                        "average_epistemic_uncertainty",
                        "average_aleatoric_uncertainty",
                        "average_oob")
rownames(table_rg) <- c('Standard RF Model',
                        'RF exclude C',
                        'RF exclude M',
                        'RF exclude X2',
                        'RF include Z',
                        'RF include Exogenous V',
                        'RF with only Exogenous V',
                        'RF with Exogenous V instead X2')

result_rg <- as.data.frame(table_rg)
result_rg$model <- rownames(table_rg)
View(result_rg)



########### visualisierung #############
set.seed(1)
x <- ctree(EPClass ~ StudyTime + Intelligence +
             SleepQuality + ExamDifficulty, data=test_data)
print(x)
plot(x, tp_args = list(ymax = 1))

##### define My_theme ####
My_Theme <- theme(
  axis.title.x = element_text(size = 19),
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
  axis.title.y = element_text(size = 19),
  title = element_text(size = 20))
###### plot oob and total uncertainty rf #####

ggplot(result_rf, mapping = aes(average_oob, average_total_uncertainty, colour = model))+
  geom_point(size = 3) + theme_bw() +
  labs(title = "Relationship between OOB error and total uncertainty",
       x = "Average OOB error",
       y = "Average total uncertainty",
       colour = "Models") +
  My_Theme
###### plot oob and total uncertainty rg #####
plot_TU_vs_obb_rg <- ggplot(result_rg,
                            mapping = aes(average_oob,
                                          average_total_uncertainty,
                                          colour = model))+
  geom_point(size = 4) +
  theme_bw() +
  labs(title = "Relationship between OOB Error and Total Uncertainty",
       x = "OOB Error",
       y = "Average Total Uncertainty") +
  My_Theme +
  scale_colour_brewer(palette = "Set1", name= "RF Models")+ 
   theme(
      legend.position = c(0.85, 0.2),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13))+
  ylim(0, 2) + xlim(0, 0.6)
  

plot_TU_vs_obb_rg
cor(result_rg$average_total_uncertainty, result_rg$average_oob)
###### plot oob and A uncertainty rg #####
plot_AU_vs_obb_rg <- ggplot(result_rg,
                            mapping = aes(average_oob,
                                          average_aleatoric_uncertainty,
                                          colour = model))+
  geom_point(size = 4) +
  theme_bw() +
  labs(#title = "Relationship between OOB Error and Aleatoric Uncertainty",
       x = "OOB Error",
       y = "Average Aleatoric Uncertainty") +
  My_Theme +
  scale_colour_brewer(palette = "Set1", name= "RF Models")+ 
  theme(
    legend.position = c(0.85, 0.2),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
    ) +
  ylim(0, 1) + xlim(0, 0.6)


plot_AU_vs_obb_rg
cor(result_rg$average_total_uncertainty, result_rg$average_oob)
###### plot oob and E uncertainty rg #####
plot_EU_vs_obb_rg <- ggplot(result_rg,
                            mapping = aes(average_oob,
                                          average_epistemic_uncertainty,
                                          colour = model))+
  geom_point(size = 4) +
  theme_bw() +
  labs(#title = "Relationship between OOB Error and Epistemic Uncertainty",
       x = "OOB Error",
       y = "Average Epistemic Uncertainty") +
  My_Theme +
  scale_colour_brewer(palette = "Set1", name= "RF Models")+ 
  theme(
    legend.position = c(0.85, 0.2),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13))+
  ylim(0, 1) + xlim(0, 0.6)


plot_EU_vs_obb_rg
cor(result_rg$average_epistemic_uncertainty, result_rg$average_oob)
##### Separate observation of Relationship between Uncertainties and OOB Error ####
A_E_TU_OOB <- ggarrange(plot_AU_vs_obb_rg, plot_EU_vs_obb_rg,
          ncol = 2, nrow = 1, common.legend = TRUE,
          legend = "bottom")
annotate_figure(A_E_TU_OOB,
                top = text_grob("Separate Observation of Relationship between Uncertainties and OOB Error",
                                size = 21))

#### create longer data ####
longer_data <- result_rg %>%
  pivot_longer(average_aleatoric_uncertainty:average_epistemic_uncertainty,
               names_to = "type", values_to = "uncertainty")
View(longer_data)

#### barplot total uncertainty of ranger model ####
total_U_Plot <- ggplot(longer_data,
                       aes(x = reorder(model, average_total_uncertainty),
                           y = uncertainty, fill=type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = wes_palette(n = 2, name="GrandBudapest1"),
                    labels = c("Average aleatoric U", "Average epistemic U")) +
  labs(title = "Comparison of Average Total Uncertainty in Various RF Models",
       x = "RF Models",
       y = "Average Total Uncertainty") + theme_bw() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1),
        legend.position = c(0.1, 0.9),
        legend.text = element_text(size = 11),
        legend.title = element_blank()) +
  My_Theme +
  scale_y_continuous(labels = scales::percent_format()) +
  ylim(0, 2) 

total_U_Plot
#### barplot uncertainty percentage  of ranger model ####
total_U_Plot_percentage <- ggplot(longer_data[order(longer_data$uncertainty, decreasing = T),],
                       aes(x = reorder(model, uncertainty),
                           y = uncertainty, fill=type)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = wes_palette(n = 2, name="GrandBudapest1"),
                    labels = c("Average aleatoric U", "Average epistemic U")) +
  labs(title = "Comparison of Proportions of Average Uncertainties in Various RF Models",
       x = "RF Models",
       y = "Average Total Uncertainty") + theme_bw() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1),
        legend.position = c(0.9, 0.15),
        legend.text = element_text(size = 11),
        legend.title = element_blank()) +
  My_Theme+
  scale_y_continuous(labels = scales::percent_format())

total_U_Plot_percentage
#### barplot aleatoric U of ranger model ####
aleatoric_U_Plot <- ggplot(result_rg) +
  geom_bar(mapping = aes(x = reorder(model, average_aleatoric_uncertainty),
                         average_aleatoric_uncertainty),
           stat = "identity",
           fill= wes_palette(1, name ="GrandBudapest1")) +
  labs(title = "Average aleatoric Uncertainty of different RF Models",
       x = "RF Models",
       y = "Average aleatoric uncertainty") +
  My_Theme + 
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1),
        legend.position = c(0.1, 0.9),
        legend.text = element_text(size = 9),
        legend.title = element_blank()) +
  ylim(0, 2)

aleatoric_U_Plot

#### barplot epistemic U of ranger model ####
epistemic_U_Plot <- ggplot(result_rg) +
  geom_bar(mapping = aes(x = reorder(model, average_epistemic_uncertainty),
                         average_epistemic_uncertainty),
           stat = "identity",
           fill= "lightcoral") +
  labs(title = "Average epistemic Uncertainty of different RF Models",
       x = "RF Models",
       y = "Average epistemic uncertainty") +
  My_Theme + 
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1),
        legend.position = c(0.1, 0.9),
        legend.text = element_text(size = 9),
        legend.title = element_blank()) +
  ylim(0, 2)




#### export the data sets ####
write_csv(dat, file = "ExamPerformance.csv")
write_csv(dat1, file = "ExamPerformance2.csv")
