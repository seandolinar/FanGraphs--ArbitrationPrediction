#install.packages('randomForest')
#install.packages('caret')
library(randomForest)
library(caret)


####FUNCTIONS####################
#previous arbitration flag variable
prev_arb_parse <- function(df){
  
  df$prev_arb <- 0
  for (i in 1:nrow(df)) {
    
    name <- df$player[i]
    data.player <- df[which(df$player == name),]
    print(data.player)
    for (j in 1:nrow(data.player)){
      
      lag_salary <- data.player[which(data.player$year == df$year[i]-1),'salary']
      if (length(lag_salary) > 0){
        
        df$prev_arb[i] <- 1
      }  
    }
  }
  return(df)
}

unpack_list <- function(list_packed) {
  
  df <- list_packed[[1]]
  for (i in 2:length(list_packed)){
    
    df <- rbind(df,list_packed[[i]])  
  }
  return(df)
}

log_modulus <- function(number) {
  
  if (number < 0 ){
    sign = -1
  }
  else{
    sign = 1
  }
  
  return(sign * log(abs(number)+1))
  
}
##################################



##############
###Data Load##
##############
mult <- 1 #inflation multiplier

#data load -- PITCHERS
data.pitchers.FULL <- read.csv('~/fg/projects/alex_arbitration/data_merge/2011_2015_data_pitchers_v03.csv')
data.pitchers.FULL$raise <- data.pitchers.FULL$salary - data.pitchers.FULL$lag_salary
data.pitchers.FULL$new_cpi <- (data.pitchers.FULL$cpi_deflat-1)*mult+1
data.pitchers.FULL$raise_adj <- data.pitchers.FULL$raise / data.pitchers.FULL$new_cpi
data.pitchers.FULL <- subset(data.pitchers.FULL,subset = data.pitchers.FULL$note != 'remove')
data.pitchers.FULL <- prev_arb_parse(data.pitchers.FULL)
data.pitchers.FULL$SV_sq <- data.pitchers.FULL$SV^2
data.pitchers.FULL$SP <- model.matrix(~data.pitchers.FULL$Pos)[,3]
data.pitchers.FULL$IP_sq <- data.pitchers.FULL$IP^2
data.pitchers.FULL$salary_adj <- data.pitchers.FULL$salary / data.pitchers.FULL$new_cpi

data.pitchers.FULL$raise_log <- log(data.pitchers.FULL$raise)



#data load -- HITTERS
data.hitters.FULL <- read.csv('~/fg/projects/alex_arbitration/data_merge/2011_2015_data_hitter_morestats.csv')
data.hitters.FULL$raise <- data.hitters.FULL$salary - data.hitters.FULL$lag_salary
data.hitters.FULL <- subset(data.hitters.FULL,subset = data.hitters.FULL$note != 'remove')
data.hitters.FULL <- prev_arb_parse(data.hitters.FULL)
data.hitters.FULL$new_cpi <- (data.hitters.FULL$cpi_deflat-1)*mult+1
data.hitters.FULL$raise_adj <- data.hitters.FULL$raise / data.hitters.FULL$new_cpi
data.hitters.FULL$salary_adj <- data.hitters.FULL$salary / data.hitters.FULL$new_cpi


#FOR LOOP
year_list = 2012:2015



#PITCHER LOOP
#linear models
ALL.results <- list()
for (i in 1:length(year_list)) {
 

    #Create train/train sets
    train.set.pitchers <- subset(data.pitchers.FULL,subset = data.pitchers.FULL$year < year_list[i])
    test.set.pitchers <- subset(data.pitchers.FULL,subset = data.pitchers.FULL$year == year_list[i])
 
    #lm - salary = career WAR
    model.pitcher <- lm(salary_adj ~ C_WAR, data=train.set.pitchers)
    print(summary(model.pitcher))
    lm_raise_hat <- predict.lm(object = model.pitcher, newdata=test.set.pitchers)*test.set.pitchers$new_cpi
    
    #lm - raise = WAR
    model.pitcher <- lm(raise_adj ~ WAR, data=train.set.pitchers)
    print(summary(model.pitcher))
    lm_raise_hat <- predict.lm(object = model.pitcher, newdata=test.set.pitchers)*test.set.pitchers$new_cpi

    #lm - salary = career R9WAR
    model.pitcher <- lm(salary_adj ~ C_R9_WAR, data=train.set.pitchers)
    print(summary(model.pitcher))
    lm_raise_hat <- predict.lm(object = model.pitcher, newdata=test.set.pitchers)*test.set.pitchers$new_cpi
    
    #lm - raise = R9WAR
    model.pitcher <- lm(raise_adj ~ RA9.WAR, data=train.set.pitchers)
    print(summary(model.pitcher))
    lm_raise_hat <- predict.lm(object = model.pitcher, newdata=test.set.pitchers)*test.set.pitchers$new_cpi
    
    #lm - raise = RA9WAR + Saves
    model.pitcher <- lm(raise_adj ~ RA9.WAR + SV, data=train.set.pitchers)
    print(summary(model.pitcher))
    lm_raise_hat <- predict.lm(object = model.pitcher, newdata=test.set.pitchers)*test.set.pitchers$new_cpi
    
    #lm - raise = RA9WAR + Saves
    model.pitcher <- lm(raise_adj ~ IP + ERA + SV + RA9.WAR + prev_arb, data=train.set.pitchers)
    print(summary(model.pitcher))
    lm_raise_hat <- predict.lm(object = model.pitcher, newdata=test.set.pitchers)*test.set.pitchers$new_cpi
}




#PREDICTION
#FOR LOOP
year_list = 2012:2015


ALL.results <- list()
#PITCHER LOOP
#Error Validation
for (i in 3:3) {
      i <- 3
      data.pitchers.NEW <- data.pitchers.FULL
      data.pitchers.NEW$SV <- data.pitchers.FULL$SV / max(data.pitchers.FULL$SV)
      data.pitchers.NEW$SO <- data.pitchers.FULL$SO / max(data.pitchers.FULL$SO)
      data.pitchers.NEW$WAR <- data.pitchers.FULL$WAR / max(data.pitchers.FULL$WAR)
      data.pitchers.NEW$ERA <- data.pitchers.FULL$ERA / max(data.pitchers.FULL$ERA)
      data.pitchers.NEW$C_IP <- data.pitchers.FULL$C_IP / max(data.pitchers.FULL$C_IP)
      data.pitchers.NEW$C_WAR <- data.pitchers.FULL$C_WAR / max(data.pitchers.FULL$C_WAR)
      
      #Create train/train sets
      train.set.pitchers <- subset(data.pitchers.NEW,subset = data.pitchers.NEW$year != year_list[i])
      test.set.pitchers <- subset(data.pitchers.NEW,subset = data.pitchers.NEW$year == year_list[i])
    
#       #rf
      model.pitcher.rf <- randomForest(raise ~ arb_yr + IP + W + SO + ERA + SV + lag_salary + Pos + BB,
                                       data=train.set.pitchers, na.action=na.omit, ntree=2000)
#       model.pitcher.rf
      rf_raise_hat <- as.vector(predict(object = model.pitcher.rf, newdata=test.set.pitchers))
      cor(test.set.pitchers$salary, (rf_raise_hat) + test.set.pitchers$lag_salary)^2
#       
#       #ALL results list
#       ALL.results[[i]] <- data.frame(year=test.set.pitchers$year, player = test.set.pitchers$player,
#                                     salary=test.set.pitchers$salary,
#                                     rf_pred_salary = (rf_raise_hat+test.set.pitchers$lag_salary))
#       
      inputs <- train.set.pitchers[,c('arb_yr','IP','ERA','SV','SO','WAR','C_IP','C_WAR','SP')]
      output <- train.set.pitchers[,c('raise')]
      model <- knnreg(inputs, output, k=15)

      new.inputs <- test.set.pitchers[,c('arb_yr','IP','ERA','SV','SO','WAR','C_IP','C_WAR','SP')]
      predict(model, new.inputs)
      test.set.pitchers[,c('raise')]
      cor(test.set.pitchers$salary, predict(model, new.inputs) + test.set.pitchers$lag_salary)^2
}   


#unpacks the lists of different years
ALL.results <- unpack_list(ALL.results)
ALL.results <- ALL.results[[1]]

#determines correlation of all the years using the four different approaches
cor(ALL.results$salary, ALL.results$rf_pred_salary)

#tool to look at individual years
yr.results <- subset(ALL.results, subset = ALL.results$year == 2015)
cor(yr.results$salary, yr.results$rf_pred_salary)



#inference models -- NO TEST SETS

model.pitcher.salary <- lm(I(salary_adj - 390000) ~ 0 + C_IP + C_WAR + SV, data=data.pitchers.FULL)
print(summary(model.pitcher.salary))
AIC(model.pitcher.salary)
plot(model.pitcher.salary)
cor(data.pitchers.NEW$salary, exp(predict(model.pitcher.salary))+390000)

min(data.pitchers.NEW$lag_salary)

model.pitcher.salary <- lm(salary_adj ~ C_WAR, data=data.pitchers.FULL)
plot(model.pitcher.salary)
print(summary(model.pitcher.salary))
AIC(model.pitcher.salary)

model.pitcher.salary <- lm(I(log(salary_adj)) ~ C_WAR, data=data.pitchers.FULL)
plot(model.pitcher.salary)
print(summary(model.pitcher.salary))
AIC(model.pitcher.salary)


summary(data.pitchers.FULL$salary)




model.pitcher.raise <- lm(raise_adj ~ IP + SV + RA9.WAR + arb_yr, data=data.pitchers.FULL)
print(summary(model.pitcher.raise))
AIC(model.pitcher.raise)
plot(model.pitcher.raise)
lm_raise_hat <- predict.lm(object = model.pitcher.raise, newdata=data.pitchers.FULL)*data.pitchers.FULL$new_cpi
cor(data.pitchers.FULL$salary, lm_raise_hat + data.pitchers.FULL$lag_salary)

model.pitcher.raise <- lm(raise_adj ~ IP + I(SV*(arb_yr+1))+ RA9.WAR, data=data.pitchers.FULL)
print(summary(model.pitcher.raise))
AIC(model.pitcher.raise)
plot(model.pitcher.raise)
lm_raise_hat <- predict.lm(object = model.pitcher.raise, newdata=data.pitchers.FULL)*data.pitchers.FULL$new_cpi
plot(data.pitchers.FULL$salary, lm_raise_hat + data.pitchers.FULL$lag_salary)


model.pitcher.raise <- lm(raise_adj ~ IP + SV + I(IP * FIP), data=data.pitchers.FULL)
print(summary(model.pitcher.raise))
AIC(model.pitcher.raise)
plot(model.pitcher.raise)
lm_raise_hat <- predict.lm(object = model.pitcher.raise, newdata=data.pitchers.FULL)*data.pitchers.FULL$new_cpi
cor(data.pitchers.FULL$salary, lm_raise_hat + data.pitchers.FULL$lag_salary)
plot(data.pitchers.FULL$salary, lm_raise_hat + data.pitchers.FULL$lag_salary)
res <- log(model.pitcher.raise$residuals^2)
a <- lm(res ~ 0 + data.pitchers.FULL$IP + data.pitchers.FULL$SV + I(data.pitchers.FULL$IP * data.pitchers.FULL$FIP))
summary(a)
predict(a)
b <- 1/exp(predict(a))
model.pitcher.raise.fgls <- lm(raise_adj ~ 0 + IP + SV + I(IP * FIP), weights=b, data=data.pitchers.FULL)
summary(model.pitcher.raise.fgls)
plot(model.pitcher.raise.fgls)

lm_raise_hat <- predict.lm(object = model.pitcher.raise.fgls, newdata=data.pitchers.FULL)*data.pitchers.FULL$new_cpi
cor(data.pitchers.FULL$salary, lm_raise_hat + data.pitchers.FULL$lag_salary)

#PLAIN WAR
model.pitcher.raise <- lm(raise_adj ~ WAR, data=data.pitchers.FULL)
print(summary(model.pitcher.raise))
AIC(model.pitcher.raise)
plot(data.pitchers.FULL$raise_adj, predict(model.pitcher.raise))

lm_raise_hat <- predict.lm(object = model.pitcher.raise, newdata=data.pitchers.FULL)*data.pitchers.FULL$new_cpi
plot(data.pitchers.FULL$salary, lm_raise_hat + data.pitchers.FULL$lag_salary)








model.lm <- lm(raise_adj ~ IP + SV + I(IP*FIP) + arb_yr, data=data.pitchers.FULL)

model.lme <- lmer(raise_adj ~ IP + SV + I(IP*FIP) + (1 | arb_yr) + (1 | Pos), data=data.pitchers.FULL)

data.pitchers.FULL$res <- log((data.pitchers.FULL$raise_adj-predict(model.lme))^2)
b = 1/exp(predict(lmer(res ~ IP + SV + ERA + (1 | arb_yr) + (1 | Pos), data=data.pitchers.FULL)))

model.lme <- lmer(raise_adj ~ IP + SV + FIP  + (1 | arb_yr) + (1 | Pos), weights=b, data=data.pitchers.FULL)



fixef(model.lme)
summary(model.lm)
summary(model.lme)
AIC(model.lm)
AIC(model.lme)
model.lme

model.lm <- lm(salary_adj ~ C_WAR, data=data.pitchers.FULL)
print(summary(model.lm))
AIC(model.lm)


plot(data.pitchers.FULL$salary, predict(model.lm) * data.pitchers.FULL$cpi_deflat + data.pitchers.FULL$lag_salary)
plot(data.pitchers.FULL$salary, predict(model.lme) * data.pitchers.FULL$cpi_deflat + data.pitchers.FULL$lag_salary)
cor(data.pitchers.FULL$salary, predict(model.lme) * data.pitchers.FULL$cpi_deflat + data.pitchers.FULL$lag_salary)


plot(model.lme$residuals)


plot(y=data.pitchers.FULL$raise_adj-predict(model.lme) , predict(model.lme))
plot(model.lme)

###########
##hitters##
###########

#Hitters FOR loop

HIT.results <- list()

for (i in 1:length(year_list)) {


  #Creates Holdout Set
  train.set.hitters <- subset(data.hitters.FULL,subset = data.hitters.FULL$year < year_list[i])
  test.set.hitters <- subset(data.hitters.FULL,subset = data.hitters.FULL$year == year_list[i])

  #lm1
  model.hitters <- lm(raise_adj ~ PA + HR + Pos + RBI + AVG + R + RAR + SB + arb_yr + lag_salary, data=train.set.hitters)
  lm_raise_hat <- predict(object = model.hitters, newdata=test.set.hitters)*test.set.hitters$new_cpi
  
  summary(lm(test.set.hitters$salary ~ as.vector(lm_raise_hat+test.set.hitters$lag_salary)))
  summary(model.hitters)
  
  #rf#arb_yr + PA + HR + RBI + SO + AVG + RAR + +wRC. + R + lag_salary + Pos + age + C_WAR + C_HR + prev_arb + C_PA
  model.hitters.rf <- randomForest(raise_adj ~ PA + HR + RBI + AVG + SB + lag_salary + arb_yr + WAR, data=data.hitters.FULL,
                                 na.action=na.omit, ntree=2000)
  model.hitters.rf
  rf_raise_hat <- predict(object = model.hitters.rf, newdata=test.set.hitters)*test.set.hitters$new_cpi
  cor(test.set.hitters$salary, rf_raise_hat +test.set.hitters$lag_salary)^2
  
  #hitters results out
  HIT.results[[i]] <- data.frame(year=test.set.hitters$year, player = test.set.hitters$player,
                                 salary=test.set.hitters$salary, lm_pred_salary = lm_raise_hat+test.set.hitters$lag_salary,
                                 rf_pred_salary = rf_raise_hat+test.set.hitters$lag_salary)
  
}

#unpacks all the years for hitters
HIT.results[[4]]
cor(HIT.results[[4]]$salary, HIT.results[[4]]$rf_pred_salary)

HIT.results <- unpack_list(HIT.results)
cor(HIT.results$salary, HIT.results$rf_pred_salary)

lm(HIT.results$salary ~ HIT.results$rf_pred_salary)

#combines the pitchers and hitters
BOTH.results <- rbind(HIT.results, ALL.results)
cor(BOTH.results$salary, BOTH.results$rf_pred_salary)

#tool to look at each year
yr.results <- subset(HIT.results, subset = HIT.results$year == 2012)
cor(yr.results$salary, yr.results$rf_pred_salary)


train.set.hitters$log_raise <- sapply(train.set.hitters$raise_adj, FUN=log_modulus)


train.set.hitters.pre_arb <- train.set.hitters[which(train.set.hitters$prev_arb == 1),]
train.set.hitters.arb <- train.set.hitters[which(train.set.hitters$prev_arb == 0),]
hist(train.set.hitters.pre_arb$raise_adj)
hist(train.set.hitters.arb$raise_adj)

test.set.hitters.pre_arb <- test.set.hitters[which(test.set.hitters$prev_arb == 1),]
test.set.hitters.arb <- test.set.hitters[which(test.set.hitters$prev_arb == 0),]

model.hitters.career <- randomForest(log_raise ~ arb_yr + PA + HR + RBI + SO + AVG + K. + RAR + wRC. + + age , data=data.hitters.FULL,
                                 na.action=na.omit, ntree=2000)
model.hitters.career
cor(test.set.hitters$salary, exp(predict(object = model.hitters.career, newdata=test.set.hitters))-1+test.set.hitters$lag_salary)



####HITTERS

model.hitters <- lm(raise_adj ~ PA + HR + RBI + I(AVG*PA) + WAR, data=data.hitters.FULL)
model.hitters <- lm(raise_adj ~ PA + HR + Pos + RBI + I(AVG*PA) + R + RAR + SB + arb_yr, data=data.hitters.FULL)

model.hitters <- lm(salary_adj ~ C_WAR, data=data.hitters.FULL)
model.hitters <- lm(I(log(salary_adj)) ~ C_WAR, data=data.hitters.FULL)

summary(model.hitters)
exp(13.95)
exp(.1217)
plot(model.hitters)

plot(x=data.hitters.FULL$C_WAR, data.hitters.FULL$salary)
lines(exp.df)

exp.df <- data.frame(x=newdata, y=exp(predict(newdata=newdata, model.hitters)))

newdata <- data.frame(C_WAR = seq(0,20,.01))
exp(predict(newdata=newdata, model.hitters))

plot(x=exp(predict(model.hitters)), data.hitters.FULL$salary)
plot(x=(predict(model.hitters)), data.hitters.FULL$salary)


model.pitchers <- lm(salary_adj ~ C_WAR, data=data.pitchers.FULL)
model.pitchers <- lm(I(log(salary_adj)) ~ C_WAR, data=data.pitchers.FULL)

summary(model.pitchers)
plot(x=exp(predict(model.pitchers)), y=data.pitchers.FULL$salary)
plot(x=(predict(model.pitchers)), y=data.pitchers.FULL$salary)





model.hitters <- lm(raise_adj ~ PA + HR + RBI + I(AVG*PA), data=data.hitters.FULL)
model.hitters <- lmer(raise_adj ~ PA + HR + RBI + (1 | arb_yr) + (1 | Pos), data=data.hitters.FULL)
model.hitters <- lmer(raise_adj ~ (1 | arb_yr) + (1 | Pos), data=data.hitters.FULL)

model.hitters <- lmer(raise_adj ~ WAR + (1 | arb_yr) + (1 | Pos), data=data.hitters.FULL)
model.hitters <- lm(raise_adj ~ WAR, data=data.hitters.FULL)

summary(model.hitters)
AIC(model.hitters)



plot((data.hitters.FULL$salary), x=(data.hitters.FULL$C_WAR))
plot((data.pitchers.FULL$salary), x=(data.pitchers.FULL$C_WAR))

data.hitters.FULL$res <- log((data.hitters.FULL$raise_adj - predict(model.hitters))^2)

b <- exp(predict(lmer(res ~ PA + HR  + RBI + WAR + R + (1 | arb_yr) + (1 | Pos), data=data.hitters.FULL)))
model.hitters <- lmer(raise_adj ~ WAR + (1 | arb_yr) + (1 | Pos), weights = b, data=data.hitters.FULL)







cor(predict(object = model.hitters, newdata=data.hitters.FULL)*data.hitters.FULL$new_cpi+data.hitters.FULL$lag_salary,data.hitters.FULL$salary)


plot(predict(object = model.hitters, newdata=data.hitters.FULL)*data.hitters.FULL$new_cpi+data.hitters.FULL$lag_salary,data.hitters.FULL$salary)
A <- I(predict(object = model.hitters, newdata=data.hitters.FULL)*data.hitters.FULL$new_cpi+data.hitters.FULL$lag_salary)
summary(lm(data.hitters.FULL$salary ~ A))



plot(model.hitters)
summary(model.hitters)


summary(data.new)



