library("dplyr")
library("car")
library("forcats")
library("rpart")
library("rpart.plot")
library("nnet")
library("foreign")
library("corrplot")
library("randomForest")
library("pdp")
library("ggplot2")
library("gplots")
library("effects")

source(choose.files())

QK <- read.csv(choose.files())

View(QK)

#changing row names to cust ID 
row.names(QK) <- QK$custid
QK$custid <- NULL
QK$LastOrder <- NULL

#last order 
QK$LastOrder.Mon <-
  recode_factor(QK$LastOrder.Mon,
                `1` = "jan",
                `2` = "feb",
                `3` = "mar",
                `10` = "oct",
                `11` = "nov",
                `12` ="dec")

View(QK)


#binning income 
hist(QK$TotPurch, breaks = 60, col = "blue")

QK$Income.Cat <- 
  binVariable(QK$DA_Income, 
              bins = 5, 
              method = "proportions", 
              labels = c("very low", "low", "medium", "high", "very high"))

View(QK)

#model w/ income cat 
Forest00 <- randomForest(formula = SUBSCRIBE ~ LastOrder.Mon + DA_Under20 + DA_Over60 + Income.Cat +
                          DA_Single + 
                          NumDeliv + NumMeals +  MealsPerDeliv +
                          Veggie + TotPurch,
                        data = filter(QK, Sample == "Estimation"),
                        importance = TRUE, 
                        na.action=na.roughfix,
                        ntree = 500, mtry = 4)

Forest00
#model 
Forest1 <- randomForest(formula = SUBSCRIBE ~ LastOrder.Mon + DA_Under20 + DA_Over60 + DA_Income +
                          DA_Single + 
                          NumDeliv + NumMeals +  MealsPerDeliv +
                          Veggie + TotPurch,
                        data = filter(QK, Sample == "Estimation"),
                        importance = TRUE, 
                        na.action=na.roughfix,
                        ntree = 500, mtry = 4)

Forest1

#checking correlated variables 
cor(QK$MealsPerDeliv, QK$NumMeals)
cor(QK$MealsPerDeliv, QK$NumDeliv)
cor(QK$NumMeals, QK$Healthy)
cor(QK$NumMeals, QK$Meaty)
cor(QK$NumMeals, QK$Veggie)
cor(QK$NumMeals, QK$Special)

#num meals w/ healthy 

QK1 <- read.csv(choose.files())
cor(QK1$Week3Meals, QK1$TotPurch)
cor(QK1$SUBSCRIBE, QK1$Veggie)

View(QK1)

#subscribe importance 
QK2 <- read.csv(choose.files())
LinearSub <- lm(formula= SUBSCRIBE ~  DA_Under20 + DA_Over60 + DA_Income +
                  DA_Single + 
                  NumDeliv + NumMeals +  MealsPerDeliv +
                  Healthy + Veggie + Meaty + Special + TotPurch,
                data = QK2)


#de factor 
QK$LastOrder.New <- fct_collapse(QK$LastOrder.Mon, 
                                 EndYr = c("oct", "nov", "dec"),
                                 Jan =("jan"),
                                 Feb=("feb"),
                                 Mar=("mar"))


#partial dependent plot 
# partial(DA_Under20, pred.var = "DA_Income", # target and predictor
#         prob = TRUE, # probabilities on yaxis
#         which.class = 2, # predict level 2, "Y"
#         plot = TRUE, # generate plot
#         rug = TRUE, # plot decile hashmarks
#         plot.engine = "ggplot2")


#tree model 
treeQK <- rpart(formula = SUBSCRIBE ~ DA_Under20 + DA_Over60 + LastOrder.Mon + DA_Income + 
                 DA_Single + NumDeliv + NumMeals +
                TotPurch, 
                 data = filter(QK, Sample == "Estimation"),
                 cp = 0.01, 
                 model = TRUE)

#pruning table 
printcp(treeQK)
plotcp(treeQK, upper = "splits")

#plot tree
rpart.plot(treeQK, type = 0, extra = 2,
           fallen.leaves = TRUE, uniform = FALSE,
           yes.text = "true", no.text = "false",
           cex = .8,
           digits = 3)


#scatter plot matrix
scatterplotMatrix( ~ SUBSCRIBE + DA_Income + DA_Under20
                   + DA_Over60 + DA_Single + Veggie + NumDeliv + NumMeals + MealsPerDeliv + TotPurch,
                   diagonal=list(method="boxplot"),
                   data = QK)

#check for no missing value
options(digits = 3)
variable.summary(QK)

#Forest time 
Forest2 <- randomForest(formula = SUBSCRIBE ~ DA_Under20 + DA_Over60 + LastOrder.Mon + DA_Income +
                          DA_Single + 
                          NumDeliv + NumMeals + MealsPerDeliv +
                          Healthy + Veggie + Meaty + Special + TotPurch,
                        data = filter(QK, Sample == "Estimation"),
                        importance = TRUE, 
                        na.action=na.roughfix,
                        ntree = 500, mtry = 5)

Forest2

importance(Forest2, type = 2)

varImpPlot(Forest2, type = 2, main = "Importance Plot")

#forest
ForestNumMeal  <- randomForest(formula = SUBSCRIBE ~ LastOrder.Mon + DA_Under20 + DA_Over60 + DA_Income +
                          DA_Single + 
                          NumDeliv  +  MealsPerDeliv +
                          Healthy + Veggie + Meaty + Special + TotPurch,
                        data = filter(QK, Sample == "Estimation"),
                        importance = TRUE, 
                        na.action=na.roughfix,
                        ntree = 500, mtry = 4)

ForestNumMeal

#model without last order month
Forest00 <- randomForest(formula = SUBSCRIBE ~ DA_Under20 + DA_Over60 + DA_Income +
                           DA_Single + 
                           NumDeliv + NumMeals +  MealsPerDeliv +
                           Healthy + Veggie + Meaty + Special + TotPurch,
                         data = filter(QK, Sample == "Estimation"),
                         importance = TRUE, 
                         na.action=na.roughfix,
                         ntree = 500, mtry = 4)
Forest00
#model submission
ForestModelSub <- randomForest(formula = SUBSCRIBE ~ LastOrder.Mon + DA_Under20 + DA_Over60 + DA_Income +
                          DA_Single + 
                          NumDeliv + NumMeals +  MealsPerDeliv +
                          Veggie + TotPurch,
                        data = filter(QK, Sample == "Estimation"),
                        importance = TRUE, 
                        na.action=na.roughfix,
                        ntree = 500, mtry = 4)


ForestModelSub

#Forest w only veggie and no last month order 
Forest1 <- randomForest(formula = SUBSCRIBE ~ DA_Under20 + DA_Over60 + DA_Income +
                          DA_Single + 
                          NumDeliv + NumMeals +  MealsPerDeliv +
                         Veggie + TotPurch,
                        data = filter(QK, Sample == "Estimation"),
                        importance = TRUE, 
                        na.action=na.roughfix,
                        ntree = 500, mtry = 4)

Forest1

importance(Forest1, type = 2)

varImpPlot(Forest1, type = 2, main = "Importance Plot")

Sample <- QK$Sample

#model with out meaty 
ForestM <- randomForest(formula = SUBSCRIBE ~ LastOrder.Mon + DA_Under20 + DA_Over60 + DA_Income +
                           DA_Single + 
                           NumDeliv + NumMeals +  MealsPerDeliv +
                           Healthy + Veggie + Special + TotPurch,
                         data = filter(QK, Sample == "Estimation"),
                         importance = TRUE, 
                         na.action=na.roughfix,
                         ntree = 500, mtry = 4)

ForestM

#forest w/ income log 
#QK$Log.DA_Income <- log(QK$DA_Income)
#ForestLogIncome <- randomForest(formula = SUBSCRIBE ~ Disc + DA_Under20 + DA_Over60 + Log.DA_Income +
 #                         DA_Single + 
  #                        NumDeliv + NumMeals +  MealsPerDeliv +
   #                       Healthy + Veggie + Meaty + Special + TotPurch + Weeks3Meals,
    #                    data = filter(QK, Sample == "Estimation"),
     #                   importance = TRUE, 
      #                  na.action=na.roughfix,
       #                 ntree = 500, mtry = 4)

#ForestLogIncome

#Forest w/ total purchase log
#QK$Log.TotPurch <- log(QK$TotPurch)
#ForestTotPurch <- randomForest(formula = SUBSCRIBE ~ Disc + DA_Under20 + DA_Over60 + DA_Income +
 #                                 DA_Single + 
  #                                NumDeliv + NumMeals +  MealsPerDeliv +
   #                               Healthy + Veggie + Meaty + Special + Log.TotPurch + Weeks3Meals,
    #                            data = filter(QK, Sample == "Estimation"),
     #                           importance = TRUE, 
      #                          na.action=na.roughfix,
       #                         ntree = 500, mtry = 4)

#ForestTotPurch

#LinearQK with all varialbes and month in a category 
LinearQK <- glm(formula = SUBSCRIBE ~ + DA_Under20 + DA_Over60 + DA_Income +
                  DA_Single + 
                  NumDeliv + NumMeals +  MealsPerDeliv +
                  Healthy + Veggie + Meaty + Special + TotPurch,
                data = filter(QK, Sample == "Estimation"),
                 family = binomial(logit))

summary(LinearQK)
MR2 <- 1 - (LinearQK$deviance / LinearQK$null.deviance)
MR2


#log w/ last order merged 
#LinearQK1 <- glm(formula = SSUBSCRIBE ~ Disc + DA_Under20 + DA_Over60 + LastOrder.Mon + DA_Income +
   #                DA_Single + NumDeliv + NumMeals + MealsPerDeliv + Healthy + Veggie + Meaty + Special + TotPurch + Weeks3Meals,
  #              data = filter(QK, Sample == "Estimation"),
 #               family = binomial(logit))

#summary(LinearQK1)

#remove with relvl
#LinearQK2 <- glm(formula = SUBSCRIBE ~ Disc + DA_Under20 + DA_Over60 + LastOrder.Mon + DA_Income +
 #                  DA_Single + NumDeliv + NumMeals + MealsPerDeliv + Healthy + Veggie + Meaty + Special + TotPurch + Weeks3Meals,
  #               data = filter(QK, Sample == "Estimation"),
   #              family = binomial(logit))

#summary(LinearQK2)

#all 
LinearQK3 <- glm(formula = SUBSCRIBE ~ Disc + DA_Under20 + DA_Over60 + LastOrder + DA_Income +
                   DA_Single + NumDeliv + NumMeals + MealsPerDeliv + Healthy + Veggie + Meaty + Special + TotPurch + Weeks3Meals,
                 data = filter(QK, Sample == "Estimation"),
                 family = binomial(logit))

summary(LinearQK3)



#ANN
NN.HL2 <- Nnet(formula = SUBSCRIBE ~ Disc + DA_Under20 + DA_Over60  + DA_Income +
                 DA_Single + NumDeliv + NumMeals + MealsPerDeliv + Healthy + Veggie + Meaty + Special + TotPurch,
          data = filter(QK, Sample == "Estimation"), 
             decay = 0.10,
          size = 2)


summary(NN.HL2)


NN.HL4 <- Nnet(formula = SUBSCRIBE ~ Disc + DA_Under20 + DA_Over60  + DA_Income +
                 DA_Single + NumDeliv + NumMeals + MealsPerDeliv + Healthy + Veggie + Meaty + Special + TotPurch,
               data = filter(QK, Sample == "Estimation"), 
               decay = 0.10,
               size = 4)

summary(NN.HL4)

NN.HL6 <- Nnet(formula = SUBSCRIBE ~ Disc + DA_Under20 + DA_Over60  + DA_Income +
                 DA_Single + NumDeliv + NumMeals + MealsPerDeliv + Healthy + Veggie + Meaty + Special + TotPurch,
               data = filter(QK, Sample == "Estimation"), 
               decay = 0.10,
               size = 6)

summary(NN.HL6)

lift.chart(modelList = c("Forest1", "Forest00", "NN.HL6"),
           data = filter(QK, Sample == "Estimation"),
           targLevel = "Y", trueResp = 0.01,
          type = "incremental", sub = "Estimation")

lift.chart(modelList = c("Forest1", "Forest00", "NN.HL6"),
           data = filter(QK, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01,
           type = "incremental", sub = "Validation")

lift.chart(modelList = c("Forest1", "Forest00", "NN.HL6"),
           data = filter(QK, Sample == "Estimation"),
           targLevel = "Y", trueResp = 0.5,
           type = "incremental", sub = "Estimation")

lift.chart(modelList = c("Forest1", "Forest00", "NN.HL6"),
           data = filter(QK, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.5,
           type = "incremental", sub = "Validation")


warnings()
QK$Score <- predict(Forest2, QK, type="prob")
predict(Forest2, QK, type="prob")

getOption("max.print")
options(max.print = 2000)

View(QK)

