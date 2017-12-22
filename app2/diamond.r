library(rpart)


library(ranger)

library(rvest)
library(kknn)

library(e1071)


diamondsBig = read.csv("diamonds.csv", strip.white = TRUE)
diamond = na.omit(diamondsBig)[c(-1, -9)]
filter(diamond, diamond$table != '0')
filter(diamond, diamond$depth != '0')
summary(diamond)
# divide the data into two part train dataset and test dataset
set.seed(1234)
sample_data = sample(2,
                     nrow(diamond),
                     replace = TRUE,
                     prob = c(0.85, 0.15))

test = diamond[sample_data == 2, ]
train = diamond[sample_data == 1, ]


lm2 <-
  lm(
    I(log10(price)) ~ I(carat ^ (1 / 3)) + carat + cut + clarity + color + cert +
      table + depth + (table / depth) + x * y * z ,
    data = train
  ) ## good
 

#testPred2 = predict.lm(lm2, test, level = 0.95)

diamondTest = data.frame(2,
                         "Good",
                         "D",
                         "IF",
                         3,
                         2,
                         "GIA",
                         2,
                         3,
                         4)
colnames(diamondTest) = c('carat',
                          'cut',
                          "color",
                          'clarity',
                          'table',
                          'depth',
                          'cert',
                          'x',
                          'y',
                          'z')

t = test[1, ] 
View(t) # price shoule be 300
testPred2 = predict.lm(lm2, diamondTest, level = 0.95)
   10^testPred2
   
# with ranger
fm6 <-
  ranger(price ~ .,
         data = train,
         write.forest = TRUE,
         num.trees = 600)
 
testPred5 <-
  predict(fm6,
          data = t,
          interval = "prediction",
          level = .95)
####### with kknn
kknnModel = kknn(
  price ~ .,
  train,
  test,
  k = 3,
  kernel =   "optimal" ,
  distance = 2
)
testPred6 <-
  predict(kknnModel,
          data = t,
          level = .95)



