library(ranger)
library(kknn)

 

diamondsBig = read.csv("diamonds.csv", strip.white = TRUE)
diamond = na.omit(diamondsBig)[c(-1, -9)]
# diamond=diamond[which(diamond$table!=0),]
 

View(diamond)
summary(diamond)
# divide the data into two part train dataset and test dataset
set.seed(1234)

sample_data = sample(2,
                     nrow(diamond),
                     replace = TRUE,
                     prob = c(0.85, 0.15))

test = diamond[sample_data == 2, ]
train = diamond[sample_data == 1, ]


lm2 =lm(
    I(log10(price)) ~ I(carat ^ (1 / 3)) + carat + cut + clarity + color + cert +
      table + depth + (table / depth) + x * y * z ,
    data = train
  ) ## good
 

#testPred2 = predict.lm(lm2, test, level = 0.95)
 
# with ranger
fm6 <-
  ranger(price ~ .,
         data = train,
         write.forest = TRUE,
         num.trees = 550,   min.node.size=5,
         save.memory=TRUE
)
 