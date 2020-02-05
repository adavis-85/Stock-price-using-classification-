##Depending on if everything is updated you can just click on your csv file and import it.

##Attach so that you can use each separate column without writing the long way ex:TSLA$Open
attach(TSLA)

##Only strong variable is graphed to show activity
plot(Volume)

##There is only the prices and the volume given for the stock.  A vector is needed to show wether
##or not the price has gone up at the end of trading for the day.
Direction=rep(0,length(Close))
summary(Direction)

for (i in 2:253)
{
  if (Close[i]>Close[i-1])
  {
    Direction[i]=1
  }
}

##Need to add the column of up or down represented by a 0 for down or a 1 for up to the existing
##matrix/columns
T=data.frame(Direction,TSLA)

names(T)

##See the correlation of each of the price and volume columns.  The direction and date are excluded
cor(T[,c(3,4,5,6,7,8)])

summary(T)

##Fitting the linear regression model to the data.  The date and the adjusted closing price are excluded
glm.fits=glm(Direction~Open+High+Low+Close+Volume,data=T)
summary(glm.fits)

##The coeffitients of our linear model.
coef(glm.fits)

##Actually use training data to make a more realistic prediction.
train=(Date<Date[200])

##These set up the testing colummns.
Tesla.test=T[!train,]
dim(Tesla.test)
Direction.test=Direction[!train]

##Now run the training data through the linear regression model and then use the model to 
##predict the stock going up or down.
glm.fits=glm(Direction~Open+High+Low+Close+Volume,data=T,family=binomial,subset=train)
glm.probs=predict(glm.fits,Tesla.test,type="response")

##Vector to show the probabilities of the model predicting the up or down activity versus the actual
##activity of the test set
glm.pred=rep(0,54)
glm.pred[glm.probs>.5]=1
table(glm.pred,Direction.test)

##Accuracy rate for the stock.
mean(glm.pred==Direction.test)

library(MASS)

##Linear discriminant analysis
lda.fit=lda(Direction~Open+High+Low+Close+Volume,data=T,subset=train)

plot(lda.fit)

lda.fit

##Again vector to show the probabilitites for the test set for the desired contrast
lda.pred=predict(lda.fit,Tesla.test)
lda.class=lda.pred$class
table(lda.class,Direction.test)

mean(lda.class==Direction.test)

##Quadratic discriminant analysis for training dataset
qda.fit=qda(Direction~Open+High+Low+Close+Volume,data=T,subset=train)

qda.class=predict(qda.fit,Tesla.test)$class
table(qda.class,Direction.test)

mean(qda.class==Direction.test)

coef(lda.fit)

##The actual closing numbers from the day before for 2-5-2020
b=c(882.96,968.99,833.88,887.06,60775600)

##Using the prediction model to show if the next days activity should be positive or negative.
algorithm=predict(lda.fit,newdata=data.frame(Open=b[1],High=b[2],Low=b[3],Close=b[4],Volume=b[5]),type="response")

##The next days activity is predicted to be negative and was proven correct at the end of trading on 2-5-2020
##The stock was down 152.36 points at a percentage of (17.18%)
algorithm

