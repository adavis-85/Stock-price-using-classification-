# Stock price using classification

   In this project I analysed the activity of Tesla stock (TSLA) over the last year.  The stock is arranged by the date,
  opening price, highest price of the day, lowest price of the day, the closing price,the adjusted closing price of the day 
  adjusted for the activity of the day at closing and also the volume traded.  For our purposes we will not need the specific 
  date and the adjusted closing prices.  I will demonstrate how to use logistic regression and also linear discriminant analysis
  to see if the stock will increase or decrease for a day based on previous activity using R.  
      The data will need to be downloaded online and is in a csv file.  Here are the first ten entries:
 
 ```
       Date        Open  High   Low Close `Adj Close`  Volume
   <date>     <dbl> <dbl> <dbl> <dbl>       <dbl>   <dbl>
 1 2019-02-05  312.  322.  312.  321.        321. 6742800
 2 2019-02-06  320.  324.  316.  317.        317. 5038500
 3 2019-02-07  313.  315.  303   308.        308. 6520600
 4 2019-02-08  307.  307.  298.  306.        306. 5844200
 5 2019-02-11  312.  319.  310.  313.        313. 7129700
 6 2019-02-12  316.  318.  310.  312.        312. 5517600
 7 2019-02-13  312.  313.  306.  308.        308. 5141600
 8 2019-02-14  303.  307.  301   304.        304. 5200800
 9 2019-02-15  304.  308   304.  308.        308. 3904900
10 2019-02-19  307.  312.  305.  306.        306. 4168400

```

First a variable is needed by which to classify increasing and decreasing activity.  We will call this Direction.

```
Direction=rep(0,length(Close))
summary(Direction)

for (i in 2:253)
{
  if (Close[i]>Close[i-1])
  {
    Direction[i]=1
  }
}
```
The Direction column needs to be a part of the Tesla dataset so a new dataframe is needed.
```
T=data.frame(Direction,TSLA)
```
Now the correlation between the variables can be seen and the best ones can be chosen for our models.
The data set needs to be split up into a training and testing set.

```
Tesla.test=T[!train,]
dim(Tesla.test)
Direction.test=Direction[!train]
```

Then we try out a logistic regression.
  
  ```
glm.fits=glm(Direction~Open+High+Low+Close+Volume,data=T,family=binomial,subset=train)
glm.probs=predict(glm.fits,Tesla.test,type="response")
glm.pred=rep(0,54)
glm.pred[glm.probs>.5]=1
table(glm.pred,Direction.test)
```
Our results
```
        Direction.test
glm.pred  0  1
       0 16  8
       1  3 27
```
Now for the accuracy rate
```
mean(glm.pred==Direction.test)

0.7962963
```
  Now to test the linear discriminant analysis to see if the rate holds up.
```

lda.fit=lda(Direction~Open+High+Low+Close+Volume,data=T,subset=train)
lda.pred=predict(lda.fit,Tesla.test)
lda.class=lda.pred$class
table(lda.class,Direction.test)

        Direction.test
lda.class  0  1
        0 15  8
        1  4 27
        
mean(lda.class==Direction.test)
0.7777778
 ```
   So the accuracy rate for the linear discriminant analysis model is a little less accurate but this does qualify in a way
that the logistic regression was done correctly and that the activity does follow our models over the variables we have 
selected.  
    The next step is to use a quadratic discriminant analysis model to again find if the accuracy rate is better or worse 
 for our data
 ```
 qda.fit=qda(Direction~Open+High+Low+Close+Volume,data=T,subset=train)
 qda.class=predict(qda.fit,Tesla.test)$class
table(qda.class,Direction.test)

         Direction.test
qda.class  0  1
        0 14  8
        1  5 27
        
mean(qda.class==Direction.test)
0.7592593
```
   The accuracy is a little less accurate.  The linear discriminant analysis can be used for the actual activity of the 
stock for today.

```
b=c(882.96,968.99,833.88,887.06,60775600)

algorithm=predict(lda.fit,newdata=data.frame(Open=b[1],High=b[2],Low=b[3],Close=b[4],Volume=b[5]),type="response")

algorithm

$class
[1] 0
Levels: 0 1

$posterior
          0            1
1 0.9999984 1.558152e-06

$x
        LD1
1 -8.715686
```
Also for the logistic regression we can use the same entry for the previous day knowing today closed at a lesser
value than the day before

```
algorithm=predict(glm.fits,newdata=data.frame(Open=b[1],High=b[2],Low=b[3],Close=b[4],Volume=b[5]),type="response")

1 
1.259538e-07 
```
We can see that the probablity that a one was predicted is pretty close to zero which would mean that today was also
predicted to close down as did the linear discriminant analysis model.
   What these models can do is predict the day closing up or down without knowing the previous prices.  
The activity was predicted to be decreasing for today.  This was proven true as of closing of the market today
 Tesla stock was down 152.36 points or of a total value of 17.18% of the day before.  These models only predict what the 
 stock will do based off of the previous days prices hit and also the volume traded.  The date also could play a part such
 as if a new car(or truck) is unveiled or production starts or is finished on a current model.  All of these variables could 
 be taken into account as well as past history into building a reliable forecast to either buy or sell.  A way to use this example
 would be a to have a live price tracker and volume traded tracker to be able to predict activity in real time.  The close variable 
could be the present price and also the price that it was at the end of previous time period

   When there is a vector which has a classification that states if the next day was up or down we can predict what the 
next days activity will be.  Using linear discriminant analysis:
```
Next=rep(0,253)

for (i in 1:253)
{
if(Close[i]<Close[i+1])
{
  Next[i]=1
}
}

newT=data.frame(Next,TSLA)

newT.test=newT[!train,]
Next.test=Next[!train]

lda.fits=lda(Next~Open+High+Volume,data=newT,subset=train)
lda.pred=predict(lda.fit,newT.test)
lda.class=lda.pred$class
table(lda.class,Next.test)
mean(lda.class==Next.test)
```
   This model used the same training and test set.  The matrix and accuracy percentage is:
```
       Next.test
lda.class  0  1
        0 10 13
        1  9 22
        
[1] 0.5925926
```
   So the accuracy rate of going up or down for the next day is around 60%.  Testing on the information from two days ago and also today achieved the classification of:
```
guess1=predict(lda.fits,newdata=data.frame(Open=882.96,High=968.99,Volume=60775600),type="response")
guess1$class

[1] 0

guess2=predict(lda.fits,newdata=data.frame(Open=823.26,High=845.98,Volume=37343093),type="response")
guess2$class

[1] 0
```
   Guess1 was predicted correctly.  Guess2 was not predicted correctly but that is what happens when there is an error rate of 40% and an accuracy of 60%.  The Close column was also dropped.  This was because the model could be used for a real time prediction on about the same error rate on whatever time period is desired.  In real time there won't have a Close price because the stock is in the process of being traded.  Here we used days but the model could be fitted on hours or minutes or seconds.  
