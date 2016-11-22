#Assignment # 2 Problem 2 

# Note: All plots can be viewed within Quartz history (moving backward and forward)

#2a. Grabbed the cars data from R. 

data(cars)
cars

# organizing data into two sets of lists, x variable as Distance and y variable as speed. 

speed <- list()
speed <- cars$speed
Dist <- list()
Dist <- cars$dist  

#plotting the data

plot(speed, Dist, main = "Speed of Car vs. Distance to Stop", xlab = "Distance", ylab = "speed") 

cor.test(speed,Dist)
cov(speed, Dist)

#Determining the correlation between the two variables, distance and speed. The correlation and covariance, 0.8068949 and 109.9469 respectively, are positive numbers suggesting a direct relationship. Furthermore, the relatively large covariance suggests a strong positive relationship. The greater the speed of the car, the greater distance needed to make a stop. 

# Creating a linear fit into the graph created above using a least squares regression. 

linFit <- lm(Dist~speed)

abline(linFit, col="blue") 

# Computing cooks distance 

CookDist = cooks.distance(linFit)
plot(speed,CookDist, main = "Calculated Cooks Distances vs. Speed for the Linear Fit Model")

#Observation of the graph shows one major influential point (ouliar). This will be deleted from the dataset to see any changes in the relationship. 

newSpeed = c(speed[1:48], speed[50])
newDist = c(Dist[1:48], Dist[50])

newFit <- lm(newDist~newSpeed)
plot(newSpeed, newDist, main = "New Linear Fit Model with Influential Points Removed")
abline(newFit, col = "red")

cor.test(newSpeed,newDist)
cov(newSpeed,newDist)

# It seems removing the outliar has made both the correlation and covariance smaller, however only by a small amount. 

# Calculating residuals of the original linear model. 

resid(linFit)
par(mfrow = c(2,2))
plot(speed, linFit$res, main = "Residual vs. Speed for Original Linear Fit Model")
plot(Dist, linFit$res, main = "Residual vs. Distance to Stop")
plot(linFit$fitted, linFit$res, main = "Fitted Data vs. Residual")
hist(linFit$res, main = "Histogram of Residuals of the Linear Fit Model")

# Calculating the residuals of the new model after removing the outliars
resid(newFit)
plot(newSpeed, newFit$res, main = "Residual vs. Speed for New Fitted Linear Model")
plot(newDist, newFit$res, main = "Residuals vs. Distance to Stop")
plot(newFit$fitted, newFit$res, main = "Fitted Data vs. Residual")
hist(newFit$res, main = "Histogram of Residuals of New Linear Fit Model")

# Observations: After removing the outliar and calculating the residuals, the information is scattered closer around the value of zero. 

# Creating a quadratic fit model, to see if it fits better with the data. 

speed2 = speed^2
Quadfit <- lm(Dist~speed + speed^2)

summary(Quadfit)

Speedvalues <- seq(0, 25, 0.1)
predictedDist <- predict(Quadfit, list(speed=Speedvalues, speed2=Speedvalues^2))
par(mfrow=c(1,1))
plot(speed, Dist, main = "Originial Data with a Quadratic Fit Model")

lines(Speedvalues, predictedDist, col = "darkgreen")

# Observations: the quadratic model seems to fit better with the data set provided with respect to the two variables, speed and distance.

# 2b. Creating Interactive plotting for the Linear Regression Model of 2a. 

library(plotly)

p1 <- ggplot(cars, aes(x=speed, y=Dist)) +
             geom_point(shape=0.25)  +
             geom_smooth(method=lm) + ggtitle("Linear Fit Model of Speed of Cars vs. Stopping Distance") 
 
ggplotly(p1)

# file:///var/folders/g1/rv4v5j_569d3p7ljltfv92dw0000gn/T/RtmpFPpcmG/viewhtml37516785f3e9/index.html

# Interactive Plotting for New Fit Model 

newcars <- data.frame(x=newSpeed,y=newDist)
p2 <- ggplot(newcars, aes(x=newSpeed, y=newDist)) +
                 geom_point(shape=0.25) + 
                 geom_smooth(method=lm) + ggtitle("New Linear Fit Model of Car Speeds vs. Stopping Distance")
ggplotly(p2)

# file:///var/folders/g1/rv4v5j_569d3p7ljltfv92dw0000gn/T/RtmpkFM1As/viewhtml3677396bbac2/index.html



