#Assignment # 2 

#1o. Created matrix for data set. First column is height and second is head circumference.

# Note: All plots can be viewed within Quartz history (moving backward and forward)

Data = matrix (c(27.75, 24.5, 25.5, 26, 25, 27.75, 26.5, 27, 26.75, 26.75, 27.5,17.5, 17.1, 17.1, 17.3, 16.9, 17.6, 17.3, 17.5, 17.3, 17.5, 17.5),
nrow=11, ncol=2)

#Stored x column as height and y column as circ.

Height = Data[1:11,1]

Circ = Data[1:11, 2]

#1a.Creating a plot/graph for the matrix created above. 

#Labels x and y axis, assumed height and circumference measured in units of cm given babies are very small. 

plot(Height, Circ, main = "Height vs Head Circumference of 11, 3 Year Olds", xlab = "Height[cm]", ylab = "Head Circumference[cm]")

#1.b

cor.test(Height,Circ)
cov(Height,Circ)

#correlation is 0.9110727 and covariance of 0.2188636 suggesting that there is a positive relationship between these two variable, however given that the covariance is a small positive number it is a weak positive relationship. 

#2a. Creating a linear fit into the graph created above using a least squares regression. 

linfit <- lm(Circ~Height) 

#fit returns the following, Coefficients: (y-Intercept) -52.254, slope = 4.542 

abline(linfit, col="red") 

#3.a Computing the cooks distance 

CookDist = cooks.distance(linfit) 
plot (Height, CookDist, main = "Calculated Cooks Distance vs Height for the Linear Fit Model")
cleanHeight = c(Height[1], Height[3:4] , Height[6:11])
cleanCirc = c(Circ[1], Circ[3:4], Circ[6:11])
newfit <- lm(cleanCirc~cleanHeight)
plot(cleanHeight, cleanCirc, main = "New Linear Fit Model with Influential Points Removed")
abline(newfit, col = "blue")

cor.test(cleanHeight,cleanCirc)
cov(cleanHeight,cleanCirc)

# The cooks distances revealed two influential points (outliars), that were deleted. The new fit model reveals suprisingly that the correlation and covariance both decreased once the ouliars were removed. This shows that the relationship between these two variables of height of baby and the circumference of the head is still positively related by is even weaker once removing the influential points. 

#3b. 

# Calculating residuals for the original linear fit model. 

resid(linfit) 
par (mfrow =c(2,2) )
plot (Height, linfit $ res, main = "Residual vs. Height for Original Linear Fit Model" )
plot (Circ, linfit $ res, main = "Residual vs. Head Circumference" )
plot (linfit$fitted, linfit $ res, main = "Fitted Data vs. Residual")
hist(linfit$res, main = "Histogram of Residuals of the Linear Fit Model" ) 

# Observation: After plotting the residuals it can be seen that the points are scatters spaciously around zero. 

#3c. 

resid(newfit)
plot (cleanHeight, newfit $ res, main = "Residual vs. Height for New Fitted Linear Model" )
plot (cleanCirc, newfit $ res, main = "Residual vs. Head Circumference") 
plot (newfit$fitted, newfit $ res, main = "Fitted Data vs. Residual ") 
hist(newfit$res, main = "Histogram of Residuals of New Linear Fit Model")

# The residuals in this new fit model are scattered closely towards zero. This signifies a more stronger relationship. 

#3d. The assumption that removing the outliars would give a better understanding of the relationship was shown through a lower correlation and lower covariance. However the residual data showed a more predicted value given its closer proximity of points towards zero. 
#4a. Creating a Quadratic Fit Model for the Original Data 

Height2 = Height^2
quadfit <- lm(Circ~ Height + Height2) 

summary(quadfit) 

Heightvalues <- seq(0, 30, 0.1) 
predictedCirc <- predict(quadfit, list(Height=Heightvalues, Height2=Heightvalues^2)) 
par(mfrow=c(1,1))
plot(Height, Circ, main = "Original Data with a Quadratic Fit Model")
lines(Heightvalues, predictedCirc, col = "darkgreen")



