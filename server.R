###############################
#
# Shiny server code for educational application about linear correlation
#       The correlation coefficient can be changed by the user and he can see the effect on two plots
#                                             1. scatter plot + regression line
#                                             2. representation of the residuals
#       Example 1 : illustration of the correlation coefficient on linear data
#       Examples 2 -> 5 : illustrations of the behavior of the coefficient with "unusual" data
# Simon Benateau 09.10.2014
#           
###############################

# package
library(shiny)

shinyServer(function(input, output) {
  output$CorrelationPlot = renderPlot({
    
    # Get data from user interface
    ###############################
    
    n       = 20                    # length of vector (number of points on the graph)
    rho     = input$corr            # desired correlation (chosen by the user)
    example = input$select          # type of relationship between x and y
    
    # Initial distribution for the different examples
    ###############################
    
    if (example %in% 1:4) x = seq(1,10,length.out=n) else x = c(rep(0,n-1),1)   #distribution of x
    
    if(rho %in% c(1,-1)){ #solve bug with rho = 1
      y = x*sign(rho)
      
    } else {
      if (example == 1 | example == 5 ) x2 = rnorm(n, 2, 0.5)                       # linear relationship
      if (example == 2 ) x2 = log(x) +  rnorm(n, 0, 0.05)                           # log relationship
      if (example == 3 ) x2 = exp(x) +  rnorm(n, 0, 500)                            # exponential relationship
      if (example == 4 ) {x2 = x ; x2[n-1] = x[n-1]*3}                              # linear relationship with one outlier
      
      # transformation of the x values to get the desired correlation
      ###############################
      
      theta = acos(rho)                                # corresponding angle
      X     = cbind(x, x2)                             # matrix
      Xctr  = scale(X, center=TRUE, scale=FALSE)       # centered columns (mean 0)
      Id    = diag(n)                                  # identity matrix
      Q     = qr.Q(qr(Xctr[ , 1, drop=FALSE]))         # QR-decomposition, just matrix Q
      P     = tcrossprod(Q)          # = Q Q'          # projection onto space defined by x
      x2o   = (Id-P) %*% Xctr[ , 2]                    # x2ctr made orthogonal to xctr
      Xc2   = cbind(Xctr[ , 1], x2o)                   # bind to matrix
      Y     = Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))     # scale columns to length 1
      y     = Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
    }
    
    ###############################
    # Graphs
    ###############################
    par(mfrow=c(1,2))
    
    # rescale data to get similar range
    y = y*2/diff(range(y))
    
    
    # Scatter plot + regression + coefficients
    ###############################
    
    # Scatter plot
    plot(y~x,type="n")
    title("Scatter plot")
    if(input$corr > 0.3 || input$corr < -0.3  ){ # Remove the line between -0.3 and 0.3 (confusion)
      abline(lm(y~x),col="orange",lwd=3)
    }
    
    # Add coefficients on the graph
    lmRes = lm(y ~ x) 
    
    # change side of the legend for clearer plot
    if(rho > 0){
      side = "topleft"
      position = 4
    } else {
      side = "topright"
      position = 2
    }
    
    # get coordinate and plot legend
    co =legend(side ,legend = c("r","h"),bty="n",text.col="white")
    text(co$text$x[1],co$text$y[1],paste("r = ",round(cor(y,x),5)),pos=position)
    text(co$text$x[2],co$text$y[2],substitute(R^2 == h,list(h = round(summary(lmRes)$r.squared,3))),pos=position)
    
    # add points to graph
    points(y~x,pch=21,bg="forestgreen",cex=1.7)
    
    # Residuals plot
    ###############################
    
    lmResResiduals = (y - fitted(lmRes)) # calculation
    if (rho == 1 ||rho == -1 ) lmResResiduals = rep(0,n) # correct bug with r = 1
    plot(lmResResiduals,type="p", xlab = "x", ylab = "residuals")
    title("Residuals")
    points(lmResResiduals,type="h",lty=2)
    abline(h=0)
    
  }) # end render plot
}) # end shiny server function