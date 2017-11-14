#DESCRIPTIVE ANALYSIS
#-----------------------------------------------------------------------------

#MEAN

Mean<-function(arr,len)
{
   sum=0

   for(i in 1:len)
   {
     sum=sum+arr[i]
   }

   mean=sum/len
   return(mean)
}
#-------------------------------------------------------------------------------
#MEDIAN

Median<-function(arr,len)
{
   arr1=arr
   for(i in 1:(len-1))
   {
     for(j in 1:(len-i))
     {
        if(arr1[j]>arr1[j+1])
        {
           temp=arr1[j]
           arr1[j]=arr1[j+1]
           arr1[j+1]=temp
        }
      }
   }

  if(len%%2==0)
  {
    o1<-len/2
    o2<-(len/2+1)
    median<-(arr1[o1]+arr1[o2])/2
  }
  if(len%%2!=0)
  {
   obs<-((len+1)/2)
   median<-arr1[obs]
  }
  return(median)
}

#-----------------------------------------------------------------------------
#MODE

Mode<-function(arr,len)
{
  ux <- unique(arr)
  mode<-ux[which.max(tabulate(match(arr, ux)))]
  return(mode)
}

#------------------------------------------------------------------
#VARIANCE

Variance<-function(arr,len)
{
  sum=0
  mean=Mean(arr,len)
  for(i in 1:len)
  {
    sum=sum+(arr[i]-mean)*(arr[i]-mean)

  }

  variance=sum/(len-1)
  return(variance)
}

#------------------------------------------------------------------------
#STANDARD DEVIATION

Sd<-function(arr,len)
{
  variance<-Variance(arr,len)
  sd=sqrt(variance)
  return(sd)
}
#------------------------------------------------------------------------------
#MEAN ABSOLUTE DEVIATION

Mad<-function(arr,len)
{
  sum=0
  for(i in 1:len)
  {
   sum=sum+abs(arr[i]-mean)
  }
  mad=sum/len
  return(mad)
}
#----------------------------------------------------------------------------------

#QUARTILES #IQR

quart <- function(arr,len) {
  x <- sort(arr)
  m <- (len+1)/2
  if (floor(m) != m) {
    l <- m-1/2; u <- m+1/2
  } else {
    l <- m-1; u <- m+1
  }

  q1=Median(x[1:l],length(x[1:l]))
  q3=Median(x[u:len],length(x[u:len]))
  iqr=q3-q1
  print("q1 quartile is : " )
  print(q1)
  print("q3 quartile is : ")
  print(q3)
  message(sprintf("Interquartile range is :%.10f",iqr))
  
}
#-------------------------------------------------------------------------------
#RANGE

Range<-function(arr,len)
{
  max=arr[1]

  for(i in 1:len)
  {
    if(max<arr[i])
       max=arr[i]
  }

  min=arr[1]

  for(i in 1:len)
  {
    if(min>arr[i])
       min=arr[i]
  }
  message(sprintf("range is  from %.10f to %.10f",min,max))

}


#------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#MINIMUM

Minimum<-function(arr,len)
{
  min=arr[1]

  for(i in 1:len)
  {
    if(min>arr[i])
       min=arr[i]
  }
  return(min)
}
#---------------------------------------------------------------------------------------
#MAXIMUM

Maximum<-function(arr,len)
{
  max=arr[1]

  for(i in 1:len)
  {
    if(max<arr[i])
       max=arr[i]
  }
  return(max)

}
#------------------------------------------------------------------------------
#MOMENTS



#CENTRAL MOMENT

central_moment<-function(arr,len,k)
{
   mean<-Mean(arr,len)
   sum=0
   for(i in 1:len)
   {
      l=(arr[i]-mean)**k
      sum=sum+l
   }
   moment=sum/len
   message(sprintf("%.f central moment is : %.10f",k,moment))
   return(moment)

}
#----------------------------------------------------------------------------
#SKEWNESS

Skewness<-function(arr,len)
{
   u2=central_moment(arr,len,2)
   u3=central_moment(arr,len,3)
   skewness<-(u3)/(u2**1.5)
   return(skewness)
}
#---------------------------------------------------------------------------
#KURTOSIS

Kurtosis<-function(arr,len)
{
   u2=central_moment(arr,len,2)
   u4=central_moment(arr,len,4)
   kurtosis<-(u4/(u2**2))-3
   return(kurtosis)
}
#--------------------------------------------------------------------------------

#PREDICTIVE ANALYSIS



#CORRELATION

#PEARSON

Cor<-function(arr1,arr2,len1,len2)
{
  sum=0
  
  meanX<-Mean(arr1,len1)
  meanY<-Mean(arr2,len2)
  
  i=1
  j=1
  while(i<=len1 & j<=len2)
  {
    l=(arr1[i]-meanX)*(arr2[j]-meanY)
    sum=sum+l
    i=i+1
    j=j+1
  }
  
  covariance=sum/(len1-1)
  
  sdX<-Sd(arr1,len1)
  sdY<-Sd(arr2,len2)
  
  corr<-covariance/(sdX*sdY)
  message(sprintf("Correlation is : %.10f",corr))
  return(corr)
  
  
}

#====================================================================

#PROBABILITY ANALYSIS

#PERMUTATION


per <- function(n1,x1)
{
  if(x1>n1)
  {
    print("Number of sample(x1) can't be gretor than number of objects(n1)")
  }
  else
  {
    per=fact(n1) / fact(n1-x1)
    message(sprintf("Permutation is :%.10f",per))
  }
  
}


#COMBINATION


comb<-function(n1, x1) 
{
  if(x1>n1){
    print("Number of sample(x1) can't be gretor than number of objects(n1)")
  }
  else{
    co=fact(n1) / (fact(x1) * fact(n1-x1))
    message(sprintf("Combination is :%.10f",co))
  }
  
}



#BASIC PROBABILITY

prob<-function(fav_outcome,possible_outcome)
{
  probability<-fav_outcome/possible_outcome
  if(probability>1){
    print('probability can not be greator than 1')
  }
  else{
    message(sprintf("Probability is :%.10f",probability))
  }
  
}


#CONDITIONAL PROBABILITY


conditional<-function(ProbA,ProbB,Prob_AandB)
{
  
  Prob_AgivenB=Prob_AandB/ProbB
  message(sprintf("Conditional probability is :%.10f",Prob_AgivenB))
  
}




#-------------------------------------------------------------------------------


#BAYES THEOREM

bayes<-function(ProbB1,ProbB2,ProbB3,Prob_AgivenB1,Prob_AgivenB2,Prob_AgivenB3)
{
  
  Prob_B2givenA=(Prob_AgivenB2*ProbB2)/((Prob_AgivenB2*ProbB2)+(Prob_AgivenB1*ProbB1)+(Prob_AgivenB3*ProbB3))
  message(sprintf("Probability is : %.10f",Prob_B2givenA))
  
}

#DISTRIBUTIONS


fact<-function(x)
{
  if(x==0)
    return(1)
  else
  {
    f=1
    for(i in 1:x)
    {
      f=f*i
    }
    return(f)
  }
  
}



#--------------------------------------------------------------------------------
#BERNOULLI DISTRIBUTION



bernoulli<-function(x,p)
{
  if(x==0 | x==1)
  {
    prob=(p**x)*((1-p)**(1-x))
  }
  else
    return(0)
  return(prob)
}


#--------------------------------------------------------------------------------

#BINOMIAL DISTRIBUTION

binomial<-function(x,n,p)
{
  prob=(fact(n)*(p**x)*((1-p)**(n-x)))/(fact(n-x)*fact(x))
  return(prob)
  
}



#-------------------------------------------------------------------------------------------------

#GEOMETRIC DISTRIBUTION

geometric<-function(x,p)
{
  prob=p*((1-p)**(x-1))
  return(prob)
  
}


#----------------------------------------------------------------------------------------------------

#DISCRETE UNIFORM DISTRIBUTION

uniformd<-function(k)
{
  prob=1/k
  return(prob)
  
}

  #--------------------------------------------------------------------------------------------
  
  
  #POISSON DISTRIBUTION
  
  
  poisson<-function(x,lambda)
  {
    prob=((lambda**x)*(2.718281828**(-lambda)))/fact(x)
    return(prob)
    
  }
  
  
  
  
  #--------------------------------------------------------------------------------
  
  #MULTINOMIAL DISTRIBUTION
  
  
  
  
  multinomial<-function(x1,x2,x3,n,p1,p2,p3)
  {
    prob=(fact(n)*(p1**x1)*(p2**x2)*(p3**x3))/(fact(x1)*fact(x2)*fact(x3))
    return(prob)
    
  }
  
  
  
  
  #dmulti function library
  
  #---------------------------------------------------------------------------------
  
  
  #MULTIVARIATE HYPERGEOMETRIC DISTRIBUTION
  
  
  multivariate<-function(x1,x2,x3,n,m1,m2,m3)
  {
    N=m1+m2+m3
    prob=((fact(m1)/(fact(m1-x1)*fact(x1)))*(fact(m2)/(fact(m2-x2)*fact(x2)))*(fact(m3)/(fact(m3-x3)*fact(x3))))/(fact(N)/(fact(N-n)*fact(n)))
    return(prob)
    
  }
  
  
  
  #---------------------------------------------------------------------------------------------
  
  
  #NEGATIVE BINOMIAL
  
  negative<-function(x,k,p)
  {
    m=x-1
    n=k-1
    prob=(fact(m)/(fact(m-n)*fact(n)))*((p**k)*((1-p)**(x-k)))
    return(prob)
    
  }
  
  
  
  
  #--------------------------------------------------------------------------------------------
  
  
  #HYPERGEOMETRIC
  
  hyper<-function(x,n,N,M)
  {
    a=N-M
    b=n-x
    prob=(fact(M)*fact(a)*fact(N-n)*fact(n))/(fact(M-x)*fact(x)*fact(a-b)*fact(b)*fact(N))
    return(prob)
    
  }
  
  
  
  
  #----------------------------------------------------------------------------------------------------------------------------
  
  #CONTINUOUS DISTRIBUTIONS
  
  
  #NORMAL DISTRIBUTION
  
  
  normal<-function(x,mean,sd)
  {
    pdf=(2.71^(-0.5*(((x-mean)/sd)^2)))/(sd*sqrt(2*pi))
    message(sprintf("Probability density is %.10f",pdf))
  }   
  
  
  
  
  #---------------------------------------------------------------------------------------------------------------
  
  
  #EXPONENTIAL DISTRIBUTION
  
  expo<-function(alpha)
  {
    al=alpha
    k<-function(y,al){ alpha*(2.71^(-(alpha*y))) }
    l<-(integrate(k,lower=0,upper=1/6))$value
    message(sprintf("Pdf of exponential dist is %.10f",l))
  }
  
  
  
  
  #GAMMA DISTRIBUTION
  
  gamma<-function(x,alpha,beta)
  { 
    al=alpha
    k<-function(y,al){ y^(alpha-1)*(2.71^(-y)) }
    l<-(integrate(k,lower=0,upper=Inf))$value
    g<-(((x^(alpha-1))*(2.71^(-x/beta)))/(beta^alpha)*l)
    message(sprintf("Pdf of gamma dist is %.10f",g))
    
  }
  
  
  
  
  
  #UNIFORM DISTRIBUTION
  
  uniform<-function(x,alpha,beta)
  {
  }   
  
  
  #BIVARIATE NORMAL DISTRIBUTION
  
  bivariate<-function(x1,x2)
  {
    sd_x1<-Sd(x1,length(x1))
    sd_x2<-Sd(x2,length(x2))
    p=Cor(x1,x2,length(x1),length(x2))
    mu1=Mean(x1,length(x1))
    mu2=Mean(x2,length(x2))
    z=(((x1-mu1)^2)/(sd_x1^2))-((2*p*(x1-mu1)*(x2-mu2))/(sd_x1*sd_x2))+(((x2-mu2)^2)/(sd_x2^2))   
    pdf=(2.71^((-z)/(2*(1-p)^2)))/(2*pi*sd_x1*sd_x2*sqrt((1-p)^2))
    message(sprintf("\nPdf of exponential dist is %.10f",pdf))
    
    
  }
  
  
  #INTERVAL ESTIMATION
  
  #Estimation of Means
  
  E_mean<-function(n,alpha,sigma)
  {
    z_alphaby2=2.575
    e=(z_alphaby2*sigma)/sqrt(n)
    message(sprintf("The prob is %.4f that the error will be less than %.10f",1-alpha,e))
    
  }
  
  
  #Estimation of Differences in Means
  
  E_diff<-function(x1,x2,sigma1,sigma2,n1,n2,alpha)
  {
    z_alphaby2=1.88
    z_left=(x1-x2)-z_alphaby2*(sqrt((sigma1^2)/n1+(sigma2^2)/n2))
    z_right=(x1-x2)+z_alphaby2*(sqrt((sigma1^2)/n1+(sigma2^2)/n2))
    message(sprintf("There is %.f percent confidence that interval from %.5f to %.5f contains the actual difference between two means",(1-alpha)*100,z_left,z_right))  
    
  }
  
  
  #Estimation of Proportions
  
  E_prop<-function(x,n,alpha)
  {
    p=x/n
    z_alphaby2=2.575
    e=z_alphaby2*sqrt((p*(1-p))/n)
    message(sprintf("We can assert with %.f percent confidence that the error is less than %.5f",(1-alpha)*100,e))
    
  }
  
  
  #Estimation of Differences in Proportions
  
  E_diffprop<-function(x1,x2,n1,n2,alpha)
  {
    p1=x1/n1
    p2=x2/n2
    z_alpha=2.575
    e_left=(p1-p2)-z_alpha*sqrt(((p1*(1-p1))/n1)+((p2*(1-p2))/n2))
    e_right=(p1-p2)+z_alpha*sqrt(((p1*(1-p1))/n1)+((p2*(1-p2))/n2))
    message(sprintf("Interval from %.5f from to %.5f is an approximate %.f confidence interval for differnce between the proportions",e_left,e_right,(1-alpha)*100))
    
  }
  
  
  
  
  #ESTIMATION OF VARIANCES
  
  E_var<-function(n,s,alpha)
  {
    chi1=32.801
    chi2=4.601
    e_left=(n-1)*(s^2)/chi1
    e_right=(n-1)*(s^2)/chi2
    message(sprintf("Interval from %.5f to %.5f is a %.f percent confidence interval for sigma_square",e_left,e_right,(1-alpha)*100))
    
    
  }
  
  
  
  
  #Estimation of Ratio of Two Variances
  
  
  E_ratio<-function(s1,s2,alpha)
  {
    f1=6.72
    f2=5.61
    e_left=(s1^2)/((s2^2)*f1)
    e_right=((s1^2)*f2)/(s2^2)
    message(sprintf("Interval from %.5f to %.5f is a %.f percent confidence interval for sigma1/sigma2",e_left,e_right,(1-alpha)*100))
    
  }
  
  
  #NON-PARAMETRIC ANALYSIS
  
  #SIGN TEST
  
  sign<-function(sample,mu,alpha)
  {
    count_neg=0
    count_pos=0
    n=length(sample)
    for(i in 1:n)
    {
      if(sample[i]<mu)
        count_neg=count_neg+1
      if(sample[i]==mu)
        n=n-1
      else
        count_pos=count_pos+1
    }
    if(n>30)
      value=(count_neg-(n*0.5))/sqrt(n*0.5*0.5)
    if(n<=30)
      value=binomial(count_neg,n,0.5)
    print("Value of sign test statistic is ")
    print(value)
    if(alpha>value)
      print("Null hypothesis is rejected ")
    else
      print("Null hypothesis is accepted ")
    
  }
  
  
  #-------------------------------------------------------------------------------------------------
  
  #Wilcoxon Signed-Rank test
  
  
  sort<-function(arr,len)
  {
    for(i in 1:(len-1))
    {
      for(j in 1:(len-i))
      {
        if(arr[j]>arr[j+1])
        {
          temp=arr[j]
          arr[j]=arr[j+1]
          arr[j+1]=temp
        }
      }
    }
    return(arr)
    
  }
  
  
  
  
  
  
  
  
  wilcoxon<-function(sample,mu,T_alpha)
  {
    arr<-c()
    arr2<-c()
    arr3<-c()
    rank<-c()
    n<-length(sample)
    for(i in 1:n)
    {
      k=sample[i]-mu
      if(k>0 | k<0)
        arr[i]=k
      
    }
    arr <- arr[!is.na(arr)]
    n=length(arr)
    for(i in 1:n)
    {
      arr2[i]=abs(arr[i])
    }
    arr3<-sort(arr2,n)
    k=1
    for(i in 1:n)
    {
      for(j in 1:n)
      {
        if(arr2[i]==arr3[j])
        { 
          rank[k]=j
          k=k+1
        }
      }
    }
    T_neg=0
    T_pos=0
    for(i in 1:n)
    {
      
      if(arr[i]<0)
        T_neg=T_neg+rank[i]
      else
        T_pos=T_pos+rank[i]
    }
    T=min(T_neg,T_pos)
    print("Value of T statistic is ")
    print(T)
    if(T_alpha>T)
      print("Null hypothesis is rejected ")
    else
      print("Null hypothesis is accepted ")
    
    
  }
  
  #--------------------------------------------------------------------------------------------
  
  #Mann-Whitney Test
  
  whitney<-function(sample1,sample2,U_alpha,side)
  {
    sample<-c(sample1,sample2)
    arr<-sort(sample,length(sample))
    
    w1=0
    n=length(sample)
    n1=length(sample1)
    for(i in 1:n1)
    {
      for(j in 1:n)
      {
        if(sample1[i]==arr[j])
        { 
          w1=w1+j
        }
      }
      
    }
    w2=0
    n=length(sample)
    n2=length(sample2)
    for(i in 1:n2)
    {
      for(j in 1:n)
      {
        if(sample2[i]==arr[j])
        { 
          w2=w2+j
        }
      }
      
    }
    if(side=="left")
    {
      u1=w1-(n1*(n1+1))/2
      print("Value of U statistic is ")
      print(u1)
      if(U_alpha>=u1)
        print("Null hypothesis is rejected ")
      else
        print("Null hypothesis is accepted ")
    }
    
    
    if(side=="right")
    {
      u2=w2-(n2*(n2+1))/2
      print("Value of U statistic is ")
      print(u2)
      if(U_alpha>=u2)
        print("Null hypothesis is rejected ")
      else
        print("Null hypothesis is accepted ")
    }
    
    
  }
  
  
  
  
  #=========================================================================================
  
  
  #Kruskal-Wallis Test
  
  
  kruskal<-function(s1,s2,s3,chi_alpha)
  {
    
    sample<-c(s1,s2,s3)
    arr<-sort(sample,length(sample))
    
    r1=0
    n=length(sample)
    n1=length(s1)
    for(i in 1:n1)
    {
      for(j in 1:n)
      {
        if(s1[i]==arr[j])
        { 
          r1=r1+j
        }
      }
      
    }
    r2=0
    n=length(sample)
    n2=length(s2)
    for(i in 1:n2)
    {
      for(j in 1:n)
      {
        if(s2[i]==arr[j])
        { 
          r2=r2+j
        }
      }
      
    }
    r3=0
    n=length(sample)
    n3=length(s3)
    for(i in 1:n3)
    {
      for(j in 1:n)
      {
        if(s3[i]==arr[j])
        { 
          r3=r3+j
        }
      }
      
    }
    R_sum=((r1*r1)/n1+(r2*r2)/n2+(r3*r3)/n3)
    H=((12*R_sum)/(n*(n+1)))-3*(n+1)
    print("Value of H statistic is ")
    print(H)
    if(chi_alpha>=H)
      print("Null hypothesis is rejected ")
    else
      print("Null hypothesis is accepted ")
    
  }
  
  #CHI-SQUARE TEST
  
  
  
  chi_square<-function(tbl,alpha,df)
  {
    p_value=dchisq(alpha, df)
    
    chi=0
    
    grand_total=0
    for(i in 1:nrow(tbl))
    {
      grand_total=grand_total+sum(tbl[i,])
    }
    
    for(i in 1:nrow(tbl))
    {
      for(j in 1:ncol(tbl))
      {
        e=(sum(tbl[,j])*sum(tbl[i,]))/grand_total
        chi=chi+(((tbl[i,j]-e)*(tbl[i,j]-e))/e)
        
      }
    }
    message(sprintf("Degree of freedom :%.f",df))
    message(sprintf("Chi square value is :%.10f",chi))
    if(p_value>alpha)
      print("Null hypothesis is accepted")
    else
      print("Null hypothesis is rejected")
    
  }
  
  
  
  #--------------------------------------------------------------------------------------------------
  
  #Student t-test
  
  
  #EXAMPLE
  
  
  
  t_test<-function(l,mu,alpha)
  {
    p_value=0.158
    
    mean=Mean(l,length(l))
    
    sd<-Sd(l,length(l))
    
    n=10
    
    t=((mean-mu)*sqrt(n))/sd
    
    df=length(l)-1
    message(sprintf("Degree of freedom :%.f",df))
    message(sprintf("t test value is :%.10f",t))
    if(p_value>alpha)
      print("Null hypothesis is accepted")
    else
      print("Null hypothesis is rejected")
    
    
    
  }
  
  
  #------------------------------------------------------------------------------------------------
  
  #Z-TEST
  
  
  #EXAMPLE
  
  
  z_test<-function(a,mu,sd,alpha)
  {
    
    z.half.alpha = qnorm(1???alpha/2)  
    mean_a=Mean(a,length(a))
    n=length(a)
    ztest = ((mean_a - mu)*sqrt(n))/sqrt(var)
    sprintf("z test value is :%.10f",ztest)
    if(z.half.alpha>alpha)
      print("Null hypothesis is accepted")
    else
      print("Null hypothesis is rejected")
    
    
  }
  
  
  
  
  
  #-----------------------------------------------------------------------------------------------------------
  
  
  #F-TEST
  
  
  f_test<-function(X,Y,alpha)
  {
    p_value=0.56
    var_X=Variance(X,length(X))
    var_Y=Variance(Y,length(Y))
    n1=length(X)
    n2=length(Y)
    v1=n1-1
    v2=n2-1
    
    ftest=(var_X**2)/(var_Y**2)
    message(sprintf("F test value :%.10f",ftest))
    if(p_value>alpha)
      print("Null hypothesis is accepted")
    else
      print("Null hypothesis is rejected")
    
    
  }    
  
  
  
  
  
  
  
  
  
  #-----------------------------------------------------------------------------
  
  
  
  
  




setwd("C:/Users/HP/Desktop")
     
repeat
{
   print("=========================MENU==================================")
   print("1.Descriptive Analysis")
   print("2.Predictive Analysis")
   print("3.Probability Analysis")
   print("4.Discrete Distribution Functions")
   print("5.Continuous Distribution Functions")
   print("6.Sample Distribution Test Statistic")
   print("7.Interval Estimation")
   print("8.Non-Parametric Analysis")
   print("9.Visualizations")
   inp=as.numeric(readline(prompt="Enter your choice: "))
   if(inp==1)
   {
      
      data1<-read.csv("data.csv",header=TRUE)
      
      arr<-c(data1$X)
      len<-length(arr)
      repeat
      {
    
    	print("1.Mean, 2.Median, 3.Mode, 4.Variance,5. Standard Deviation,6. Mean Absolute Deviation, 
              7. Range, 8.Quartiles and IQR, 9.Minimum, 10.Maximum, 11.Skewness,12. Kurtosis,13. Moments")
      
      ch=as.numeric(readline(prompt="Enter your choice: "))
      if(ch==1)
      {
          mean<-Mean(arr,len)
          message(sprintf("mean is :%.10f",mean))
      }
      if(ch==2)
      { 
          median<-Median(arr,len)
          message(sprintf("median is :%.10f",median))
      }
      if(ch==3)
      {

          mode<-Mode(arr,len)
	        message(sprintf("mode is :%.10f",mode))
      }
      if(ch==4)
      {

          variance<-Variance(arr,len)
          message(sprintf("variance is :%.10f",variance))
      }
      if(ch==5)
      {

          sd<-Sd(arr,len)
          message(sprintf("Standard deviation is :%.10f",sd))
      }
      if(ch==6)
      {

          mad<-Mad(arr,len)
          message(sprintf("mean absolute deviation is :%.10f",mad))
      }
      if(ch==7)
           Range(arr,len)
      if(ch==8)
           quart(arr,len)
      if(ch==9)
      {

           min<-Minimum(arr,len)
           message(sprintf("minimum is :%.10f",min))
      }
      if(ch==10)
      {

           max<-Maximum(arr,len)
           message(sprintf("maximum is :%.10f",max))
      }
      if(ch==11)
      {
           skewness<-Skewness(arr,len)
           message(sprintf("skewness is : %.10f",skewness))
      }
      if(ch==12)
      {
            kurtosis<-Kurtosis(arr,len)
            message(sprintf("kurtosis is : %.10f",kurtosis))
      }
      if(ch==13)
      {
          central_moment(arr,len,1)
	        central_moment(arr,len,2)
	        central_moment(arr,len,3)
	        central_moment(arr,len,4)
      }
      
      input=readline(prompt="Want to continue this module (yes or no): ")	
      if(input=="no")
      {
        break
      }
     }
     
   }
   if(inp==2)
   {
     repeat{
       print("==========================Predictive Analysis==============================")
       
       print("1.Correlation, 2.Multiple Linear Regression, 3. Logistic Regression")
       
       ch=as.numeric(readline(prompt="Enter your choice: "))
       if(ch==1)
       {
         arr1<-c(data1$X)
         len1<-length(arr1)
         
         arr2<-c(data1$Y)
         len2<-length(arr2)
         
         Cor(arr1,arr2,len1,len2)
         
         
       }
       if(ch==2)
       {
         print("Multiple Linear regression function")
       }
       if(ch==3)
       {
         print(" Logistic regression funtion")
       }
       
       input=readline(prompt="Want to continue this module(yes or no): ")	
       if(input=="no")
       {
         break
       }
       
     }
   }
   if(inp==3)
   {
     repeat{
       print("==========================Probability Analysis==============================")
       
       print("1. Permutations. 2. Combinations, 3. Basic Probabitlity 4.Conditional Probability, 5. Bayes Theorem")
       
       ch=as.numeric(readline(prompt="Enter your choice: "))
       if(ch==1)
       {
         n1<-as.numeric(readline(prompt = "Enter Value of n1 : "))
         x1<-as.numeric(readline(prompt = "Enter Value of x1 : "))
         per(n1,x1)
         
         
         
       }
       if(ch==2)
       {
         n1<-as.numeric(readline(prompt = "Enter Value of n1 : "))
         x1<-as.numeric(readline(prompt = "Enter Value of x1 : "))
         comb(n1,x1)
       }  
       if(ch==3)
       {
         
         fav_outcome<-as.numeric(readline(prompt = "Enter fav_outcome : "))
         possible_outcome<-as.numeric(readline(prompt = "Enter possible_outcome : "))
         prob(fav_outcome,possible_outcome)
       }
       if(ch==4)
       {
         
         probA<-as.numeric(readline(prompt = "Enter probA : "))
         probB<-as.numeric(readline(prompt = "Enter probB : "))
         prob_AandB<-as.numeric(readline(prompt = "Enter prob_AandB: "))
         conditional(0.90,0.80,0.72)
         
       }       
       if(ch==5)
       {
         
         ProbB1<-as.numeric(readline(prompt = "Enter probB1 : "))
         ProbB2<-as.numeric(readline(prompt = "Enter probB2 : "))
         ProbB3<-as.numeric(readline(prompt = "Enter probB3 : "))
         Prob_AgivenB1<-as.numeric(readline(prompt = "Enter prob_AgivenB1 : "))
         Prob_AgivenB2<-as.numeric(readline(prompt = "Enter prob_AgivenB2 : "))
         Prob_AgivenB3<-as.numeric(readline(prompt = "Enter prob_AgivenB3 : "))
         bayes(0.6,0.3,0.10,0.09,0.2,0.06)
       }        
       input=readline(prompt="Want to continue this module(yes or no): ")	
       if(input=="no")
       {
         break
       }
     }
   }
   if(inp==4)
   {
     repeat{
       print("==========================Discrete Distribution Function==============================")
       
       print("1. Uniform, 2. Bernoulli, 3. Binomial, 4.Geometric, 5. Hyper_geometric, 6. Negative Binomial, 7. Poisson, 8. Multinomial, 9. Multivariate Hypergeometric")
       
       ch=as.numeric(readline(prompt="Enter your choice: "))
       if(ch==1)
       {
         k <-as.numeric(readline(prompt="Enter k : "))
         prob<-uniformd(k)
         message(sprintf("Probability is : %.10f",prob))
       }
       if(ch==2)
       {
         x<-as.numeric(readline(prompt = "Enter value of x : "))
         p<-as.numeric(readline(prompt ="Enter probability : "))
         prob<-bernoulli(0,0.3)
         message(sprintf("Probability is : %.10f",prob))
       }
       if(ch==3)
       {
         n<-as.numeric(readline(prompt="Enter numer of trials : "))
         x<-as.numeric(readline(prompt="Enter number of successes:"))
         p<-as.numeric(readline(prompt="Enter probablity : "))
         prob<-binomial(15,19,0.5)
         message(sprintf("Probability is : %.10f",prob))
       }
       
       if(ch==4)
       {
         x<-as.numeric(readline(prompt="Enter numer of trials : "))
         p<-as.numeric(readline(prompt="Enter number of successes:"))
         prob<-geometric(4,0.75)
         message(sprintf("Probability is : %.10f",prob))
         
       }
       
       if(ch==5)
       {
         x<-as.numeric(readline(prompt="Enter x: "))
         n<-as.numeric(readline(prompt="Enter n : "))
         N<-as.numeric(readline(prompt="Enter N: "))
         M<-as.numeric(readline(prompt="Enter M: "))
         prob<-hyper(0,6,24,4)
         message(sprintf("Prob is : %.10f",prob))
       }
       
       if(ch==6)
       {
         x<-as.numeric(readline(prompt="Enter x: "))
         k<-as.numeric(readline(prompt="Enter k : "))
         p<-as.numeric(readline(prompt="Enter p: "))
         prob<-negative(10,3,0.4)
         message(sprintf("Prob is : %.10f",prob))

       }
       
       if(ch==7)
       {
         x<-as.numeric(readline(prompt="Enter x: "))
         lambda<-as.numeric(readline(prompt="Enter lambda : "))
         prob<-poisson(5,8)
         message(sprintf("Prob is : %.10f",prob))
         
       }
       
       if(ch==8)
       {
         x1<-as.numeric(readline(prompt="Enter x1: "))
         x2<-as.numeric(readline(prompt="Enter x2: "))
         x3<-as.numeric(readline(prompt="Enter x3: "))
         n<-as.numeric(readline(prompt="Enter n: "))
         p1<-as.numeric(readline(prompt="Enter p1: "))
         p2<-as.numeric(readline(prompt="Enter p2: "))
         p3<-as.numeric(readline(prompt="Enter p2: "))
         prob<-multinomial(5,2,1,8,0.5,0.3,0.2)
         message(sprintf("Prob is : %.10f",prob))
         
       }
       
       if(ch==9)
       {
         x1<-as.numeric(readline(prompt="Enter x1: "))
         x2<-as.numeric(readline(prompt="Enter x2: "))
         x3<-as.numeric(readline(prompt="Enter x3: "))
         n<-as.numeric(readline(prompt="Enter n: "))
         m1<-as.numeric(readline(prompt="Enter m1: "))
         m2<-as.numeric(readline(prompt="Enter m2: "))
         m3<-as.numeric(readline(prompt="Enter m2: "))
         prob<-multivariate(4,1,5,10,6,3,7)
         message(sprintf("Prob is : %.10f",prob))
         
       }
       input=readline(prompt="Want to continue this module(yes or no): ")	
       if(input=="no")
       {
         break
       }
     }
   }
   if(inp==5)
   {
     repeat{
       print("==========================Continuous Distribution functions==============================")
       
       print("1.Uniform, 2. Normal, 3. Bivariate Normal, 4. Gamma, 5. Exponential")
       
       ch=as.numeric(readline(prompt="Enter your choice: "))
       if(ch==1)
       {
         print("Uniform function")
       }
       if(ch==2)
       {
         x<-as.numeric(readline(prompt="Enter x: "))
         mean<-as.numeric(readline(prompt="Enter mean:"))
         sd<-as.numeric(readline(prompt="Enter sd:"))
         
         normal(365,300,50)
       }
       if(ch==3)
       {
         x1=data1$X
         x2=data1$Y
         
         bivariate(x1,x2)
         
       }
       
       if(ch==4)
       {
         x<-as.numeric(readline(prompt="Enter x: "))
         alpha<-as.numeric(readline(prompt="Enter alpha:"))
         beta<-as.numeric(readline(prompt="Enter beta:"))
         gamma(10,8.5,7)
       }
       
       if(ch==5)
       {
         alpha<-as.numeric(readline(prompt="Enter alpha: "))
         
         expo(8.4)
       }
       
       input=readline(prompt="Want to continue this module(yes or no):")
       if(input=="no")
          {
          break
          }
     }
   }
   if(inp==6)
   {
     repeat{
       print("==========================Sample Distribution Test Statistic==============================")
       
       print("1.Chi-Squre 2. Student t-test ,3. F-test, 4. Z-test")
       
       ch=as.numeric(readline(prompt="Enter your choice: "))
       if(ch==1)
       {
         library(MASS)
         
         tbl = table(survey$Smoke, survey$Exer)
         
         df=(nrow(tbl)-1)*(ncol(tbl)-1)
         
         chi_square(tbl,0.05,df) 
         
         
       }
       if(ch==2)
       {
         l<-c(7.65,7.60,7.65,7.70,7.55,7.55,7.40,7.40,7.50,7.50)
         t_test(l,7.5,0.05)
         
       }
       if(ch==3)
       {
         X<-data1$X
         Y<-data1$Y
         f_test(X,Y,0.10)
         
       }
       
       if(ch==4)
       {
         
         a = c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81)
         z_test(a,75,sqrt(18),0.05)
         
       }
       
       
       input=readline(prompt="Want to continue this module(yes or no): ")	
       if(input=="no")
       {
         break
       }
     }
   }
   
   if(inp==7)
   {
     repeat{
       print("==========================Interval Estimation==============================")
       
       print("1. Estimation of Means, 2. Esimation of Differences in Means, 3. Estimation of Proportions, 4. Estimation of Differences in Proportions, 5.Estimation of Variances, 6. Estimation of Ratio of Two Variances.")
       
       ch=as.numeric(readline(prompt="Enter your choice: "))
       if(ch==1)
       {
         n<-as.numeric(readline(prompt="Enter n: "))
         alpha<-as.numeric(readline(prompt="Enter alpha:"))
         sigma<-as.numeric(readline(prompt="Enter sigma:"))
         
         E_mean(150,0.005,6.2)
         
         
       }
       if(ch==2)
       {
         x1<-as.numeric(readline(prompt="Enter x1: "))
         x2<-as.numeric(readline(prompt="Enter x2: "))
         n1<-as.numeric(readline(prompt="Enter n1: "))
         n2<-as.numeric(readline(prompt="Enter n2: "))
         
         
         alpha<-as.numeric(readline(prompt="Enter alpha:"))
         sigma1<-as.numeric(readline(prompt="Enter sigma1:"))
         sigma2<-as.numeric(readline(prompt="Enter sigma2:"))
         
         
         E_diff(418,402,26,22,40,50,0.06)  
         
         
       }
       if(ch==3)
       {
         x<-as.numeric(readline(prompt="Enter x: "))
         alpha<-as.numeric(readline(prompt="Enter alpha:"))
         n<-as.numeric(readline(prompt="Enter n:"))
         
         E_prop(140,400,0.01)
         
       }
       
       if(ch==4)
       {
         x1<-as.numeric(readline(prompt="Enter x1: "))
         x2<-as.numeric(readline(prompt="Enter x2: "))
         n1<-as.numeric(readline(prompt="Enter n1: "))
         n2<-as.numeric(readline(prompt="Enter n2: "))
         
         
         alpha<-as.numeric(readline(prompt="Enter alpha:"))
         
         E_diffprop(132,90,200,150,0.01)
         
       }
       
       if(ch==5)
       {
         s<-as.numeric(readline(prompt="Enter s: "))
         alpha<-as.numeric(readline(prompt="Enter alpha:"))
         n<-as.numeric(readline(prompt="Enter n:"))
         
         E_var(16,2.2,0.01)
       }
       
       if(ch==6)
       {
         s1<-as.numeric(readline(prompt="Enter s1: "))
         s2<-as.numeric(readline(prompt="Enter s2:"))
         alpha<-as.numeric(readline(prompt="Enter alpha:"))
         
         E_ratio(0.5,0.7,0.01)
       }
       
       input=readline(prompt="Want to continue this module(yes or no): ")	
       if(input=="no")
       {
         break
       }
     }
   }
   if(inp==8)
   {
     repeat{
       print("==========================Non-Parametric analysis==============================")
       
       print("1.Sign Test, 2.Wilcoxon Signed-Rank Test, 3.Mann-Whitney Test, 4.Kruskal-Wallis Test")
       
       ch=as.numeric(readline(prompt="Enter your choice: "))
       if(ch==1)
       {
         sample<-c(163,165,160,189,161,171,158,151,169,162,163,139,172,165,148,166,172,163,187,173)
         v<-sign(sample,160,0.05)
         
         
       }
       if(ch==2)
       {
         wilcoxon(c(97.5,95.2,97.3,96,96.8,100.3,97.4,95.3,93.2,99.1,96.1,97.6,98.2,98.5,94.9),98.5,21)
         
       }
       if(ch==3)
       {
         sample1<-c(14.9,11.3,13.2,16.6,17,14.1,15.4,13.0,16.9)
         
         sample2<-c(15.2,19.8,14.7,18.3,16.2,21.2,18.9,12.2,15.3,19.4)
         
         whitney(sample1,sample2,24,"left") 
         
         
       }
       
       if(ch==4)
       {
         s1<-c(94,88,91,74,87,97)
         s2<-c(85,82,79,84,61,73,80)
         s3<-c(89,67,72,76,69)    
         
         
         kruskal(s1,s2,s3,5.9)
         
       }
       input=readline(prompt="Want to continue this module(yes or no): ")	
       if(input=="no")
       {
         break
       }
     }
   }
   if(inp==9)
   {
     repeat{
       print("==========================Visualizations==============================")
       
       print("1.Histogram 2.Line Graph 3.Bar Graph, 4. Pie Chart, 5. Scatter Plot, 6. Box lot 7. q-q plot 8.Pareto chart,9. Stem-leaf plot")
       
       ch=as.numeric(readline(prompt="Enter your choice: "))
       if(ch==1)
       {
         print("Histogram")
         X<-data1$X
         hist(X)
         
       }
       if(ch==2)
       {
         print("Line Graph")
         plot(X,type = "o",col="green")
       }
       if(ch==3)
       {
         print("Bar Graph")
         library(MASS)
         
         X.freq=table(X)
         
         barplot(X.freq,main="Count",col="blue")
         
         
       }
       
       if(ch==4)
       {
         print("Pie Chart")
         x <- c(21, 62, 10, 53)
         labels <- c("London", "New York", "Singapore", "Mumbai")
         
         pie(x, labels, main = "City pie chart", col = rainbow(length(x)))
       }
       
       if(ch==5)
       {
         print("Scatter plot")
         
         input <- mtcars[,c('wt','mpg')]
         
         plot(x = input$wt,y = input$mpg,
              xlab = "Weight",
              ylab = "Milage",
              xlim = c(2.5,5),
              ylim = c(15,30),		 
              main = "Weight vs Milage")
         
       }
       
       if(ch==6)
       {
         print("Box-plot")
         input <- mtcars[,c('mpg','cyl')]
         
         boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
                 ylab = "Miles Per Gallon", main = "Mileage Data")
         
         
         
       }
       
       if(ch==7)
       {
         print("q-q plot")
         y <- rt(200, df = 5)
         qqnorm(y); qqline(y, col = 2)
         qqplot(y, rt(300, df = 5))
         data(precip)
         qqnorm(precip, ylab = "Precipitation [in/yr] for 70 US cities")
         
       }
       
       if(ch==8)
       {
         print("Pareto Chart")
         library(qcc)
         defect <- c(80, 27, 66, 94, 33)
         names(defect) <- c("price code", "schedule date", "supplier code", "contact num.", "part num.")
         pareto.chart(defect, ylab = "Error frequency", col=heat.colors(length(defect)))
         
         
       }
       
       if(ch==9)
       {
         
         print("Stem-leaf plot")
         stem(X)
       }
       input=readline(prompt="Want to continue this module(yes or no): ")	
       if(input=="no")
       {
         break
       }
     }
   }
   
     
     
   
   
       
   
   
  input=readline(prompt="Want to continue this code(y or n): ")	
  if(input=="n")
  {
     break
  }
   
}

   