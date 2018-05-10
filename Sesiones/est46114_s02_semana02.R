#Step 1: Find path of data file and copy path (if using windows)
#Step 2: In the console below, type readClipboard()
#Step 3: Copy and paste R's path to the line below in quotes

#CHANGE BETWEEN QUOTES IN THIS LINE TO REFLECT DIRECTORY OF DATA:
myDataLocation = ""

#SET WORKING DIRECTORY TO LOCATION OF DATA FILE
#setwd(myDataLocation)

if (require("mvtnorm") == FALSE){
  install.packages("mvtnorm")
}

library("mvtnorm")

#Playing with Matrices -------------------------------------------
#IMPORT DATA AND PUT INTO DATASET
data01 = read.csv(file="./Sesiones/Semana02/est46114_semana02.csv",header=TRUE)

#defining the matrix X
X = cbind(data01$SATV,data01$SATM) #cbind stands for column-bind -- makes into a matrix

#showing a column vector:
xcolumn=matrix(X[,1]) #the matrix() function ensures that the data are stored in matrix form -- important for ops

#showing a row vector:
xrow = matrix(X[1,])

#saving satv and satm for later use
satv = matrix(X[,1])
satm = matrix(X[,2])

#showing an element in matrix X
X[10,2] #row 10, column 2
X[1,1] #row 1, column 1

#matrix transpose:
Xtranspose = t(X)

#Playing with Vectors -------------------------------------------
a = matrix(c(1,2))
b = matrix(c(2,3))

length_a = sqrt(sum(a^2)) #length of vector a
length_a
length_b = sqrt(sum(b^2)) #length of vector b
length_b

length_satv = sqrt(sum(satv^2))
length_satv

length_satm = sqrt(sum(satm^2))
length_satm

#vector addition
c = a + b 
c

sat_total = satv+satm

#scalar multiplication
d = 2*a
d

length_d = sqrt(sum(d^2))
length_d


#dot product of vectors:
a_dot_b = t(a)%*%b
a_dot_b

satv_dot_satm = t(satv)%*%satm
satv_dot_satm

#angle between vectors:
angle_ab = acos(a_dot_b/(length_a*length_b))
angle_ab

angle_satvsatm = acos(satv_dot_satm/(length_satv*length_satm))
angle_satvsatm

#angle to correlation: use mean centered variables/vectors
satv_c = satv-mean(satv)
satm_c = satm-mean(satm)

cor_satvsatm = t(satv_c)%*%satm_c/(sqrt(sum(satv_c^2))*sqrt(sum(satm_c^2)))
cor_satvsatm

#vector projections:
a_proj_b = (a_dot_b/(length_b^2))[1]*b
a_proj_b

t(satv_c)%*%satm_c/sum(satm_c^2)[1]

reg01 = lm(satv~satm) #regression
summary(reg01)

#fun with matrices -----------------------------------------
#matrix multiplication -- more dot products
XtransX = t(X)%*%X
XtransX

#identity matrices:
identity3 = diag(3)
identity3

#zero vector:
zero3 = matrix(0,3,1)
zero3

#ones vector:
ones3 = matrix(1,3,1)
ones3

#matrix inverse:
XtransXinv = solve(XtransX)
XtransXinv

XtransXinv%*%XtransX #showing how close to identity this becomes
XtransX%*%XtransXinv #showing how close to identity this becomes

#advanced matrix functions and operations ----------------------

R = matrix(1,2,2) 
R[1,2]=cor(satv,satm) #for example -- using correlation matrix
R[2,1]=cor(satv,satm) #for example -- using correlation matrix
R

traceR = sum(diag(R)) #trace of R
traceR

detR = det(R)
detR

#multivariate statistics -------------------------------

#calculating the mean vector:
N = (1/length(X[,1]))[1]
ONES = matrix(1,length(X[,1]),1)

XBAR = N*t(X)%*%ONES
XBAR

#calculating the covariance matrix:
S = N*t(X-ONES%*%t(XBAR))%*%(X-ONES%*%t(XBAR))
S

#diagonal matrix of standard deviations:
D = sqrt(diag(diag(S)))
D
Dinv = solve(D)
Dinv

R2 = Dinv%*%S%*%Dinv
R2
R

#Generalized Sample Variance
gsv = det(S)
gsv

#Total Sample Variance
tsv = sum(diag(S))
tsv

#Multivariate Normal Distribution PDF -----------------------------------------
XBAR
S

#use dmvnorm() function to return likelihood and log-likelihood from MVN:
likelihood_case631 = dmvnorm(x=X[631,],mean = XBAR,sigma = S, log=FALSE)
likelihood_case631

loglikelihood_case631 = dmvnorm(x=X[631,],mean = XBAR,sigma = S, log=TRUE)
loglikelihood_case631

likelihood_case717 = dmvnorm(x=X[717,], mean = XBAR, sigma = S, log=FALSE)
likelihood_case717

loglikelihood_case717 = dmvnorm(x=X[717,], mean = XBAR, sigma = S, log=TRUE)
loglikelihood_case717

likelihood_caseXBAR = dmvnorm(x=t(XBAR), mean = XBAR, sigma = S, log=FALSE)
likelihood_caseXBAR

loglikelihood_caseXBAR = dmvnorm(x=t(XBAR), mean = XBAR, sigma = S, log=TRUE)
loglikelihood_caseXBAR

