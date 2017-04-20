setwd("/Users/dshartra/Documents")
#DOE Creation
A = c(rep(0,6),rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6))
rangeA = max(A)-min(A)
minA = min(A)
for (i in 1:36){
  A[i]= (A[i]-minA)/rangeA
}
y=1
x=-1
range2=y-x
for (i in 1:36){
  A[i]= (A[i]*range2)+x
}
B = rep(c(0,1,2,3,4,5),6)
rangeB = max(B)-min(B)
minB = min(B)
for (i in 1:36){
  B[i]= (B[i]-minB)/rangeB
}
for (i in 1:36){
  B[i]= (B[i]*range2)+x
}
C = c(0,1,2,3,4,5,1,2,0,4,5,3,2,0,1,5,3,4,3,5,4,0,2,1,4,3,5,1,0,2,5,4,3,2,1,0)
rangeC = max(C)-min(C)
minC = min(C)
for (i in 1:36){
  C[i]= (C[i]-minC)/rangeC
}
for (i in 1:36){
  C[i]= (C[i]*range2)+x
}
D = c(0,2,2,1,1,0,0,0,1,2,1,2,1,2,1,2,0,0,2,0,0,1,2,1,2,1,2,0,0,1,1,1,0,0,2,2)
rangeD = max(D)-min(D)
minD = min(D)
for (i in 1:36){
  D[i]= (D[i]-minD)/rangeD
}
for (i in 1:36){
  D[i]= (D[i]*range2)+x
}
E =c(0,1,2,1,0,2,0,1,2,0,0,1,1,2,0,0,2,1,2,0,1,1,0,2,2,0,1,2,1,0,1,2,0,2,1,0)
rangeE = max(E)-min(E)
minE = min(E)
for (i in 1:36){
  E[i]= (E[i]-minE)/rangeE
}
for (i in 1:36){
  E[i]= (E[i]*range2)+x
}
F=c(0,0,1,1,2,2,1,2,1,0,0,2,0,2,2,1,0,1,1,1,0,2,2,0,2,0,0,2,1,1,2,1,2,0,1,0)
rangeF = max(F)-min(F)
minF = min(F)
for (i in 1:36){
  F[i]= (F[i]-minF)/rangeF
}
for (i in 1:36){
  F[i]= (F[i]*range2)+x
}
G= c(0,1,2,2,1,0,1,2,0,2,1,0,0,1,2,0,2,1,1,2,0,1,0,2,2,0,1,0,2,1,2,0,1,1,0,2)
rangeG = max(G)-min(G)
minG = min(G)
for (i in 1:36){
  G[i]= (G[i]-minG)/rangeG
}
for (i in 1:36){
  G[i]= (G[i]*range2)+x
}
H =c(0,2,1,0,2,1,1,0,2,1,0,2,2,1,0,2,1,0,0,2,1,1,0,2,2,1,0,0,2,1,1,0,2,2,1,0)
rangeH = max(H)-min(H)
minH = min(H)
for (i in 1:36){
  H[i]= (H[i]-minH)/rangeH
}
for (i in 1:36){
  H[i]= (H[i]*range2)+x
}
I = c(0,2,1,0,1,2,1,0,2,2,0,1,1,0,2,0,1,2,2,1,0,1,2,0,0,2,1,1,2,0,2,1,0,2,0,1)
rangeI = max(I)-min(I)
minI = min(I)
for (i in 1:36){
  I[i]= (I[i]-minI)/rangeI
}
for (i in 1:36){
  I[i]= (I[i]*range2)+x
}
J = c(0,2,0,1,2,1,1,2,2,1,0,0,1,1,0,2,2,0,2,0,2,0,1,1,0,0,1,2,1,2,2,1,1,0,0,2)
rangeJ = max(J)-min(J)
minJ = min(J)
for (i in 1:36){
  J[i]= (J[i]-minJ)/rangeJ
}
for (i in 1:36){
  J[i]= (J[i]*range2)+x
}
design = data.frame(t(A),t(B),t(C),t(D),t(E),t(F),t(G),t(H),t(I),t(J))
design = data.frame(A,B,C,D,E,F,G,H,I,J)
colnames(design) <- c("A","B","C","D","E","F","G","H","I","J")
write.table(design, "design.txt", sep="\t")
#Data Analysis
data<-read.table("07.txt",h=T)
#Qudaratic Effect Function
qua = function(fac)
  {
  n = length(fac)
  qua = numeric()
  for (i in 1:n)
  {
    if(fac[i]==1)
      qua[i] = -2
    else       
      qua[i] = 1
    }
   return(qua)
}
lin6 = function(fac)
{
  n = length(fac)
  lin6 = numeric()
  for (i in 1:n)
  {
    if(fac[i]==-1)
      lin6[i] = -5 
    else if(fac[i]==-.6)
      lin6[i] = -3
    else if(fac[i]==-.2)
      lin6[i]=-1
    else if(fac[i]==.2)
      lin6[i] = 1
    else if(fac[i]==.6)
      lin6[i] = 3
    else       
      lin6[i] = 5
  }
  return(lin6)
}
quad6 = function(fac)
{
  n = length(fac)
  quad6 = numeric()
  for (i in 1:n)
  {
    if(fac[i]==-1)
      quad6[i] = 5 
    else if(fac[i]==-.6)
      quad6[i] = -1
    else if(fac[i]==-.2)
      quad6[i]=-4
    else if(fac[i]==.2)
      quad6[i] = -4
    else if(fac[i]==.6)
      quad6[i] = -1
    else       
      quad6[i] = 5
  }
  return(quad6)
}
cube6 = function(fac)
{
  n = length(fac)
  cube6 = numeric()
  for (i in 1:n)
  {
    if(fac[i]==-1)
      cube6[i] = -5 
    else if(fac[i]==-.6)
      cube6[i] = 7
    else if(fac[i]==-.2)
      cube6[i]=4
    else if(fac[i]==.2)
      cube6[i] = -4
    else if(fac[i]==.6)
      cube6[i] = -7
    else       
      cube6[i] = 5
  }
  return(cube6)
}
##############
## Generate linear and quadratic effects for A to J
attach(data)
Al = lin6(A); Al = as.matrix(Al); Al = Al/sqrt(70)
Aq = quad6(A); Aq=as.matrix(Aq); Aq = Aq/sqrt(84)
Ac = cube6(A); Ac=as.matrix(Ac); Ac = Ac/sqrt(180)
Bl = lin6(B); Bl = as.matrix(Bl); Bl = Bl/sqrt(70)
Bq = quad6(B); Bq=as.matrix(Bq); Bq = Bq/sqrt(84)
Bc = cube6(B); Bc=as.matrix(Bc); Bc = Bc/sqrt(180)
Cl = lin6(C); Cl = as.matrix(Cl); Cl = Cl/sqrt(70)
Cq = quad6(C); Cq=as.matrix(Cq); Cq = Cq/sqrt(84)
Cc = cube6(C); Cc=as.matrix(Cc); Cc = Cc/sqrt(180)
Dl = D/sqrt(2); Dl= as.matrix(Dl)
Dq = qua(D); Dq = as.matrix(Dq); Dq = Dq/sqrt(6)
El = E/sqrt(2); El = as.matrix(El)
Eq = qua(E); Eq = as.matrix(Eq); Eq = Eq/sqrt(6)
Fl = F/sqrt(2); Fl=as.matrix(Fl)
Fq = qua(F); Fq = as.matrix(Fq); Fq = Fq/sqrt(6)
Gl = G/sqrt(2); Gl=as.matrix(Gl)
Gq = qua(G); Gq = as.matrix(Gq); Gq = Gq/sqrt(6)
Hl = H/sqrt(2); Hl = as.matrix(Hl)
Hq = qua(H); Hq = as.matrix(Hq); Hq = Hq/sqrt(6)
Il = I/sqrt(2); Il= as.matrix(Il)
Iq = qua(I); Iq = as.matrix(Iq); Iq = Iq/sqrt(6)
Jl = J/sqrt(2); Jl=as.matrix(Jl)
Jq = qua(J); Jq = as.matrix(Jq); Jq = Jq/sqrt(6)
detach(data)
### Analysis of Factors
cor <-lm(y~Al+Bl+Cl+Aq+Bq+Cq+Dl+Dq+El+Eq+Fl+Fq+Gl+Gq+Hl+Hq+Il+Iq+Jl+Jq,data=data)
eff_ybar2 <- 2*cor$coef[-1]
median(abs(eff_ybar2))
s02=1.5*median(abs(eff_ybar2))
median(abs(eff_ybar2[abs(eff_ybar2)<2.5*s02]))
pse2=1.5*median(abs(eff_ybar2[abs(eff_ybar2)<2.5*s02]))
abs(eff_ybar2)/pse2 # 
require(graphics)
library(faraway)
halfnorm(eff_ybar2)
m2<-lm(y~(El+Il+Eq+Iq+Al+Aq+Cl)^2,data=data)
m2.step<-step(m2)
summary(m2.step) # Il Iq El Eq Al Aq Cl ElAq IlAl EqIq EqAq AqCl R^2a = 0.9915
m3<-lm(y~El+Il+Eq+Iq+Al+Aq+Cl+El*Aq+Il*Al+Eq*Iq+Eq*Aq+Aq*Cl,data=data)
m3.step<-step(m3)
summary(m3.step) # converged.
##More analysis methods to check
library(leaps)
b2 = regsubsets(y~El+Il+Eq+Iq+Al+Aq+Cl+El*Aq+Il*Al+Eq*Iq+Eq*Aq+Aq*Cl,data=data)
rs2=summary(b2)
rs2
rs2$adjr2 #Maximum R^2a is 0.9853692 with 8 parameters
#However 7 paramter is slightly simpler with only less of a large R^2a
#Model with 6 paramters has Aq and IlAl but no Al. Choose this model and add in Al term
plot(2:9,rs2$cp,xlab="No. of Parameters",ylab="Mallow's Cp Statistic") ##Not a great analysis with Mallow's
#good fit based on R^2a without being along the abline
abline(0,1)
rs2$cp
#Final Model
final<-lm(y~Al+Aq+El+Il+Eq+Iq+Al*Il,data=data)
summary(final)
plot(final)
##Residual Plots look fairly good, no need to transform
##Function to generate predictions
# x should be entered of the form (1,Al,Aq,El,Il,Eq,Iq,AlIl)
int = Al*Il
one = c(rep(1,36))
xmat = data.frame(as.vector(one),as.vector(Al),as.vector(Aq),as.vector(El),as.vector(Il),as.vector(Eq),as.vector(Iq),as.vector(int))
xmat = as.matrix(xmat)
fhat=function(x){
    fhat = numeric()
    for (i in 1:36){
      fhat[i] = 13.5120*x[i,1] + 1.5345*x[i,2] -3.9848*x[i,3] +4.7831*x[i,4] +6.1290*x[i,5] -4.3567*x[i,6] +3.3046*x[i,7]+4.9597*x[i,8]
    }
  return(fhat)
}
ypred=fhat(xmat)



