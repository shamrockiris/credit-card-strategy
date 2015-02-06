data1 <- read.csv(file.choose(),header=T)
APR <- 0.1299
debt <- 882600
NF <- sum(data1[,2:11])+sum(data1[,13])+sum(data1[,16:17])+sum(data1[,20])                                                                                                                          
O1 <- sum(data1[1:3,12])
O2 <- sum(data1[4:6,12])
O3 <- sum(data1[7:9,12])
O4 <- sum(data1[10:12,12])
A1 <- sum(data1[1:3,14])
A2 <- sum(data1[4:6,14])
A3 <- sum(data1[7:9,14])
A4 <- sum(data1[10:12,14])
G1 <- sum(data1[1:3,15])+sum(data1[1:3,19])
G2 <- sum(data1[4:6,15])+sum(data1[4:6,19])
G3 <- sum(data1[7:9,15])+sum(data1[7:9,19])
G4 <- sum(data1[10:12,15])+sum(data1[10:12,19])
P1 <- sum(data1[1:3,18])
P2 <- sum(data1[4:6,18])
P3 <- sum(data1[7:9,18])
P4 <- sum(data1[10:12,18])
Office <- O1+O2+O3+O4
Clothes <- A1+A2+A3+A4
Food <- G1+G2+G3+G4
Fuel <- P1+P2+P3+P4
To <- sum(data1[,-1])
# matrix to find the popularity change factor according to different reward rate
P <- rbind(c(0.43,0.43,rep(1,99)),rep(1,101),rep(1.57,101),rep(1.57,101),rep(1.57^2,101),rep(1.57^2,101),matrix(rep(1.57^3,95*101),nrow=95,ncol=101))
# Benifit function
fr <- function(r,sale) {
  result <- (2-r)*0.01*sale+APR*debt*sale/To 
  return(result)
}
# 1% reward for everything no limit
fixedRate <- fr(1,To)
# 2% reward for grocery, 3% for fuel, 1% everything else, no limit
categoryRate <- fr(2,Food)*1.57+fr(3,Fuel)*1.57^2+fr(1,To-Food-Fuel)
# 5% reward for particular category changing every quater, 0% for everything else
quaterRate <- fr(5,G1+A3+P4)*1.57^2+fr(0,To-G1-A3-P4)

# try to do optimization to find best reward rate based on same APR
#frc <- function(R) {
#  r1 <- R[1]
#  r21 <- R[2]
#  r22 <- R[3]
#  r23 <- R[4]
#  r24 <- R[5]
#  r31 <- R[6]
#  r32 <- R[7]
#  r33 <- R[8]
#  r34 <- R[9]
#  r41 <- R[10]
#  r42 <- R[11]
#  r43 <- R[12]
#  r44 <- R[13]
#  r51 <- R[14]
#  r52 <- R[15]
#  r53 <- R[16]
#  r54 <- R[17]
#  r61 <- R[18]
#  r62 <- R[19]
#  r63 <- R[20]
#  r64 <- R[21]
#  return((-1)*(fr(r1,NF)*P[round(r1),round (max(R))]+fr(r21,P1)*P[round(r21),round(max(R))]+fr(r22,P2)*P[round(r22),round(max(R))]+fr(r23,P3)*P[round(r23),round(max(R))]+fr(r24,P4)*P[round(r24),round(max(R))]+fr(r31,A1)*P[round(r31),round(max(R))]+fr(r32,A2)*P[round(r32),round(max(R))]+fr(r33,A3)*P[round(r33),round(max(R))]+fr(r34,A4)*P[round(r34),round(max(R))]+fr(r41,G1)*P[round(r41),round(max(R))]+fr(r42,G2)*P[round(r42),round(max(R))]+fr(r43,G3)*P[round(r43),round(max(R))]+fr(r44,G4)*P[round(r44),round(max(R))]+fr(r51,P1)*P[round(r51),round(max(R))]+fr(r52,P2)*P[round(r52),round(max(R))]+fr(r53,P3)*P[round(r53),round(max(R))]+fr(r54,P4)*P[round(r54),round(max(R))]+fr(r61,S1)*P[round(r61),round(max(R))]+fr(r62,S2)*P[round(r62),round(max(R))]+fr(r63,S3)*P[round(r63),round(max(R))]+fr(r64,S4)*P[round(r64),round(max(R))]))
#}
#optim(rep(1,21),frc,method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent") , lower = 0, upper = 100)

# Lay out different situations to compare bennifit
# zero reward for not frequently purchased & some category up to 1% reward. The Max Benifit will be zero reward for all 
Ben1 <- fr(0,To)*0.43
# zero reward for not frequently purchased, 1% reward for all others
Ben2 <- fr(0,NF)*0.43+fr(1,T0-NF)
# zero reward for not frequently purchased & 2% reward for 1 category
Ben3 <- fr(0,To-P1-P2-P3-P4)*0.43+fr(2,P1+P2+P3+P4)*1.57
# zero reward for not frequently purchased & 3% reward for 1 category
Ben4 <- fr(0,To-O1-O2-O3-O4)*0.43+fr(3,O1+O2+O3+O4)*1.57
# zero reward for not frequently purchased & 2% reward for 2 catogories
compare <- function(X1,X2,X3,X4){
  fr(2,X1+X2)*1.57+fr(0,X3+X4)*0.43
}
ben5 <- max(compare(Office,Clothes,Food,Fuel),compare(Office,Food,Clothes,Fuel),compare(Office,Fuel,Food,Clothes),compare(Clothes,Food,Office,Fuel),compare(Clothes,Fuel,Office,Food),compare(Food,Fuel,Office,Clothes))
Ben5 <- fr(0,NF)*0.43+ben5
# zero reward for not frequently purchased & 3% reward for 2 categories
compare2 <- function(X1,X2,X3,X4){
  fr(3,X1+X2)*1.57+fr(0,X3+X4)
}
ben6 <- max(compare2(Office,Clothes,Food,Fuel),compare2(Office,Food,Clothes,Fuel),compare2(Office,Fuel,Food,Clothes),compare2(Clothes,Food,Office,Fuel),compare2(Clothes,Fuel,Office,Food),compare2(Food,Fuel,Office,Clothes))
Ben6 <- fr(0,NF)+ben6
# zero reward for not frequently purchased & 2% reward for 3 categories
compare3 <- function(X1,X2,X3,X4){
  fr(2,X1+X2+X3)*1.57+fr(0,X4)
}
ben7 <- max(compare3(Office,Clothes,Food,Fuel),compare3(Office,Clothes,Fuel,Food),compare3(Office,Fuel,Food,Clothes),compare3(Fuel,Clothes,Food,Office))
Ben7 <- fr(0,NF)+ben7
# zero reward for not frequently purchased & 3% reward for 3 categories
compare4 <- function(X1,X2,X3,X4){
  fr(3,X1+X2+X3)*1.57+fr(0,X4)
}
ben8 <- max(compare4(Office,Clothes,Food,Fuel),compare4(Office,Clothes,Fuel,Food),compare4(Office,Fuel,Food,Clothes),compare4(Fuel,Clothes,Food,Office))
Ben8 <- fr(0,NF)+ben8
# zero reward for not frequently purchased & 2% for all other
Ben9 <- fr(0,NF)+fr(2,To-NF)*1.57
# zero reward for not frequently purchased & 2% for all other in order
compare5 <- function(X1,X2,X3,X4) {
  fr(2,X1)*1.57+fr(0,X2+X3+X4)
}
Ben9_1 <- fr(0,NF)+compare5(O1,A1,G1,P1)+compare5(A2,O2,G2,P2)+compare5(G3,A3,O3,P3)+compare5(P4,A4,G4,O4)
Ben9_2 <- fr(0,NF)+compare5(A1,O1,G1,P1)+compare5(O2,A2,G2,P2)+compare5(G3,A3,O3,P3)+compare5(P4,A4,G4,O4)
Ben9_3 <- fr(0,NF)+compare5(O1,A1,G1,P1)+compare5(A2,O2,G2,P2)+compare5(P3,A3,O3,G3)+compare5(G4,A4,P4,O4)
Ben9_4 <- fr(0,NF)+compare5(P1,A1,G1,O1)+compare5(G2,O2,A2,P2)+compare5(O3,A3,P3,G3)+compare5(A4,G4,P4,O4)
Ben9_5 <- fr(0,NF)+compare5(G1,A1,P1,O1)+compare5(P2,O2,A2,G2)+compare5(O3,A3,P3,G3)+compare5(A4,G4,P4,O4)
# zero reward for not frequently purchased & 3% for all other
Ben10 <- fr(0,NF)*1.57+fr(3,To-NF)*1.57
# zero reward for not frequently purchased & 3% for all other in order
compare6 <- function(X1,X2,X3,X4) {
  fr(3,X1)*1.57+fr(0,X2+X3+X4)
}
Ben10_1 <- fr(0,NF)+compare6(O1,A1,G1,P1)+compare6(A2,O2,G2,P2)+compare6(G3,A3,O3,P3)+compare6(P4,A4,G4,O4)
Ben10_2 <- fr(0,NF)+compare6(O1,A1,G1,P1)+compare6(A2,O2,G2,P2)+compare6(P3,A3,O3,G3)+compare6(G4,A4,P4,O4)
Ben10_3 <- fr(0,NF)+compare6(A1,O1,G1,P1)+compare6(O2,A2,G2,P2)+compare6(G3,A3,O3,P3)+compare6(P4,A4,G4,O4)
Ben10_4 <- fr(0,NF)+compare6(P1,A1,G1,O1)+compare6(G2,O2,A2,P2)+compare6(O3,A3,G3,P3)+compare6(A4,P4,G4,O4)
# zero reward for not frequently purchased & 4% for 1 category
compare7 <- function(X1,X2,X3,X4) {
  fr(4,X1)*1.57^2+fr(0,X2+X3+X4)
}
ben11 <- max(compare7(Office,Clothes,Food,Fuel),compare7(Clothes,Office,Food,Fuel),compare7(Food,Clothes,Office,Fuel),compare7(Fuel,Clothes,Food,Office))
Ben11 <- fr(0,NF)+ben11
# zero reward for not frequently purchased & 4% for 2 categories
compare8 <- function(X1,X2,X3,X4) {
  fr(4,X1+X2)*1.57^2+fr(0,X3+X4)
}
ben12 <- max(compare8(Office,Clothes,Food,Fuel),compare8(Office,Food,Clothes,Fuel),compare8(Office,Fuel,Food,Clothes),compare8(Clothes,Food,Office,Fuel),compare8(Clothes,Fuel,Office,Food),compare8(Food,Fuel,Office,Clothes))
Ben12 <- fr(0,NF)+ben12
# zero reward for not frequently purchased & 4% for 3 categories
compare9 <- function(X1,X2,X3,X4) {
  fr(4,X1+X2+X3+X4)*1.57^2+fr(0,X4)
}
ben13 <- max(compare9(Office,Clothes,Food,Fuel),compare9(Office,Clothes,Fuel,Food),compare9(Office,Fuel,Food,Clothes),compare9(Fuel,Clothes,Food,Office))
Ben13 <- fr(0,NF)+ben13
# zero reward for not frequently purchased & 4% for all others
Ben14 <- fr(0,NF)*1.57+fr(4,To-NF)*1.57^2
# zero reward for not frequently purchased & 4% for all other in time order
compare10 <- function(X1,X2,X3,X4) {
  fr(4,X1)*1.57^2+fr(0,X2+X3+X4)*1.57
}
Ben14_1 <- fr(0,NF)*1.57+compare10(O1,A1,G1,P1)+compare10(A2,O2,G2,P2)+compare10(G3,A3,O3,P3)+compare10(P4,A4,G4,O4)
Ben14_2 <- fr(0,NF)*1.57+compare10(P1,A1,G1,O1)+compare10(G2,O2,A2,P2)+compare10(O3,A3,G3,P3)+compare10(A4,P4,G4,O4)
Ben14_3 <- fr(0,NF)*1.57+compare10(A1,O1,G1,P1)+compare10(O2,A2,G2,P2)+compare10(G3,A3,O3,P3)+compare10(P4,A4,G4,O4)
Ben14_4 <- fr(0,NF)*1.57+compare10(G1,A1,P1,O1)+compare10(P2,O2,A2,G2)+compare10(O3,A3,G3,P3)+compare10(A4,P4,G4,O4)
# zero reward for not frequently purchased & 5% for 1 category
compare11 <- function(X1,X2,X3,X4) {
  fr(5,X1)*1.57^3+fr(0,X2+X3+X4)
}
ben15 <- max(compare11(Office,Clothes,Food,Fuel),compare11(Clothes,Office,Food,Fuel),compare11(Food,Clothes,Office,Fuel),compare11(Fuel,Clothes,Food,Office))
Ben15 <- fr(0,NF)+ben15
# zero reward for not frequently purchased & 5% for 2 categories
compare12 <- function(X1,X2,X3,X4) {
  fr(5,X1+X2)*1.57^3+fr(0,X3+X4)
}
ben16 <- max(compare12(Office,Clothes,Food,Fuel),compare12(Office,Food,Clothes,Fuel),compare12(Office,Fuel,Food,Clothes),compare12(Clothes,Food,Office,Fuel),compare12(Clothes,Fuel,Office,Food),compare12(Food,Fuel,Office,Clothes))
Ben16 <- fr(0,NF)*1.57+ben16
# zero reward for not frequently purchased & 5% for 3 categories
compare13 <- function(X1,X2,X3,X4) {
  fr(5,X1+X2+X3+X4)*1.57^3+fr(0,X4)*1.57
}
ben17 <- max(compare13(Office,Clothes,Food,Fuel),compare13(Office,Clothes,Fuel,Food),compare13(Office,Fuel,Food,Clothes),compare13(Fuel,Clothes,Food,Office))
Ben17 <- fr(0,NF)*1.57+ben17
# zero reward for not frequently purchased & 5% for all others
Ben18 <- fr(0,NF)*1.57+fr(5,To-NF)*1.57^3
# zero reward for not frequently purchased & 5% for all others in time order
compare14 <- function(X1,X2,X3,X4) {
  fr(5,X1)*1.57^3+fr(0,X2+X3+X4)*1.57
}
Ben18_1 <- fr(0,NF)*1.57+compare14(O1,A1,G1,P1)+compare14(A2,O2,G2,P2)+compare14(G3,A3,O3,P3)+compare14(P4,A4,G4,O4)
Ben18_2 <- fr(0,NF)*1.57+compare14(P1,A1,G1,O1)+compare14(G2,O2,A2,P2)+compare14(O3,A3,G3,P3)+compare14(A4,P4,G4,O4)
# zero reward for not frequently purchased & 10% for 1 category
compare15 <- function(X1,X2,X3,X4) {
  fr(10,X1)*1.57^4+fr(0,X2+X3+X4)
}
ben20 <- max(compare15(Office,Clothes,Food,Fuel),compare15(Clothes,Office,Food,Fuel),compare15(Food,Clothes,Office,Fuel),compare15(Fuel,Clothes,Food,Office))
Ben20 <- fr(0,NF)+ben15
# zero reward for not frequently purchased & 10% for 2 categories
compare16 <- function(X1,X2,X3,X4) {
  fr(10,X1+X2)*1.57^4+fr(0,X3+X4)*1.57
}
ben21 <- max(compare16(Office,Clothes,Food,Fuel),compare16(Office,Food,Clothes,Fuel),compare16(Office,Fuel,Food,Clothes),compare16(Clothes,Food,Office,Fuel),compare16(Clothes,Fuel,Office,Food),compare16(Food,Fuel,Office,Clothes))
Ben21 <- fr(0,NF)*1.57+ben21
# zero reward for not frequently purchased & 7% for 1 category
compare17 <- function(X1,X2,X3,X4) {
  fr(7,X1)*1.57^4+fr(0,X2+X3+X4)
}
ben22 <- max(compare17(Office,Clothes,Food,Fuel),compare17(Clothes,Office,Food,Fuel),compare17(Food,Clothes,Office,Fuel),compare17(Fuel,Clothes,Food,Office))
Ben22 <- fr(0,NF)+ben22
# zero reward for not frequently purchased & 7% for 2 categories
compare18 <- function(X1,X2,X3,X4) {
  fr(7,X1+X2)*1.57^4+fr(0,X3+X4)*1.57
}
ben23 <- max(compare18(Office,Clothes,Food,Fuel),compare18(Office,Food,Clothes,Fuel),compare18(Office,Fuel,Food,Clothes),compare18(Clothes,Food,Office,Fuel),compare18(Clothes,Fuel,Office,Food),compare18(Food,Fuel,Office,Clothes))
Ben23 <- fr(0,NF)*1.57+ben23
# zero reward for not frequently purchased & 10% for 3 categories
compare19 <- function(X1,X2,X3,X4) {
  fr(10,X1+X2+X3+X4)*1.57^4+fr(0,X4)*1.57^2
}
ben24 <- max(compare19(Office,Clothes,Food,Fuel),compare19(Office,Clothes,Fuel,Food),compare19(Office,Fuel,Food,Clothes),compare19(Fuel,Clothes,Food,Office))
Ben24 <- fr(0,NF)*1.57^2+ben24
# zero reward for not frequently purchased & 7% for 3 categories
compare20 <- function(X1,X2,X3,X4) {
  fr(7,X1+X2+X3+X4)*1.57^4+fr(0,X4)*1.57^2
}
ben25 <- max(compare20(Office,Clothes,Food,Fuel),compare20(Office,Clothes,Fuel,Food),compare20(Office,Fuel,Food,Clothes),compare20(Fuel,Clothes,Food,Office))
Ben25 <- fr(0,NF)*1.57^2+ben25
# zero reward for not frequently purchased & 10% for all others
Ben26 <- fr(0,NF)*1.57^3+fr(10,To-NF)*1.57^4
# zero reward for not frequently purchased & 7% for all others
Ben27 <- fr(0,NF)*1.57^3+fr(7,To-NF)*1.57^4
# zero reward for not frequently purchased & 10% for all other in time order
compare21 <- function(X1,X2,X3,X4) {
  fr(10,X1)*1.57^4+fr(0,X2+X3+X4)*1.57^2
}
Ben26_1 <- fr(0,NF)*1.57^2+compare21(O1,A1,G1,P1)+compare21(A2,O2,G2,P2)+compare21(G3,A3,O3,P3)+compare21(P4,A4,G4,O4)
Ben26_2 <- fr(0,NF)*1.57^2+compare21(P1,A1,G1,O1)+compare21(G2,O2,A2,P2)+compare21(O3,A3,G3,P3)+compare21(A4,P4,G4,O4)
# zero reward for not frequently purchased & 7% for all other in time order
compare22 <- function(X1,X2,X3,X4) {
  fr(7,X1)*1.57^4+fr(0,X2+X3+X4)*1.57^2
}
Ben27_1 <- fr(0,NF)*1.57^2+compare22(O1,A1,G1,P1)+compare22(A2,O2,G2,P2)+compare22(G3,A3,O3,P3)+compare22(P4,A4,G4,O4)
Ben27_2 <- fr(0,NF)*1.57^2+compare22(P1,A1,G1,O1)+compare22(G2,O2,A2,P2)+compare22(O3,A3,G3,P3)+compare22(A4,P4,G4,O4)
# 1% reward for not frequently purchased & 5% for all others in time order
compare23 <- function(X1,X2,X3,X4) {
  fr(5,X1)*1.57^3+fr(0,X2+X3+X4)*1.57^2
}
Ben28_1 <- fr(1,NF)*1.57^2+compare23(O1,A1,G1,P1)+compare23(A2,O2,G2,P2)+compare23(G3,A3,O3,P3)+compare23(P4,A4,G4,O4)
Ben28_2 <- fr(1,NF)*1.57^2+compare23(P1,A1,G1,O1)+compare23(G2,O2,A2,P2)+compare23(O3,A3,G3,P3)+compare23(A4,P4,G4,O4)
# 1% reward for not frequently purchased & 7% for all others in time order
compare23 <- function(X1,X2,X3,X4) {
  fr(7,X1)*1.57^4+fr(0,X2+X3+X4)*1.57^3
}
Ben28_1 <- fr(1,NF)*1.57^3+compare23(O1,A1,G1,P1)+compare23(A2,O2,G2,P2)+compare23(G3,A3,O3,P3)+compare23(P4,A4,G4,O4)
Ben28_2 <- fr(1,NF)*1.57^3+compare23(P1,A1,G1,O1)+compare23(G2,O2,A2,P2)+compare23(O3,A3,G3,P3)+compare23(A4,P4,G4,O4)
# 1% reward for not frequently purchased & 10% for all others in time order
compare24 <- function(X1,X2,X3,X4) {
  fr(10,X1)*1.57^4+fr(0,X2+X3+X4)*1.57^3
}
Ben29_1 <- fr(1,NF)*1.57^3+compare24(O1,A1,G1,P1)+compare24(A2,O2,G2,P2)+compare24(G3,A3,O3,P3)+compare24(P4,A4,G4,O4)
Ben29_2 <- fr(1,NF)*1.57^3+compare24(P1,A1,G1,O1)+compare24(G2,O2,A2,P2)+compare24(O3,A3,G3,P3)+compare24(A4,P4,G4,O4)



