##################################################
# Filename: hw2.R
# Purpose : Experimental Design, HW2 6-1 (d) (e)
# Author  : Potumas Liu
# Date    : 2025/03/30
##################################################


##################################################
# 6-1 (d)
##################################################
# the fitted model
estimate <- function (vec) {
    r <- 3; N <- r*8
    b0  <- 980 / N 
    b1  <- 4 / (r*4) / 2
    b2  <- 136 / (r*4) / 2
    b3  <- 82 / (r*4) / 2
    b13 <- (-106) / (r*4) / 2

    x1 <- vec[1]; x2 <- vec[2]; x3 <- vec[3]

    return ( b0 + b1*x1 + b2*x2 + b3*x3 + b13*x1*x3 )
}

# original data
dat <- c(22, 31, 25,
         32, 43, 29,
         35, 34, 50,
         55, 47, 46,
         44, 45, 38,
         40, 37, 36,
         60, 50, 54, 
         39, 41, 47)

# coded factors
effect <- list(c(-1, -1, -1), 
               c( 1, -1, -1),
               c(-1,  1, -1), 
               c( 1,  1, -1),
               c(-1, -1,  1),
               c( 1, -1,  1),
               c(-1,  1,  1),
               c( 1,  1,  1))

# estimated values
estimated <- lapply(effect, estimate)
estimated <- t(sapply(estimated, FUN = function(x) { return(rep(x, 3)) }))
print(estimated)

# residuals
residual <- dat - as.numeric(t(estimated))
print(residual)


# plot the residuals
res <- as.numeric(t(residual)); est <- as.numeric(t(estimated))
se <- sqrt(30.167)
print(res); print(est)

pdf("6-1-(d)-residuals.pdf")

par(mfrow=c(2, 2), 
    mar = c(3.5, 3.5, 1.8, 1.3),  # margin for each side (bltr)
    font.main = 2,  # 1: normal, 2: bold
    mgp = c(2.3, 0.8, 0))  # Default is c(3, 1, 0)

# font sizes, point types
lab <- 1; main <- 1.1; pch <- 1; cex <- 1

# histogram
hist(res, las=1, bty="l", cex.lab=lab, cex.main=main,
     main="Histogram of the Residuals", xlab="Residuals", 
     col="skyblue")
# scatter plot of residuals and fitted values
plot(x=est, y=res, las=1, bty="l",
     cex.lab=lab, cex.main=main, pch=pch, cex=cex,
     main="Residuals vs. Fitted", xlab="Fitted Value", ylab="Residuals")
lines(lowess(est, res), col="blue", lwd=2)
# QQ plot
qqnorm(res, main="QQ-Plot of the Residuals", bty="l", 
       las=1, pch=pch, cex.main=main, cex=cex, cex.lab=lab)
qqline(res, col="red", lwd=2)
# Standardized Residual Plot
plot(est, res/se, las=1, pch=pch, bty="l", 
     cex.main=main, cex=cex, cex.lab=lab,
     main="Standardized Residuals vs. Fitted",          
     xlab="Fitted Value", ylab="Standardized Residuals")
    lines(lowess(est, res/se), col="blue", lwd=2)

dev.off()


# testing residual normality
ksP <- ks.test(res, "pnorm")$p
adP <- nortest::ad.test(res)$p
swP <- shapiro.test(res)$p
llP <- nortest::lillie.test(res)$p

# testing non-constant variance of residuals
effectMtx <- sapply(effect, FUN = function(x) { 
        x1 <- x[1]
        x2 <- x[2]
        x3 <- x[3]
        x13 <- x1 * x3
        return(rep(c(x1, x2, x3, x13), 3))
    })
effectMtx <- 
    data.frame(matrix(as.numeric(effectMtx), ncol=4, byrow=TRUE))

dat <- cbind(dat, effectMtx)
bpP <- car::ncvTest(lm(dat~., data=dat))$p

testRes <- c(ksP, adP, swP, llP, bpP)
names(testRes) <- c("KS", "AD", "SW", "Li", "BP")
write.csv(testRes, "6-1-(d)-testResults.csv")



##################################################
# 6-1 (e)
##################################################
# interaction plot between factor A and factor C
rawDat <- dat[-5]
colnames(rawDat) <- c("y", "A", "B", "C")
print(rawDat)

# interaction plot
pdf("6-1(e).pdf")
par(mar=c(4.5, 4.5, 1, 1))
interaction.plot(
    x.factor = rawDat$A,      
    trace.factor = rawDat$C,     
    response = rawDat$y,     
    col = c("blue", "red"),             
    las=1, lty = 1, lwd = 2, pch = 16,
    main = "",
    ylab = "Life (in hours)", xlab = "Cutting Speed (A)",
    cex.lab=1.5, cex.axis=1.5,
    legend=F, 
    panel.first = {
        grid(nx=NULL, ny=NULL, col="gray80", lty=1, lwd=1)
    }
)
legend("topright", title="Cutting Angle (C)",
       legend=c("-1", "1"), col=c("blue", "red"), lwd=2,
       title.cex=1.5, cex=1.5, bg="white")
dev.off()



# Additional code for non-constant variance test

# manually conducting the BP test
# create squared residuals
#res_sq <- res^2
#effectMtx <- cbind(res_sq, effectMtx)
# regressing squared residuals on intercept
#aux_model <- lm(res_sq ~ ., data=effectMtx)
#R2 <- summary(aux_model)$r.square
#k <- ncol(effectMtx)-1
#n <- nrow(effectMtx)

# chi-square statistic
#LM <- n*R2
#print(1 - pchisq(LM, df=k))

# F statistic
#Fvalue <- ( R2 / k ) / (1 - (R2 / (n-k-1))) 
#print(1 - pf(Fvalue, df1=k, df2=(n-k-1)))

# another ncv test
#lmtest::bptest(lm(dat~., data=dat))
