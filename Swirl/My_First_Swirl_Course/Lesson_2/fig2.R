plot(mpg~wt,data=mtcars,col=cyl,pch=19)
mdl<-lm(mpg~wt,data=mtcars)
abline(mdl,col='red')
