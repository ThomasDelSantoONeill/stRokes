data <- read.csv("DataRaw.csv")
benchmark <- data.frame(Distance = 1:600,
                        Tee = )

tee(1:600)

putt <- data.frame(Distance = c(3,4,5))
tee <- approxfun(data$Distance,data$Tee)
ferway <- approxfun(data$Distance,data$Fairway)
ferway(100)
tee(400)
sand <- approxfun(data$Distance,data$Sand)
sand(12)
val <- c()
for (i in 1:600) {
  s <- tee(i)
  val <- append(val, s)
}
val

plot(seq(1,600,1),seq(0,5,0.00834), type = "n")
points(1:600, val)
points(data$Distance,data$Tee, col = "red", pch = 16, cex = 2)
mod <- lm(data$Tee~data$Distance)
summary(mod)
abline(mod)
plot(mod)
mod2<-loess(data$Tee[10:length(data$Tee)]~data$Distance[10:length(data$Distance)],
            control=loess.control(surface="direct"))
lines(data$Distance[10:length(data$Distance)],predict(mod2), col = "green")
lines(1:99,predict(mod2, newdata = 1:99))
mod3 <- mgcv::gam(data$Tee~data$Distance)
summary(mod3)
par(mfrow = c(2,2))
mgcv::gam.check(mod3)

predict(mod3, data.frame(data$Distance = 1:99))
