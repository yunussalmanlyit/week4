# drugs

dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
drugs <- data.frame(dose, drugA, drugB)
str(drugs)
plot(drugs)

attach(drugs)
plot(dose, type = "o", col = "blue")

plot(dose, drugA, type = "b")

?plot
?par
# par functions

opar <- par(no.readonly = TRUE)
par(lty = 2, pch= 17)
plot(dose, drugA, type = "b")
