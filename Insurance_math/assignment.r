print("worked")

file_name <- "PolandLifeTable.txt"

life_table <- read.delim(file_name, header = TRUE, stringsAsFactors = FALSE, skip = 2, sep = "")
str(life_table)
head(life_table)
year <- life_table$Year
age <- life_table$Age
qx <- life_table$qx
ex <- life_table$ex
px <- (1 - qx)

       # a) Approximate μx,t = qx,t, using Taylor approx and UDD stating
# that mortality rate is constant between two integer years

μx1958 <- qx[year == 1958 & age != "110+"]
agesx <- c(0:109)
μx1990 <- qx[year == 1990 & age != "110+"]
μx2019 <- qx[year == 2019 & age != "110+"]

par(mar = c(5, 5, 4, 2) + 0.5)
plot(agesx, μx1958,
    main = "Mortality rates for each age (Poland, 1958, 1990 and 2019)",
    xlab = "Age x",
    ylab = expression(paste("Mortality rate ", μ[x])),
    type = "l",
    col = "blue",
    ylim = c(0, 0.55),
    cex.main = 2,
    cex.lab = 2,
    cex.axis = 2,
    lwd = 2)
lines(agesx, μx1990, col = "red", lwd = 2)
lines(agesx, μx2019, col = "#077b07", lwd = 2)
legend("topleft", legend = c("1958", "1990", "2019"),
       col = c("blue", "red", "#077b07"), lty = 1, cex = 2)

# b) Again, we approximate μx,t = qx,t
yearst <- c(1958:2019)
μx0 <- qx[age == 0]
μx20 <- qx[age == 20]
μx50 <- qx[age == 50]
μx80 <- qx[age == 80]

plot(yearst, μx0,
    main = "Mortality rates for each year (Poland, 0, 20, 50, 80 years old)",
    xlab = "Year of the survey t",
    ylab = expression(paste("Mortality rate ", μ[x])),
    type = "l",
    col = "blue",
    ylim = c(0, 0.12),
    cex.main = 2,
    cex.lab = 2,
    cex.axis = 2,
    lwd = 3)
lines(yearst, μx20, col = "red", lwd = 3)
lines(yearst, μx50, col = "#077b07", lwd = 3)
lines(yearst, μx80, col = "#6a0259", lwd = 3)
legend("topright", legend = c("0 years old", "20 years old", "50 years old", "80 years old"),
       col = c("blue", "red", "#077b07", "#6a0259"), lty = 1, cex = 2)


# c) To calculate that, we need to use multiplication formula to
# express each S_0(x)=x_p_0 as p_0*p_1*p_2*...*p_(x-1)
# To do that, we use comprod and add 1 in the beginning as 0_p_0 = 1.
px_1958 <- px[year == 1958 & age != "110+"]
xp0_1958 <- c(1, cumprod(px_1958[(1):(110)]))

px_1990 <- px[year == 1990 & age != "110+"]
xp0_1990 <- c(1, cumprod(px_1990[(1):(110)]))

px_2009 <- px[year == 2009 & age != "110+"]
xp0_2009 <- c(1, cumprod(px_2009[(1):(110)]))

px_2019 <- px[year == 2019 & age != "110+"]
xp0_2019 <- c(1, cumprod(px_2019[(1):(110)]))

ages_x2 <- (0:110)

plot(ages_x2, xp0_1958,
    main = "Survival function of a newborn (Poland, 1958, 1990, 2009, 2019)",
    xlab = "Age x",
    ylab = expression(paste("Survival function of (0) ", S_0(x))),
    type = "l",
    col = "blue",
    ylim = c(0, 1),
    cex.main = 2,
    cex.lab = 2,
    cex.axis = 2,
    lwd = 3)
lines(ages_x2, xp0_1990, col = "red", lwd = 2)
lines(ages_x2, xp0_2009, col = "#077b07", lwd = 2)
lines(ages_x2, xp0_2019, col = "#6a0259", lwd = 2)
legend("topright", legend = c("1958", "1990","2009", "2019"),
       col = c("blue", "red", "#077b07", "#6a0259"), lty = 1, cex = 2)


# d) Future life expectancies for a person that is aged x at time t

e_0_t <- ex[age == 0]
e_20_t <- ex[age == 20]
e_40_t <- ex[age == 40]
e_70_t <- ex[age == 70]
e_90_t <- ex[age == 90]
yearst <- c(1958:2019)

plot(yearst, e_0_t,
    main = "Future life expectancies for a person that is aged x at time t (Poland, 0, 10, 20, 40, 70, 90 years old)",
    xlab = "Time t",
    ylab = "Future life expectancy E_x(t) ",
    type = "l",
    col = "blue",
    ylim = c(0, 110),
    cex.main = 1.2,
    cex.lab = 2,
    cex.axis = 2,
    lwd = 3)
lines(yearst, e_20_t, col = "red", lwd = 3)
lines(yearst, e_40_t, col = "#077b07", lwd = 3)
lines(yearst, e_70_t, col = "#6a0259", lwd = 3)
lines(yearst, e_90_t, col = "#fda605", lwd = 3)
legend("topright", legend = c("0 years old", "20 years old", "40 years old", "70 years old", "90 years old"),
       col = c("blue", "red", "#077b07", "#6a0259", "#fda605"), lty = 1, cex = 2)

