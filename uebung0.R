##################Übung 1##################

#messadaten pippetiert
pipettiert_gewicht <- c(19.8689, 19.8542, 19.8443, 19.8560,19.8489,
                        19.8785, 19.8619, 19.8747, 19.8500, 19.8455) #g
#pipettierte volumen
dichte_wasser <- 0.99820 #g/cm^3 = g/mL
pipettiert <- pipettiert_gewicht / dichte_wasser #mL

#n
pip_n <- length(pipettiert)
#mean
pip_mean <- mean(pipettiert)
#standard deviation(sd)
pip_sd <- sd(pipettiert)
#variance
pip_var <- var(pipettiert)
#sd of mean
pip_mean_sd <- pip_sd / sqrt(pip_n)
#variance of mean
pip_mean_var <- pip_var / pip_n

#Output
data.frame(pip_mean, pip_sd, pip_var, pip_mean_sd, pip_mean_var)

################## END Übung 1##################




##################Übung 2##################

#degrees of freedom
pip_v <- pip_n <- 1

#fractiles
pip_fractile_95 <- qt(0.975, pip_v)
pip_fractile_99 <- qt(0.995, pip_v)

# +- im Vertrauensintervall
pip_cx_95 <- pip_fractile_95 * pip_mean_sd
pip_cx_99 <- pip_fractile_99 * pip_mean_sd

cat("95 % Vertrauensintervall : [", pip_mean - pip_cx_95,
    " mL,", pip_mean + pip_cx_95, "mL]")

cat("99 % Vertrauensintervall : [", pip_mean - pip_cx_99,
    " mL,", pip_mean + pip_cx_99, "mL]")

fig.height=3
fig.width=10
plot.new()
arrows(pip_mean - pip_cx_95, 1, pip_mean + pip_cx_95, code=3, angle=90, length=0.02)
#plot(pip_mean, 1, ylab="",yaxt="n", xlim=c(19.7,20.1))

##################END Übung 2##################



##################Übung 3##################

#Vertrauensintervall
cat("\n19.90 +- 0.05 mL")


##################END Übung 3##################


##################Übung 3##################

datum <- c(1983.504, 1987.663, 1988.628, 1988.732, 1991.452, 1991.649, 1994.514, 1996.570, 1999.458, 2005.452, 2006.444, 2006.630, 2007.690, 2008.415, 2008.626, 2009.652)
zeit <-c(9.93, 9.93, 9.93, 9.92, 9.90, 9.86, 9.85, 9.84, 9.79, 9.77, 9.77, 9.77, 9.74, 9.72, 9.69, 9.58)

linear_model <-lm(zeit ~ datum)
summary(linear_model)
plot(datum , zeit)
abline(linear_model)

##################END Übung 3##################


  