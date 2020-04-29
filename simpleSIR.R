# R Code for simple SIR model of "7 day photo challenge" #
# Graham Wheeler (Twitter: @DrGWheeler)
# GitHub: https://github.com/graham-wheeler/
#
# Thanks to Aidan Findlater (Twitter: @AidanFindLater) for his original code, which this file is based on.
# Original code source: https://archives.aidanfindlater.com/blog/2010/04/20/the-basic-sir-model-in-r/

#############################
# Install and load packages #
#############################

install.packages("deSolve")
library(deSolve)

############
# Function #
############

sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}


#########
# Setup #
#########

n_users <- 330000000 # Est. number of Twitter users
genuine_users <- 0.9*n_users # Est. number that are not bots capable of tweeting and infecting other users
initial_infected <- 100 # Initial number of accounts that will start the challenge
init <- c(S = 1-(initial_infected/genuine_users), I = initial_infected/genuine_users, 0.0)

r0 <- 2 # Assumed Reproduction Ratio (Average number of users that one person will infect)
duration <- 7 # Duration of infection (days)
parameters <- c(beta = r0/duration, gamma = 1/duration)

######################
# Model the epidemic #
######################

times <- seq(0, 180, by = 1)
out <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters))
out$time <- NULL

####################
# Plot the results #
####################

subtitle_text<-ifelse(initial_infected == 1, paste0("R0 = ",r0,"; Duration of infection = ", duration," days; Assumed non-bot Twitter population = ", format(genuine_users, scientific = FALSE), " users; ", initial_infected," user starts the challenge"),
                      paste0("R0 = ",r0,"; Duration of infection = ", duration," days; Assumed non-bot Twitter population = ", format(genuine_users, scientific = FALSE), " users; ", initial_infected," users start the challenge"))
matplot(times, out, type = "l", xlab = "Time (days)", ylab = "Proportion of population in each state", main = "SIR Model - 7 Day Photo Challenge", lwd = 5, lty = 1:3, bty = "l", col = c("orange", "red", "blue"), las = 1, xaxt="n")
axis(side = 1, at = seq(0, max(times), by = 30), label = as.character(seq(0, max(times), by = 30)))
mtext(text = subtitle_text, side = 3, line = 0)
legend(max(times), 0.5, xjust = 1, c("Susceptible", "Infectious", "Recovered"), pch = NULL, lty = 1:3, lwd = 5, col = c("orange", "red", "blue"), cex = 1.5)


#############################################
# Looking at several different values of R0 #
#############################################

r0_vec <- c(2, 3, 4, 5, 6, 7)
duration <- 7
parameters <- matrix(c(beta = r0_vec/duration, gamma = rep(1/duration, length(r0_vec))), ncol = 2, byrow = F)
colnames(parameters)<-c("beta", "gamma")
times <- seq(0, 180, by = 1)
out_list<-lapply(1:length(r0_vec), function(z) {tmp<-as.data.frame(ode(y = init, times = times, func = sir, parms = parameters[z,]))
  tmp$time <- NULL
  tmp})
y_max<-max(sapply(1:length(r0_vec), function(z) max(out_list[[z]]$I)))

#####################################
# Plotting Proportion infected only #
#####################################

subtitle_text<-ifelse(initial_infected == 1, paste0("Duration of infection = ", duration," days; Assumed non-bot Twitter population = ", format(genuine_users, scientific = FALSE), " users; ", initial_infected," user starts the challenge"),
                      paste0("Duration of infection = ", duration," days; Assumed non-bot Twitter population = ", format(genuine_users, scientific = FALSE), " users; ", initial_infected," users start the challenge"))
matplot(times, out_list[[1]]$I, type = "l", xlab = "Time (days)", ylab = "Proportion of population infected", main = "SIR Model - 7 Day Photo Challenge", lwd = 5, lty = 1, bty = "l", col = 1, las = 1, ylim = c(0,y_max), xaxt="n")
for(i in 2:length(r0_vec)){
  lines(times, out_list[[i]]$I, type = "l", lwd = 5, lty = i, col = i)
}
axis(side = 1, at = seq(0, max(times), by = 30), label = as.character(seq(0, max(times), by = 30)))
mtext(text = subtitle_text, side = 3, line = 0)
legend(max(times), y_max, xjust = 1, as.character(r0_vec), title = "R0 value", lwd = 5, col = 1:length(r0_vec), lty = 1:length(r0_vec))

#######
# END #
#######
