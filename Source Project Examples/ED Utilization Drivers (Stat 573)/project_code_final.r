# Library
library(here)
library(tidyverse)
library(ggplot2)
library(boot)

# setwd
setwd('/home/petepham/Documents/School/STAT 573/Final Project/data')
getwd()

# functions
icdf = function(x, alpha){
    # x = data or vector
    # alpha = percentile of interest

    # sort & define length
    list.data = sort(x)
    n = length(x)
    # Apply logic for alpha < 0.50 && alpha >= 50
    i = ifelse(
        alpha < 0.50,
        floor(alpha * (n+1)),
        (n+1) - floor((1-alpha)*(n+1))
    )
    # Return results
    return(list.data[i])
}

# import
raw = read.csv('quality.csv')

colnames(raw)

df = raw %>%
    select(
        MemberID,
        OfficeVisits,
        ERVisits,
        MedicalClaims,
        InpatientDays
    ) %>%
    mutate(
        pcp.group = ifelse(
            OfficeVisits >= 14,
            'high.engagement',
            'low.engagement'
        )
    )

df.group = df %>%
    group_by(pcp.group) %>%
    summarise(
        patient_count = n(),
        total_er = sum(ERVisits),
        mean_er = mean(ERVisits),
        sd_er = sd(ERVisits)
    )

view(df.group)

hist(df$ERVisits)


# reframe
high = df$ERVisits[df$pcp.group == 'high.engagement']
low = df$ERVisits[df$pcp.group == 'low.engagement']

# data variables
n = length(df$ERVisits)-1
mean = mean(df$ERVisits)
var = var(df$ERVisits)
std.dev = sqrt(var)

n.high = length(high)
mean.high = mean(high)
var.high = var(high)
sd.high = sqrt(var.high)

n.low = length(low)
mean.low = mean(low)
var.low = var(low)
sd.low = sqrt(var.low)

###############################
# normal
###############################
mean.high
norm.high.lower = mean.high - (1.96*(sqrt(var.high/(n.high-1))))
norm.high.upper = mean.high + (1.96*(sqrt(var.high/(n.high-1))))
c(norm.high.lower, norm.high.upper)

mean.low
norm.low.lower = mean.low - (1.96*(sqrt(var.low/(n.low-1))))
norm.low.upper = mean.low + (1.96*(sqrt(var.low/(n.low-1))))
c(norm.low.lower, norm.low.upper)

###############################
# Poisson
###############################
mean.high
pois.high.lower = mean.high - (1.96*(sqrt(mean.high/(n.high-1))))
pois.high.upper = mean.high + (1.96*(sqrt(mean.high/(n.high-1))))
c(pois.high.lower, pois.high.upper)

mean.low
pois.low.lower = mean.low - (1.96*(sqrt(mean.low/(n.low-1))))
pois.low.upper = mean.low + (1.96*(sqrt(mean.low/(n.low-1))))
c(pois.low.lower, pois.low.upper)

###############################
##### boostrap-t, trimmed #####
###############################

tmean = function(x, gama = 0.2) {
    x = sort(x)
    n = length(x)
    a = mean(x, trim = gama)
    den = n * (1-2*gama)^2
    t = floor(n*gama)
    l = x[t+1]
    u = x[n-t]
    wv = c(rep(l, times=t),x[(t+1):(n-1)], rep(u, times=t))
    b = (a - tmuhat)/sqrt(var(wv)/den)
}

boot.t.mean = function(x, B) {
    n = length(x)
    boot.t = rep(0, B)

    for(i in 1:B) {
        xstar = sample(x, n, replace = TRUE)
        boot.t[i] = tmean(xstar)
    }

    return(boot.t)
}

# high
tmuhat.high = mean(high)
var.t.high = var(high)
den.high = length(high)*(1-2*0.2)^2
sqrt(var.t.high/den.high)

out.high = boot.t.mean(high, 1000)

tmuhat.high - icdf(out.high, 0.05)*sqrt(var.t.high/den.high) # upper
tmuhat.high - icdf(out.high, 0.95)*sqrt(var.t.high/den.high) # lower

# low
tmuhat.low = mean(low)
var.t.low = var(low)
den.low = length(low)*(1-2*0)^2
sqrt(var.t.low/den.low)

out.low = boot.t.mean(low, 1000)

tmuhat.low - icdf(out.low, 0.05)*sqrt(var.t.low/den.low) # upper
tmuhat.low - icdf(out.low, 0.95)*sqrt(var.t.low/den.low) # lower


###############################
##### BCA #####
###############################

library(boot)

mean.boot = function(x, i) {
    return(
        c(
            mean(x[i]), sqrt(var(x[i]))/sqrt(length(i))
        )
    )
}


# high
bca.boot.high = boot(
    high,
    mean.boot,
    R = 1000
)

boot.ci(
    bca.boot.high,
    conf = 0.95,
    type = c(
        'norm', 'perc', 'basic', 'stud', 'bca'
    )
)

# low
bca.boot.low = boot(
    low,
    mean.boot,
    R = 1000
)

boot.ci(
    bca.boot.low,
    conf = 0.95,
    type = c(
        'norm', 'perc', 'basic', 'stud', 'bca'
    )
)
