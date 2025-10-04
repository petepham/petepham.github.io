# Library
library(here)
library(tidyverse)
library(ggplot2)

# setwd
setwd('/home/petepham/Documents/School/STAT 573/Final Project/data')
getwd()

##### import 
patient = read.csv('diabetic_data.csv')
colnames(patient)
hist(patient$age)
hist(patient$num_medications)
hist(patient$number_outpatient)
hist(patient$number_emergency)
hist(patient$number_inpatient)

df = patient %>%
    filter(
        admission_type_id %in% c(1,2,3,5,6)
    ) %>%
    select(
        patient_nbr,
        A1Cresult,
        number_outpatient,
        readmitted,
        time_in_hospital
    ) %>%
    mutate(
        hba1c.group = ifelse(
            A1Cresult == '>8',
            'Poor Control',
            'Good Control'
            ),
        outpatient.group = ifelse(
            number_outpatient > 0,
            'Engaged',
            'Unengaged'
        ),
        readmitted = ifelse(
            readmitted == 'NO',
            0,
            1
        )
    )
    
df.diabetes = df %>%
    filter(
        A1Cresult != 'None'
    ) %>%
    group_by(
        patient_nbr,
        hba1c.group
    ) %>%
    summarise(
        days = sum(time_in_hospital)
    )

summary.diabetes = df.diabetes %>%
    group_by(hba1c.group) %>%
    summarise(
        count_members = n(),
        days.total = sum(days)
    )

view(summary.diabetes)

df.outpatient = df %>%
    group_by(
        patient_nbr,
        outpatient.group
    ) %>%
    summarise(
        days = sum(time_in_hospital)
    ) 

summary.outpatient = df.outpatient %>%
    group_by(outpatient.group) %>%
    summarise(
        count_members = n(),
        days.total = sum(days)
    )

view(summary.outpatient)

##### Create Samples #####
good.control = sample(df.diabetes$days[df.diabetes$hba1c.group == 'Good Control'], 100, replace = TRUE)
poor.control = sample(df.diabetes$days[df.diabetes$hba1c.group == 'Poor Control'], 100, replace = TRUE)


engaged = sample(df.outpatient$days[df.outpatient$outpatient.group == 'Engaged'], 100, replace = TRUE)
unengaged = sample(df.outpatient$days[df.outpatient$outpatient.group == 'Unengaged'], 100, replace = TRUE)

################# bootstrap-t to estimate confidence interval #######################
# Define icdf function
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

boot.t = function(x, B) {
    n = length(x)
    boot.t = rep(NA,B)

    # Use non-winsorized tmean
    for(i in 1:B) {
        xstar = sample(x, n, replace = TRUE)
        boot.t[i] = mean(xstar, 0.5) 
    }
    return(boot.t)
}

#### poor control
output.poor = boot.t(poor.control, B=1000)

# mean
mean.poor = mean(output.poor)
sdev.poor = sqrt(var(output.poor))
median.poor = median(output.poor)
c(mean.poor, sdev.poor, median.poor)

# Bias
mean.poor-mean(poor.control)

# Normal CI @ 90%, compare to [448.19109, 451.80891]
median.poor - (sdev*1.645)
median.poor + (sdev*1.645)

# Parametric CI
tmu = mean(poor.control)
se.poor = var(poor.control) / length(poor.control)
lower.poor = icdf(output.poor, 0.025)
upper.poor = icdf(output.poor, 0.975)

tmu - (lower.poor * se.poor)
tmu - (upper.poor * se.poor)
