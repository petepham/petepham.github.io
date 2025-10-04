# Set WD
setwd('c:/wd/School/STAT 573/Final Project')
data.folder = paste0(getwd(),"/data/")
# Data Import
data.folder

patient = read.csv(
    paste0(
        data.folder,
        'Patient_Data_Training_Kaggle_V1.csv'
    ),
    check.names = FALSE
)

length(unique(patient$State))
length(colnames(df))
medication = read.csv(
    paste0(
        data.folder,
        'Medication_Data_Kaggle_V1.csv'
    )
)

socio = read.csv(
    paste0(
        data.folder,
        'Socio_Economics_Data_Kaggle_V1.csv'
    )
)

df = patient %>%
    select(
        ID,
        Year,
        Age,
        Race,
        Sex,
        '# Labs',
        '# All Physician OP',
        '# Readmissions',
        '# ER',
        '# ER Admissions',
        '# Inpatient',
        '# Short Term Stay Hospital',
        '# Long Term Stay Hospital',
        '# Rx Claims',
        '# DME',
        'TotalCost_Y_Actual'
    ) %>%
    mutate(across(where(is.character), str_trim))

names(df) = c(
    'ID',
    'Year',
    'Age',
    'Race',
    'Sex',
    'labs',
    'outpatient.visits',
    'readmissions',
    'er.visits',
    'er.readmissions',
    'inpatient.visits',
    'shortterm.stays',
    'longterm.stays',
    'rx.claims',
    'dme',
    'totalcost',
    'hospital.indicator'
)

df.out = df %>%
    mutate(
        hospital.visits = sum(
            readmissions,
            er.visits,
            er.readmissions,
            inpatient.visits,
            shortterm.stays,
            longterm.stays,
            na.rm = TRUE
        )
    )


nrow(df.out)

write.csv(df,"data.csv")

hist(df.out$totalcost)
hist(df.out$outpatient.visits)
