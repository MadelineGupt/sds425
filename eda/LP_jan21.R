
setwd('/Users/linhpham/Downloads/sds425/class_2_jan21')

# Main Question to Answer : Do restraints reduce fatalities in motor accidents? 

# Person.REST_USE :
# - 0 None Used 
# - 1 shoulder belt 
# - 2 lap belt
# - 3 lap and shoulder belt 
# - 4 child safety seat
# - 5 motorcycle helmet 
# - 8 Other Restraint Used 
# - 9 unknown 

# What data set have you selected? 
# I selected national data reported between the years 1991-1993. This is because
# I am interested in seeing how restraints impact fatalities in motor accidents. 
# The main variables I plan to focus on are REST_USE and INJ_SEV/FATALS. REST_USE
# indicates what restraint was use and if so, which one. 


# Method of Analysis 

# First, I load in my data

accident91 <- read.csv("FARS1991NationalCSV/ACCIDENT.CSV")
person91 <- read.csv("FARS1991NationalCSV/PERSON.CSV")
vehicle91 <- read.csv("FARS1991NationalCSV/VEHICLE.CSV")

accident92 <- read.csv("FARS1992NationalCSV/ACCIDENT.CSV")
person92 <- read.csv("FARS1992NationalCSV/PERSON.CSV")
vehicle92 <- read.csv("FARS1992NationalCSV/VEHICLE.CSV")

accident93 <- read.csv("FARS1993NationalCSV/ACCIDENT.CSV")
person93 <- read.csv("FARS1993NationalCSV/PERSON.CSV")
vehicle93 <- read.csv("FARS1993NationalCSV/VEHICLE.CSV")

accident <- rbind(accident91,accident92, accident93)

colnames(person93)[66] <- "DRUGTEST"
colnames(person93)[67] <- "DRUG_RES"
person93 <- person93[, !(names(person93) %in% c("DRUGTST2", "DRUGRES2", "DRUGTST3", "DRUGRES3"))]
person <- rbind(person91, person92, person93)

vehicle <- rbind(vehicle91, vehicle92, vehicle93)

X <- merge(accident, person)
X <- merge(X, vehicle)

# I merged in all years into one data set X. Because REST_USE is a scale from 1-9, 
# I also make a separate column that is only binary 

#ChatGPT 
X$restraints<- ifelse(X$REST_USE == 0, 0, 
     ifelse(X$REST_USE == 9, NA, 1))

X <- X[, colnames(X) %in% c("HOUR", "MONTH", "DAY", "AGE", "SEX", "PER_TYP",
    "EJECTION","AIR_BAG","DRINKING", "DRUGS", "HOSPITAL", 
    "ALC_RES", "INJ_SEV" , "SEAT_POS", 
    "VE_FORMS", "FATALS", "PREV_ACC", "FATALS",
    "PREV_DWI", "PREV_SPD", "PREV_OTH", "REST_USE", "restraints")]



# I am going to filter out where y is unknown

X <- X[X$REST_USE != 9,] # Now the size is (238776, 22)

# I also checked to make sure it was the same with y2 
# t <- X[complete.cases(y2),]

# With X and y, we can now model a linear regression 


lm.r = lm(REST_USE ~ VE_FORMS + MONTH + DAY + HOUR + FATALS + AGE + SEX + 
            PER_TYP + SEAT_POS + EJECTION + DRINKING + INJ_SEV + HOSPITAL +
            AIR_BAG + DRUGS  + ALC_RES + PREV_ACC + PREV_DWI  + PREV_SPD + 
            PREV_OTH, data = X)
summary(lm.r)


lm.s = lm(restraints ~ VE_FORMS + MONTH + DAY + HOUR + FATALS + AGE + SEX + 
            PER_TYP + SEAT_POS + EJECTION + DRINKING + INJ_SEV + HOSPITAL + 
            AIR_BAG + DRUGS  + ALC_RES + PREV_ACC + PREV_DWI  + PREV_SPD + 
            PREV_OTH , data = X)
summary(lm.s)

# Depending on which response variable I choose, I get different results. 


lm.r = lm(FATALS ~ VE_FORMS + MONTH + DAY + HOUR + AGE + SEX + 
            PER_TYP + SEAT_POS + EJECTION + DRINKING  + HOSPITAL +
            AIR_BAG + DRUGS  + ALC_RES + PREV_ACC + PREV_DWI  + PREV_SPD + 
            PREV_OTH + REST_USE, data = X)
summary(lm.r)


lm.s = lm(FATALS ~ VE_FORMS + MONTH + DAY + HOUR + FATALS + AGE + SEX + 
            PER_TYP + SEAT_POS + EJECTION + DRINKING + HOSPITAL + 
            AIR_BAG + DRUGS  + ALC_RES + PREV_ACC + PREV_DWI  + PREV_SPD + 
            PREV_OTH + restraints ,data = X)
summary(lm.s)

# This was not as effective 

X$fatality<- ifelse(X$INJ_SEV == 4, 1, 
                      ifelse(X$REST_USE == 9, NA, 0))

lm.r = lm(fatality ~ VE_FORMS + MONTH + DAY + HOUR + AGE + SEX + 
            PER_TYP + SEAT_POS + EJECTION + DRINKING  + HOSPITAL +
            AIR_BAG + DRUGS  + ALC_RES + PREV_ACC + PREV_DWI  + PREV_SPD + 
            PREV_OTH + REST_USE, data = X)
summary(lm.r)


lm.s = lm(fatality~ VE_FORMS + MONTH + DAY + HOUR + AGE + SEX + 
            PER_TYP + SEAT_POS + EJECTION + DRINKING + HOSPITAL + 
            AIR_BAG + DRUGS  + ALC_RES + PREV_ACC + PREV_DWI  + PREV_SPD + 
            PREV_OTH + restraints ,data = X)
summary(lm.s)

# Now just restraints 

lm.s = lm(fatality ~ restraints ,data = X)
summary(lm.s) 

# R Squared is extremely low 

