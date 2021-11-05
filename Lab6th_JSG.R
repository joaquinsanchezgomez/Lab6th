
#Lab 6
#Joaquin Sanchez-Gomez

#I decided to change some variables, to see if anxiety and perhaps sexual orientation
#are significant to determine if people decide to be vaccinated.


#Lab 6

model_logit1 <- glm(vaxx ~ EEDUC,
                    family = binomial, data = Household_Pulse_data)

#First Step

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 

#to confirm that we don't have NA
sum(is.na(Household_Pulse_data$RECVDVACC))

table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)

summary(Household_Pulse_data$vaxx)

summary(as.numeric(Household_Pulse_data$vaxx))

vaxx_factor <- as.factor(Household_Pulse_data$vaxx)

levels(vaxx_factor)

levels(vaxx_factor) <- c("no","yes")

glm(RECVDVACC ~ EEDUC,family = binomial)

pick_use1 <- (Household_Pulse_data$REGION == "South") # just for example!
dat_use1 <- subset(Household_Pulse_data, pick_use1)

# and to be finicky, might want to use this for factors after subsetting in case some get lost
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 

model_logitX <- glm(vaxx ~ TBIRTH_YEAR + EEDUC + ANXIOUS + RRACE + SEXUAL_ORIENTATION + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logitX)









