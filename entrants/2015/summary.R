d <- read.csv('base.csv') 
for(i in 1:length(d)){
  d[i][d[i] == ""] <- NaN
}
d$target_speciality_id <- as.factor(d$target_speciality_id)
d$target_organization_id <- as.factor(d$target_organization_id)
d$nationality_type_id <- as.factor(d$nationality_type_id)
d$region_id <- as.factor(d$region_id)
d$gender_id <- as.factor(d$gender_id)
d$identity_document_type_id <- as.factor(d$identity_document_type_id)
d$identity_document_series <- as.factor(d$identity_document_series)
d$identity_document_number <- as.factor(d$identity_document_number)
d$education_document_type_id <- as.factor(d$education_document_type_id)
d$education_document_series <- as.factor(d$education_document_series)
d$education_document_number <- as.factor(d$education_document_number)
for(i in 1:18){
  d[paste("X", as.character(i), sep='')] <- as.factor(d[[paste("X", as.character(i), sep='')]])
}
d[paste("X", as.character(1), sep='')]
d$summa <- d$chemistry + d$biology + d$russian
d$summa_full <- ifelse(d$achievement == T, d$summa + 10, d$summa)
d_competiton <- subset(d, d$chemistry >= 36 & d$biology >= 36 & d$russian >= 36 & is.na(d$last_deny_day))
summary(d_competiton)
plot(table(d_competiton$))
plot(table(d_competiton$registration_date))

rd <- table(d$registration_date)
od <- table(d$original_received_date)
rd$type <- "rd"
cbind(rd, od)
