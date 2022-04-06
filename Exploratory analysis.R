source("Rfunctions.R")
Default <- read.csv("tw_credit_data.csv", stringsAsFactors = TRUE)
library(matrixStats)
library(corrplot)

Default_1 <- Default
Default_test <- Default


colnnms_pay <- c("PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6")
colnnms_bill <- c("BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")

Default_test$PAY_TOTAL <- rowSums(Default[,colnnms_pay])
Default_test$BILL_TOTAL <- rowSums(Default[,colnnms_bill])



#OUTLIERS 

Default_test$out_LIMIT_BAL<- ifelse(Default_test$LIMIT_BAL %in% boxplot.stats(Default_test$LIMIT_BAL)$out, 1, 0)
Default_test$LIMIT_BAL[Default_test$out_LIMIT_BAL == 1] <- NA
summary(Default_test$LIMIT_BAL)

Default_test$out_BIL1<- ifelse(Default_test$BILL_AMT1 %in% boxplot.stats(Default_test$BILL_AMT1)$out, 1, 0)
Default_test$BILL_AMT1[Default_test$out_BIL1 == 1] <- NA
summary(Default_test$BILL_AMT1)

Default_test$out_BIL2<- ifelse(Default_test$BILL_AMT2 %in% boxplot.stats(Default_test$BILL_AMT2)$out, 1, 0)
Default_test$BILL_AMT2[Default_test$out_BIL2 == 1] <- NA
summary(Default_test$BILL_AMT2)

Default_test$out_BIL3<- ifelse(Default_test$BILL_AMT3 %in% boxplot.stats(Default_test$BILL_AMT3)$out, 1, 0)
Default_test$BILL_AMT3[Default_test$out_BIL3 == 1] <- NA
summary(Default_test$BILL_AMT3)

Default_test$out_BIL4<- ifelse(Default_test$BILL_AMT4 %in% boxplot.stats(Default_test$BILL_AMT4)$out, 1, 0)
Default_test$BILL_AMT4[Default_test$out_BIL4 == 1] <- NA
summary(Default_test$BILL_AMT4)

Default_test$out_BIL5<- ifelse(Default_test$BILL_AMT5 %in% boxplot.stats(Default_test$BILL_AMT5)$out, 1, 0)
Default_test$BILL_AMT5[Default_test$out_BIL5 == 1] <- NA
summary(Default_test$BILL_AMT5)

Default_test$out_BIL6<- ifelse(Default_test$BILL_AMT6 %in% boxplot.stats(Default_test$BILL_AMT6)$out, 1, 0)
Default_test$BILL_AMT6[Default_test$out_BIL6 == 1] <- NA
summary(Default_test$BILL_AMT6)

Default_test$out_BILTOTAL<- ifelse(Default_test$BILL_TOTAL %in% boxplot.stats(Default_test$BILL_TOTAL)$out, 1, 0)
Default_test$BILL_TOTAL[Default_test$out_BILTOTAL == 1] <- NA
summary(Default_test$BILL_TOTAL)

Default_test$out_PAY1<- ifelse(Default_test$PAY_AMT1 %in% boxplot.stats(Default_test$PAY_AMT1)$out, 1, 0)
Default_test$PAY_AMT1[Default_test$out_PAY1== 1] <- NA
summary(Default_test$PAY_AMT1)

Default_test$out_PAY2<- ifelse(Default_test$PAY_AMT2 %in% boxplot.stats(Default_test$PAY_AMT2)$out, 1, 0)
Default_test$PAY_AMT2[Default_test$out_PAY2== 1] <- NA
summary(Default_test$PAY_AMT2)

Default_test$out_PAY3<- ifelse(Default_test$PAY_AMT3 %in% boxplot.stats(Default_test$PAY_AMT3)$out, 1, 0)
Default_test$PAY_AMT3[Default_test$out_PAY3== 1] <- NA
summary(Default_test$PAY_AMT3)

Default_test$out_PAY4<- ifelse(Default_test$PAY_AMT4 %in% boxplot.stats(Default_test$PAY_AMT4)$out, 1, 0)
Default_test$PAY_AMT4[Default_test$out_PAY4== 1] <- NA
summary(Default_test$PAY_AMT4)

Default_test$out_PAY5<- ifelse(Default_test$PAY_AMT5 %in% boxplot.stats(Default_test$PAY_AMT5)$out, 1, 0)
Default_test$PAY_AMT5[Default_test$out_PAY5== 1] <- NA
summary(Default_test$PAY_AMT5)

Default_test$out_PAY6<- ifelse(Default_test$PAY_AMT6 %in% boxplot.stats(Default_test$PAY_AMT6)$out, 1, 0)
Default_test$PAY_AMT6[Default_test$out_PAY6== 1] <- NA
summary(Default_test$PAY_AMT6)

Default_test$out_PAYTOTAL<- ifelse(Default_test$PAY_TOTAL %in% boxplot.stats(Default_test$PAY_TOTAL)$out, 1, 0)
Default_test$PAY_TOTAL[Default_test$out_PAYTOTAL== 1] <- NA
summary(Default_test$PAY_TOTAL)

Default_test$out_AGE<- ifelse(Default_test$AGE %in% boxplot.stats(Default_test$AGE)$out, 1, 0)
Default_test$AGE[Default_test$out_AGE== 1] <- NA
summary(Default_test$AGE)


Default_test <- Default_test[,1:25]

Default_test$EDUCATION_1[Default_test$EDUCATION == 1] <- "Higher Ed"
Default_test$EDUCATION_1[Default_test$EDUCATION == 2] <- "Higher Ed"
Default_test$EDUCATION_1[Default_test$EDUCATION == 3] <- "Non Higher Ed"
Default_test$EDUCATION_1[Default_test$EDUCATION == 4] <- "Non Higher Ed"

IV_1 <- create_infotables(data=Default_test, y = "default", bins = 5)
IV_1

IV_2 <- create_infotables(data=Default, y = "default", bins = 5)
IV_2

Default_test$default <- ifelse(Default_test$default == 1, "Yes", "No")
Default_test$default <- as.factor(Default_test$default)




attach(Default_test)

Default_test$DELAY_1[Default_test$DELAY_1 > 0] <- "Yes"
Default_test$DELAY_1[Default_test$DELAY_1 == 0] <- "No"

Default_test$DELAY_2[Default_test$DELAY_2 > 0] <- "Yes"
Default_test$DELAY_2[Default_test$DELAY_2 == 0] <- "No"

Default_test$DELAY_3[Default_test$DELAY_3 > 0] <- "Yes"
Default_test$DELAY_3[Default_test$DELAY_3 == 0] <- "No"

Default_test$DELAY_4[Default_test$DELAY_4 > 0] <- "Yes"
Default_test$DELAY_4[Default_test$DELAY_4 == 0] <- "No"

Default_test$DELAY_5[Default_test$DELAY_5 > 0] <- "Yes"
Default_test$DELAY_5[Default_test$DELAY_5 == 0] <- "No"

Default_test$DELAY_6[Default_test$DELAY_6 > 0] <- "Yes"
Default_test$DELAY_6[Default_test$DELAY_6 == 0] <- "No"



sum(default == 1)
sum(default == 1)/nrow(Default)
sum(default == 1 & EDUCATION == 1)
sum(default == 1 & EDUCATION == 2)
sum(default == 1 & EDUCATION == 3)
sum(default == 1 & EDUCATION == 4)
sum(MARRIAGE == 2)


sum(default == 1 & Default$PAY_TOTAL > Default$BILL_TOTAL)
sum(default == 1 & Default$PAY_TOTAL <= Default$BILL_TOTAL)

str(Default)




#colnms_diff <- c("BILL_TOTAL","PAY_TOTAL")
#Default$DIFFERENCE <- rowDiffs(as.matrix(Default[,colnms_diff]))






cc_barplot(Default, "DELAY_1", "default", freq = "relfreq")
cc_barplot(Default, "DELAY_2", "default", freq = "relfreq")
cc_barplot(Default, "DELAY_3", "default", freq = "relfreq")
cc_barplot(Default, "DELAY_4", "default", freq = "relfreq")
cc_barplot(Default, "DELAY_5", "default", freq = "relfreq")
cc_barplot(Default, "DELAY_6", "default", freq = "relfreq")

cc_boxplot(Default_test, "LIMIT_BAL", "EDUCATION")

cc_barplot(Default_test, "DELAY_1", "default", freq = "relfreq")
cc_barplot(Default_test, "DELAY_1", "default", freq = "condprob")
cc_barplot(Default_test, "DELAY_2", "default", freq = "relfreq")
cc_barplot(Default_test, "DELAY_2", "default", freq = "condprob")
cc_barplot(Default_test, "DELAY_3", "default", freq = "relfreq")
cc_barplot(Default_test, "DELAY_3", "default", freq = "condprob")
cc_barplot(Default_test, "DELAY_4", "default", freq = "relfreq")
cc_barplot(Default_test, "DELAY_4", "default", freq = "condprob")
cc_barplot(Default_test, "DELAY_5", "default", freq = "relfreq")
cc_barplot(Default_test, "DELAY_5", "default", freq = "condprob")
cc_barplot(Default_test, "DELAY_6", "default", freq = "relfreq")
cc_barplot(Default_test, "DELAY_6", "default", freq = "condprob")


sum(default == 1)
sum(DELAY_1 > 0 & default == 1)
sum(DELAY_2 > 0 & default == 1)
sum(DELAY_3 > 0 & default == 1)
sum(DELAY_4 > 0 & default == 1)
sum(DELAY_5 > 0 & default == 1)
sum(DELAY_6 > 0 & default == 1)

plot(DELAY_2, PAY_AMT2, col = Default_test$default)

cc_hist(Default_test[!is.na(Default_test$LIMIT_BAL), ], "LIMIT_BAL", "default", breaks = 20)
cc_boxplot(Default_test[!is.na(Default_test$LIMIT_BAL), ], "LIMIT_BAL", "default")


sum(LIMIT_BAL <= 100000 & default == 1, na.rm = TRUE)/sum(default == 1)
sum(LIMIT_BAL <= 100000 & default == 1, na.rm = TRUE)/sum(LIMIT_BAL > 0, na.rm = TRUE)
sum(LIMIT_BAL <= 150000, na.rm = TRUE)/sum(LIMIT_BAL > 0, na.rm = TRUE)

sum(LIMIT_BAL > 150000 & default == 1, na.rm = TRUE)/sum(default == 1)
sum(LIMIT_BAL <= 100000, na.rm = TRUE)/sum(LIMIT_BAL >= 0, na.rm = TRUE)

mean(LIMIT_BAL, na.rm = TRUE)

cc_hist(Default, "PAY_AMT1", "default")
cc_hist(Default, "PAY_AMT2", "default")
cc_hist(Default, "PAY_AMT3", "default")
cc_hist(Default, "PAY_AMT4", "default")
cc_hist(Default, "PAY_AMT5", "default")
cc_hist(Default, "PAY_AMT6", "default")

cc_barplot(Default_test, "EDUCATION", "default", freq = "condprob")
cc_barplot(Default_test, "EDUCATION", "default", freq = "relfreq")

cc_barplot(Default_test, "EDUCATION_1", "default", freq = "condprob")
cc_barplot(Default_test, "EDUCATION_1", "default", freq = "relfreq")


cc_boxplot(Default_test, "LIMIT_BAL", "EDUCATION_1")
cc_boxplot(Default_test, "LIMIT_BAL", "EDUCATION")
cc_boxplot(Default_test, "LIMIT_BAL", "MARRIAGE")

cc_hist(Default_test[!is.na(Default_test$LIMIT_BAL), ], "LIMIT_BAL", "EDUCATION_1", breaks = 20)

cc_barplot(Default_test, "DELAY_1", "EDUCATION", freq = "relfreq")

sum(EDUCATION_1 == "Higher Ed" & default == "Yes")

sum(EDUCATION == 1 & default == "Yes")/nrow(Default)
sum(EDUCATION == 2 & default == "Yes")/nrow(Default)
sum(EDUCATION == 3)/nrow(Default)
sum(EDUCATION == 4)/nrow(Default)

sum(default == "Yes" & EDUCATION == 1)
sum(EDUCATION == 2 & default == "Yes")
sum(EDUCATION == 3 & default == "Yes")

cc_hist(Default_test[!is.na(Default_test$AGE), ], "AGE", "default")
cc_hist(Default_test[!is.na(Default_test$AGE), ], "AGE", "EDUCATION_1")
cc_hist(Default_test[!is.na(Default_test$AGE), ], "AGE", "MARRIAGE")

cc_boxplot(Default_test[!is.na(Default_test$AGE), ], "AGE", "MARRIAGE")
cc_boxplot(Default_test[!is.na(Default_test$AGE), ], "AGE", "EDUCATION")

plot(AGE, LIMIT_BAL)

sum(AGE < 30 & default == "Yes")
sum(AGE >= 25 & AGE <= 40 & default == "Yes", na.rm = TRUE)/sum(AGE >= 25 & AGE <= 40, na.rm = TRUE)
sum(AGE >= 40 & AGE < 50 & default == "Yes")

sum(AGE < 25 & default == "Yes", na.rm = TRUE)/sum(AGE<25, na.rm = TRUE)

summary(AGE)

hist(Default$PAY_AMT1)

cor(AGE, MARRIAGE)
cor(AGE, EDUCATION)
cor(AGE, !is.na(Default_test$LIMIT_BAL))
cor(!is.na(Default_test$AGE), as.numeric(default))

cc_hist(Default, "BILL_AMT1", "default")
cc_hist(Default, "BILL_AMT2", "default")
cc_hist(Default, "BILL_AMT3", "default")
cc_hist(Default, "BILL_AMT4", "default")
cc_hist(Default, "BILL_AMT5", "default")
cc_hist(Default, "BILL_AMT6", "default")

cc_barplot(Default_test, "MARRIAGE", "default", freq = "condprob")
cc_barplot(Default_test, "MARRIAGE", "default", freq = "relfreq")

cc_boxplot(Default_test, "LIMIT_BAL", "MARRIAGE")
cc_barplot(Default_test, "DELAY_1", "MARRIAGE", freq = "relfreq")

sum(MARRIAGE == 0)/nrow(Default)
sum(MARRIAGE == 1)/nrow(Default)
sum(MARRIAGE == 2)/nrow(Default)


plot(PAY_AMT1, BILL_AMT2, col = Default_test$default)
plot(PAY_TOTAL, BILL_TOTAL, col = Default_test$default)


plot(PAY_AMT2, BILL_AMT2, col = Default$default, xlim = c(0,50000))
plot(PAY_AMT2, BILL_AMT2, col = Default$default)

plot(PAY_AMT3, BILL_AMT3, col = Default$default, xlim = c(0,50000))
plot(PAY_AMT3, BILL_AMT3, col = Default$default)

plot(PAY_AMT4, BILL_AMT4, col = Default$default, xlim = c(0,50000))
plot(PAY_AMT4, BILL_AMT4, col = Default$default)

plot(PAY_AMT5, BILL_AMT5, col = Default$default, xlim = c(0,50000))
plot(PAY_AMT5, BILL_AMT5, col = Default$default)

plot(PAY_AMT6, BILL_AMT6, col = Default$default, xlim = c(0,50000))
plot(PAY_AMT6, BILL_AMT6, col = Default$default)

plot(PAY_TOTAL, BILL_TOTAL, col = Default_test$default)
legend("bottomright", legend = c("Not Defaulted", "Defaulted"), col = c("black", "red"), pch = 1)

plot(Default$PAY_TOTAL, Default$BILL_TOTAL, col = Default_test$default)
legend("bottomright", legend = c("Not Defaulted", "Defaulted"), col = c("black", "red"), pch = 1)


cc_hist(Default_test[!is.na(Default_test$PAY_AMT1), ], "PAY_AMT1", "default", breaks = 20)
cc_hist(Default_test[!is.na(Default_test$PAY_AMT2), ], "PAY_AMT2", "default", breaks = 20)

cc_hist(Default_test[!is.na(Default_test$BILL_AMT1), ], "BILL_AMT1", "default", breaks = 20)
cc_hist(Default_test[!is.na(Default_test$BILL_AMT2), ], "BILL_AMT2", "default", breaks = 20)

cc_hist(Default_test[!is.na(Default_test$PAY_TOTAL), ], "PAY_TOTAL", "default", breaks = 20)
cc_hist(Default_test[!is.na(Default_test$BILL_TOTAL), ], "BILL_TOTAL", "default", breaks = 20)


cor(!is.na(PAY_AMT1), !is.na(BILL_AMT2))
cor(!is.na(PAY_AMT2), !is.na(BILL_AMT3))
cor(!is.na(PAY_AMT3), !is.na(BILL_AMT4))
cor(!is.na(PAY_AMT4), !is.na(BILL_AMT5))
cor(!is.na(PAY_AMT5), !is.na(BILL_AMT6))
cor(!is.na(PAY_AMT6), !is.na(BILL_AMT6))
cor(!is.na(PAY_TOTAL), !is.na(BILL_TOTAL))


cor(Default$DELAY_1, Default$BILL_AMT2)
cor(Default$DELAY_1, Default$PAY_AMT2)

r <- cor(!is.na(Default_test[,c(11,12,13,14,15,16,17,18,19,20,21,22)]))
corrplot(r, method = 'square', type = 'lower')
         


sum(PAY_TOTAL <= 15000 & default == "Yes", na.rm = TRUE)/sum(default == "Yes")
sum(PAY_TOTAL <= 15000 & default == "No", na.rm = TRUE)/sum(default == "No")

sum(PAY_TOTAL <= 15000, na.rm = TRUE)
sum(PAY_AMT1 >= BILL_AMT2 & default == "Yes", na.rm = TRUE)
sum(PAY_AMT1 < BILL_AMT2 & default == "Yes", na.rm = TRUE)
sum(PAY_AMT1 >= BILL_AMT2 & default == "No", na.rm = TRUE)


#chisq test for categorical variables


T_d1 <- table(DELAY_1, default)
T_d1
chisq.test(T_d1)

T_d2 <- table(DELAY_2, default)
T_d2
chisq.test(T_d2)

T_d3 <- table(DELAY_3, default)
T_d3
chisq.test(T_d3)

T_d4 <- table(DELAY_4, default)
T_d4
chisq.test(T_d4)

T_d5 <- table(DELAY_5, default)
T_d5
chisq.test(T_d5)

T_d6 <- table(DELAY_6, default)
T_d6
chisq.test(T_d6)

T_ed <- table(EDUCATION, default)
T_ed
chisq.test(T_ed)


T_ed1 <- table(EDUCATION_1, default)
T_ed1
chisq.test(T_ed1)

T_mar <- table(MARRIAGE, default)
T_mar
chisq.test(T_mar)

#chisq test for continous variables

#chisq for LIMIT_BAL
v_bal <- seq(from=min(LIMIT_BAL, na.rm = TRUE)-1, to=max(LIMIT_BAL, na.rm = TRUE)+1, length.out = 6)
d.bal <- cut(LIMIT_BAL, v_bal)
T_bal <- table(d.bal, default)
T_bal

chisq.test(T_bal)

v_bal <- v_bal[-10]
d.bal <- cut(LIMIT_BAL, v_bal)
T_bal <- table(d.bal, default)
T_bal

v_bal <- v_bal[-9]
d.bal <- cut(LIMIT_BAL, v_bal)
T_bal <- table(d.bal, default)
T_bal

v_bal <- v_bal[-8]
d.bal <- cut(LIMIT_BAL, v_bal)
T_bal <- table(d.bal, default)
T_bal

chisq.test(T_bal)

#chisq for age

v_age <- seq(from=min(AGE, na.rm = TRUE)-1, to=max(AGE, na.rm = TRUE)+1, length.out = 10)
d.age <- cut(AGE, v_age)
T_age <- table(d.age, default)
T_age
v_age <- v_age[-9]
d.age <- cut(AGE, v_age)
T_age <- table(d.age, default)
T_age

chisq.test(T_age)

#chisq for PAY_AMT1

v_p1 <- seq(from=min(PAY_AMT1, na.rm = TRUE)-1, to=max(PAY_AMT1, na.rm = TRUE)+1, length.out = 7)
d.p1 <- cut(PAY_AMT1, v_p1)
T_p1 <- table(d.p1, default)
T_p1



chisq.test(T_p1)

v_p2 <- seq(from=min(PAY_AMT2, na.rm = TRUE)-1, to=max(PAY_AMT2, na.rm = TRUE)+1, length.out = 7)
d.p2 <- cut(PAY_AMT2, v_p2)
T_p2 <- table(d.p2, default)
T_p2

chisq.test(T_p2)


v_p3 <- seq(from=min(PAY_AMT3, na.rm = TRUE)-1, to=max(PAY_AMT3, na.rm = TRUE)+1, length.out = 7)
d.p3 <- cut(PAY_AMT3, v_p3)
T_p3 <- table(d.p3, default)
T_p3

chisq.test(T_p3)

v_p4 <- seq(from=min(PAY_AMT4, na.rm = TRUE)-1, to=max(PAY_AMT4, na.rm = TRUE)+1, length.out = 7)
d.p4 <- cut(PAY_AMT4, v_p4)
T_p4 <- table(d.p4, default)
T_p4

chisq.test(T_p4)

v_p5 <- seq(from=min(PAY_AMT5, na.rm = TRUE)-1, to=max(PAY_AMT5, na.rm = TRUE)+1, length.out = 7)
d.p5 <- cut(PAY_AMT5, v_p5)
T_p5 <- table(d.p5, default)
T_p5

chisq.test(T_p5)

v_p6 <- seq(from=min(PAY_AMT6, na.rm = TRUE)-1, to=max(PAY_AMT6, na.rm = TRUE)+1, length.out = 7)
d.p6 <- cut(PAY_AMT6, v_p6)
T_p6 <- table(d.p6, default)
T_p6

chisq.test(T_p6)

v_pt <- seq(from=min(PAY_TOTAL, na.rm = TRUE)-1, to=max(PAY_TOTAL, na.rm = TRUE)+1, length.out = 7)
d.pt <- cut(PAY_TOTAL, v_pt)
T_pt <- table(d.pt, default)
T_pt

chisq.test(T_pt)

cor(as.numeric(default), !is.na(PAY_AMT1))
cor(as.numeric(default), !is.na(PAY_AMT2))
cor(as.numeric(default), !is.na(PAY_AMT3))
cor(as.numeric(default), !is.na(PAY_AMT4))
cor(as.numeric(default), !is.na(PAY_AMT5))
cor(as.numeric(default), !is.na(PAY_AMT6))


v_b1 <- seq(from=min(BILL_AMT1, na.rm = TRUE)-1, to=max(BILL_AMT1, na.rm = TRUE)+1, length.out = 6)
d.b1 <- cut(BILL_AMT1, v_b1)
T_b1 <- table(d.b1, default)
T_b1

chisq.test(T_b1)

v_b2 <- seq(from=min(BILL_AMT2, na.rm = TRUE)-1, to=max(BILL_AMT2, na.rm = TRUE)+1, length.out = 6)
d.b2 <- cut(BILL_AMT2, v_b2)
T_b2 <- table(d.b2, default)
T_b2

chisq.test(T_b2)

v_b3 <- seq(from=min(BILL_AMT3, na.rm = TRUE)-1, to=max(BILL_AMT3, na.rm = TRUE)+1, length.out = 6)
d.b3 <- cut(BILL_AMT3, v_b3)
T_b3 <- table(d.b3, default)
T_b3

chisq.test(T_b3)

v_b4 <- seq(from=min(BILL_AMT4, na.rm = TRUE)-1, to=max(BILL_AMT4, na.rm = TRUE)+1, length.out = 6)
d.b4 <- cut(BILL_AMT4, v_b4)
T_b4 <- table(d.b4, default)
T_b4

chisq.test(T_b4)

v_b5 <- seq(from=min(BILL_AMT5, na.rm = TRUE)-1, to=max(BILL_AMT5, na.rm = TRUE)+1, length.out = 6)
d.b5 <- cut(BILL_AMT5, v_b5)
T_b5 <- table(d.b5, default)
T_b5

chisq.test(T_b5)

v_b6 <- seq(from=min(BILL_AMT6, na.rm = TRUE)-1, to=max(BILL_AMT6, na.rm = TRUE)+1, length.out = 6)
d.b6 <- cut(BILL_AMT6, v_b6)
T_b6 <- table(d.b6, default)
T_b6

chisq.test(T_b6)

v_bt <- seq(from=min(BILL_TOTAL, na.rm = TRUE)-1, to=max(BILL_TOTAL, na.rm = TRUE)+1, length.out = 7)
d.bt <- cut(BILL_TOTAL, v_bt)
T_bt <- table(d.bt, default)
T_bt

chisq.test(T_bt)