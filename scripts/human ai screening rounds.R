library(irr)
library(irrCAC)
library(writexl)

d <- all_rounds

# Round 1

# Percent agreement
agree_t1 <- agree(data.frame(d$ks_incl, d$t1_incl_gpt))
print(agree_t1)

# Cohen's kappa
ratings <- data.frame(d$ks_incl, d$t1_incl_gpt)
kappa_result <- kappa2(ratings, weight = "unweighted")
print(kappa_result)

# Gwet's AC1
ac1_result <- gwet.ac1.raw(ratings)
print(ac1_result)

# Confusion matrix
cm <- table(d$ks_incl, d$t1_incl_gpt)
print(cm)


# Extract values
TN <- cm["0","0"]
FP <- cm["0","1"]
FN <- cm["1","0"]
TP <- cm["1","1"]

# Precision, Recall, F1
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * (precision * recall) / (precision + recall)

precision
recall
f1


# Round 2

# Percent agreement
agree_t2 <- agree(data.frame(d$ks_incl, d$t3_incl_gpt))
print(agree_t2)

# Cohen's kappa
ratings2 <- data.frame(d$ks_incl, d$t3_incl_gpt)
kappa_result2 <- kappa2(ratings2, weight = "unweighted")
print(kappa_result2)

# Gwet's AC1
ac1_result2 <- gwet.ac1.raw(ratings2)
print(ac1_result2)


# Confusion matrix
cm2 <- table(d$ks_incl, d$t3_incl_gpt)
print(cm2)

# Extract values
TN2 <- cm2["0","0"]
FP2 <- cm2["0","1"]
FN2 <- cm2["1","0"]
TP2 <- cm2["1","1"]

# Precision, Recall, F1
precision2 <- TP2 / (TP2 + FP2)
recall2 <- TP2 / (TP2 + FN2)
f1_2 <- 2 * (precision2 * recall2) / (precision2 + recall2)

precision2
recall2
f1_2


# Reliability with itself

agree_t3 <- agree(data.frame(d$t1_incl_gpt, d$t3_incl_gpt))
print(agree_t3)

# Cohen's kappa
ratings3 <- data.frame(d$t1_incl_gpt, d$t3_incl_gpt)
kappa_result3 <- kappa2(ratings3, weight = "unweighted")
print(kappa_result3)

# Gwet's AC1
ac1_result3 <- gwet.ac1.raw(ratings3)
print(ac1_result3)

# Confusion matrix
cm3 <- table(d$t1_incl_gpt, d$t3_incl_gpt)
print(cm3)

# Extract values
TN3 <- cm3["0","0"]
FP3 <- cm3["0","1"]
FN3 <- cm3["1","0"]
TP3 <- cm3["1","1"]

# Precision, Recall, F1
precision3 <- TP3 / (TP3 + FP3)
recall3 <- TP3 / (TP3 + FN3)
f1_3 <- 2 * (precision3 * recall3) / (precision3 + recall3)

precision3
recall3
f1_3
