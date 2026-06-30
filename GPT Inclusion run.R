source("GPT Run.R")
library(irr)
library(irrCAC)
library(writexl)

## This creates a data frame where each row has one paragraph
d <- all


# Prompt for t_incl.gpt 
prompt <- 'I will provide the title and abstract from a scholarly article. Your task is to assess whether the article meets all of the following three criteria: First, if the abstract concerns an empirical study, it must measure or manipulate cognitions, perceptions, or narratives about the future, such as future thinking, future orientation, anticipated outcomes, psychological temporal distance, or future discounting. If the abstract concerns, theoretical or philosophical approaches it must explicitly address these constructs. Please exclude papers focused on foresight methods, scenario planning, or anticipatory governance, and studies proposing specific policy/action plans. Secondly, it addresses societal challenges requiring cooperation. This may include real-world challenges, such as climate change, resource conservation, vaccination and herd immunity, and controlling the spread of infectious disease. It may also focus on conceptual analyses of coordination dilemmas, societal risks, or other issues requiring sustained collective cooperation over time. Third, it involves prosocial decision-making, even if motivated by self-interest, including behaviors, intentions, moral reasoning, or strategies that support the collective good—often involving trade-offs or personal costs, such as inconvenience or delayed gratification. Fourth, for empirical papers, the study directly measures or tests attitudes (e.g., support for climate policies), intentions (e.g., willingness to reduce energy use), or behaviors (e.g., self-reported or observed mitigation actions). For theoretical or philosophical papers, it must conceptually address all of these dimensions. If the abstract meets all four of these criteria, annotate it with a 1. If any of the three criteria are not met, annotate it with a 0. Do not include any other information besides the 1 or the 0 in your response.'

# Generate t_incl.gpt
# Combine prompt with both Title and Abstract
# d$t_incl.gpt <- map_chr(paste(prompt, "Title:", d$Title, "Abstract:", d$Abstract), hey_chatGPT)

# Percent agreement
agree_t1 <- agree(data.frame(d$ks_incl, d$t_incl.gpt))
print(agree_t1)

# Confusion matrix
table(d$ks_incl, d$t_incl.gp)

# Cohen's kappa
ratings <- data.frame(d$ks_incl, d$t_incl.gpt)
kappa_result <- kappa2(ratings, weight = "unweighted")
print(kappa_result)

# Gwet's AC1
ac1_result <- gwet.ac1.raw(ratings)
print(ac1_result)

# GPT precision <- 61 / (61 + 53)
# [1] 0.5351 or 53.5%

# GPT recall <- 61 / (61 + 76)
# [1] 0.4453 or 44.5%

# GPT f1 <- 2 * (precision * recall) / (precision + recall)
# [1] 0.4857 or 48.6%


# write_xlsx(d, "C:/Users/kls52/OneDrive - University of Leicester/Existential Threats Syme-Krockow/Integrative Review R Project/round 1.xlsx")

# Disagreements
# Get rows where ks_incl and t_incl.gpt disagree
disagree_d <- d[d$ks_incl != d$t_incl.gpt, ]

# write_xlsx(disagree_d, "C:/Users/kls52/OneDrive - University of Leicester/Existential Threats Syme-Krockow/Integrative Review R Project/round 1 disagreements.xlsx")

