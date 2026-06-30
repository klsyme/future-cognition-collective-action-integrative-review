source("GPT Run.R")
library(writexl)
library(irr)

## This creates a data frame where each row has one paragraph
d <- Empirical_Only_for_Similarity_Score

# Prompt for similarity_score (Likert 5) rating only
prompt <- 'Please rate the similarity of these two texts on a scale from 1-5 where 1 is highly dissimilar and 5 is highly similar. Please do not provide any information except the rating:'

# Generate similarity_score.gpt
# Combine prompt with both Key Results and Key Results 2
# d$similarity_score.gpt <- pmap_chr(
  list(d$Results, d$Results2),
  function(r1, r2) {
    full_prompt <- paste0(
      prompt,
      "\nResults: ", r1,
      "\nResults2: ", r2
    )
    hey_chatGPT(full_prompt)
  }
)

# write_xlsx(d, "C:/Users/kls52/OneDrive - University of Leicester/Existential Threats Syme-Krockow/Integrative Review R Project/Analyses Data/Round 1 similarity score.xlsx")


# Re-Test: Generate similarity_score2.gpt
d$similarity_score2.gpt <- pmap_chr(
  list(d$Results, d$Results2),
  function(r1, r2) {
    full_prompt <- paste0(
      prompt,
      "\nResults: ", r1,
      "\nResults2: ", r2
    )
    hey_chatGPT(full_prompt)
  }
)

# write_xlsx(d, "C:/Users/kls52/OneDrive - University of Leicester/Existential Threats Syme-Krockow/Integrative Review R Project/Analyses Data/Round 1 & 2 similarity score.xlsx")


d <- Round_1_2_similarity_score

# Likert scale 5- Extract the relevant columns for analysis
ratings <- data.frame(d$similarity_score.gpt, d$similarity_score2.gpt)

# 1. Percent Agreement 
percent_agreement <- agree(ratings)
print(percent_agreement)

# 2. Cohen's Kappa (use weighted if Likert-type/ordinal)
kappa_result <- kappa2(ratings, weight = "unweighted")  # Use "equal" or "squared" if ordinal
print(kappa_result)

d$similarity_score.gpt <- as.numeric(d$similarity_score.gpt)
mean(d$similarity_score.gpt, na.rm = TRUE)
d$similarity_score2.gpt <- as.numeric(d$similarity_score2.gpt)
mean(d$similarity_score2.gpt, na.rm = TRUE)
