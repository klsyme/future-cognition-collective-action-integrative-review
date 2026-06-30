library(tidyverse) # dplyr # purr # stringr
library(readxl)
library(httr)
library(openxlsx)


# Enter API key
api_key <- "api goes here"

# This defines the function that talks to ChatGpt
hey_chatGPT <- function(prompt) {
  retries <- 0
  max_retries <- 3  # Set a maximum number of retries
  while (retries < max_retries) {
    tryCatch({
      chat_GPT_answer <- POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", api_key)),
        content_type_json(),
        encode = "json",
        body = list(
          model = "gpt-4-1106-preview",
          temperature = 0,
          messages = list(
            list(role = "system", content = "analyst"),
            list(role = "user", content = prompt)
          )
        )
      )
      
      if (status_code(chat_GPT_answer) != 200) {
        print(paste("API request failed with status", status_code(chat_GPT_answer)))
        retries <- retries + 1
        Sys.sleep(1)  # Wait a second before retrying
      } else {
        result <- content(chat_GPT_answer)$choices[[1]]$message$content
        if (nchar(result) > 0) {
          return(str_trim(result))
        } else {
          print("Received empty result, retrying...")
          retries <- retries + 1
          Sys.sleep(1)  # Wait a second before retrying
        }
      }
    }, error = function(e) {
      print(paste("Error occurred:", e))
      retries <- retries + 1
      Sys.sleep(1)  # Wait a second before retrying 
    })
  }
  return(NA)  # Return NA if all retries failed
}



