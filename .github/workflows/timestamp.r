# Install required packages
install.packages(c("httr", "xml2"))

# Load required libraries
library(httr)
library(xml2)

# Make a GET request to the BBC UK news RSS feed
url <- "http://feeds.bbci.co.uk/news/rss.xml?edition=uk"
response <- GET(url)

# Check if the request is successful (status code 200)
if (http_status(response)$category == "Success") {
#   Parse the XML content
  content <- content(response, as = "text")
  xml_data <- read_xml(content)
} else {
  cat("Failed to fetch the RSS feed.")
}

# Install required packages
install.packages(c("tidyverse", "tidytext"))

# Load required libraries
library(tidyverse)
library(tidytext)

# Extract titles and descriptions
titles <- xml_data %>% xml_find_all("//item/title") %>% xml_text()
descriptions <- xml_data %>% xml_find_all("//item/description") %>% xml_text()

# Sentiment analysis function
sentiment_analysis <- function(text) {
  text %>%
    tibble(text = .) %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(word) %>%
    summarize(sentiment_score = sum(value))
}

# Perform sentiment analysis
titles_sentiment <- sentiment_analysis(titles)
descriptions_sentiment <- sentiment_analysis(descriptions)

# Compute summary statistics
titles_summary <- data.frame(summary(titles_sentiment))
descriptions_summary <- data.frame(summary(descriptions_sentiment))

# Save the summary statistics to a CSV file
write_csv(titles_summary, "titles_summary.csv")
write_csv(descriptions_summary, "descriptions_summary.csv")

# Commit the files to the repository
system("git add titles_summary.csv descriptions_summary.csv")
system("git commit -m 'Update summary statistics'")
system("git push")
