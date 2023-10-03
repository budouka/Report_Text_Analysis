# Define helper functions to read in raw source code, strip away html code, turn into corpus object
library(xml2)
library(quanteda)
library(readtext)

unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

read_and_strip <- function(server_path,start_key,end_key,output_format) {
  if (output_format == "corpus" | output_format == "data.frame") {
    source_code <- readtext(server_path,
                            docvarsfrom = "filenames",
                            docvarnames = c("ID"),
                            encoding = "UTF-8",
                            verbosity = 3)
    source_code_clean <- source_code
    source_code_clean$text <- gsub(paste0(".*",start_key),"",source_code_clean$text)
    source_code_clean$text <- gsub(paste0(end_key,".*"),"",source_code_clean$text)
    source_code_clean$text <- gsub('<table class=.*?</table>', '', source_code_clean$text)
    source_code_clean$text <- gsub('</p>', ' NEWLINEPLACEHOLDER ', source_code_clean$text)
    source_code_clean$text <- gsub('<h', ' NEWLINEPLACEHOLDER -HEADER-START- <', source_code_clean$text)
    source_code_clean$text <- gsub('</h', ' -HEADER-END- <', source_code_clean$text)
    source_code_clean$text <- gsub('<.*?>', '', source_code_clean$text)
    source_code_clean$text <- str_squish(source_code_clean$text)
    source_code_clean$text <- gsub('NEWLINEPLACEHOLDER', '\n', source_code_clean$text)
    source_code_clean$text <- sapply(source_code_clean$text, function(x) unescape_html(x))
    text_corpus <- corpus(source_code_clean)
  
    if (output_format == "corpus") {
      return(text_corpus)
      } else if (output_format == "data.frame") {
        text_df <- convert(text_corpus, to = "data.frame")
        return(text_df) 
      }
    } else {
      print("invalid output setting")
    }
  }


# Example for use: read, strip and convert 4 corporate reports into a data.frame
server_path <- "C:/Users/pthompson/Dropbox (Box.FU)/TTT/Other/reports_for_testing"

start_key <- "<!-- Publication  -->"
end_key <- "<!-- Result Pager Bottom  -->"

test_texts <- read_and_strip(server_path,start_key, end_key, output_format = "data.frame")
