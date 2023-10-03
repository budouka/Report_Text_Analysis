# Function

segment_and_extract <- function(x,keywords,output) {
  stopifnot(output %in% c("corpus_segmented","window_segmented","window_merged"))
  if (class(x)[1] == "data.frame") {
    x <- corpus(x)
  }
  corpus_seg <- corpus_segment(x, pattern = '\n', valuetype = "glob",
                               pattern_position = "before", use_docvars = TRUE)
  corpus_seg$ntoken <- ntoken(corpus_seg)
  corpus_seg <- corpus_subset(corpus_seg, ntoken > 0)
  corpus_seg$filter <- str_detect(corpus_seg,
                                    regex(paste(keywords, collapse = "|"),ignore_case = TRUE))
  if (output == "corpus_segmented") {
    return(corpus_seg)
  } else {
    corpus_seg_trim <- corpus_subset(corpus_seg, filter == TRUE)
    if (output == "window_segmented") {
      return(corpus_seg_trim)
    } else {
      corpus_seg_trim$firmyear <- paste0(corpus_seg_trim$company,corpus_seg_trim$year)
      corpus_window <- corpus_group(corpus_seg_trim, groups = firmyear,concatenator = " ")
      if (output == "window_merged") {
        return(corpus_window)
      } else stop("invalid output setting")
    }
  }
}


# Example for use: segmenting, trimming and/or merging 4 example corporate reports
load("C:/Users/pthompson/Dropbox (Box.FU)/TTT/Portfolio/test_texts.Rdata")

test_window_seg <- segment_and_extract(test_texts,
                                       keywords = c("responsibility","Responsibility"),
                                       output = "window_segmented")

test_window_merge <- segment_and_extract(test_texts,
                                         keywords = c("responsibility","Responsibility"),
                                         output = "window_merged")

test_corpus_seg <- segment_and_extract(test_texts,
                                       keywords = c("responsibility","Responsibility"),
                                       output = "corpus_segmented")
