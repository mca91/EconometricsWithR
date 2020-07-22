script <- "<script async='async' src='https://cdn.datacamp.com/dcl-react.js.gz'></script>"

inject_document_head <- function(script, css_file = "", workdir = "/URFITE Bookdown/DCL") {
  # set directory to look for files
  setwd(workdir)
  # read css file
  css <- c("<style>", readLines(css_file, warn = F), "</style>")
  # generate list of html files in working directory 
  files <- list.files(full.names = T, pattern = "*.html")
  # run lapply over html files:
  lapply(files, function(f) {
    # read file
    doc <- readLines(f)
    # indices of <head> and </head>
    oh <- grep('<head>', doc)
    ch <- grep('</head>', doc)
    # delete code in header
    doc <- doc[-((oh+1):(ch-1))]
    # update indices of <head> and </head>
    oh <- grep('<head>', doc)
    ch <- grep('</head>', doc)
    # inject code
    doc <- paste(
      c(doc[1:oh], 
        "<meta charset='utf-8'/>", 
        script, 
        css,
        doc[ch:length(doc)]), 
        collapse = "\n") 
    # write to file
    cat(doc, file = f) 
  })
}

# run function
inject_document_head(script, css_file = "exercise.css")






