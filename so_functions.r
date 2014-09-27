# ---------------------------------------------------------------------------------------- #
#                                                                                          #
#                        Four functions here:                                              #
#                        -------------------                                               #
#                                                                                          #
#     stackQuestion    --  Opens a webbrowser, pointed to questionURL.                     #
#                          Default is new question                                         #
#                                                                                          #
#     getWebbrowserApp --  support function to stackQuestion(), returns the full path      #
#                          to the web browser application given a descriptive name         #
#                                                                                          #
#     .sorepro         --  copies to clipboard a simple (yet often repeated) reminder      #
#                          for new users on SO, on how to make a reproducible example      #
#                                                                                          #
#     .so              --  based on function written by @flodel, this will combine         #
#                          code + output into a cleanly formatted text.                    #
#                                                                                          #
#                          the major modification from @flodels code is that this          #
#                          current iteration can take raw text, or even clipboard text     #
#                                                                                          #
#                                                                                          #
# ---------------------------------------------------------------------------------------- #



## Set default web browser. 
options(webbrowser.default="chrome")

getWebbrowserApp <- function(app.name, applications.folder="/Applications/") {
  if (!is.character(app.name))
    stop ("'app.name' must be a character")

  browsers <- setNames(nm=c("chrome", "safari", "firefox", "internet explorer"), obj=paste0(applications.folder, c("Google Chrome.app", "Safari.app", "Firefox.app", "Internet Explorer.app")))
  b.matched <- pmatch(gsub("google\\s*", "", tolower(app.name)), names(browsers))

  if (is.na(b.matched)) {
    warning("'", app.name, "' did not match any known browsers. Valid options are: \n\t", paste0("'", names(browsers), "'", collapse=", "))
    return(NA)
  }

  if(names(browsers)[b.matched] == "internet explorer") 
    warning("Unrecoverable erorr:  Your browser is internet explorer.")

  return(browsers[b.matched])
}

stackQuestion <- function(questionURL="http://stackoverflow.com/questions/ask", browser=c("chrome", "safari", "firefox", "internet explorer")) {

  if (missing(browser))
    browser <- getOption("webbrowser.default", browser[[1]])

  browser.app <- getWebbrowserApp(browser)

  command.mac <- sprintf("/usr/bin/open -a '%s' '%s'", browser.app, questionURL)

  ## TODO:  
  ## The goal is to open the page, with text pasted into the text box located here 
  ##    <textarea id="wmd-input" class="wmd-input processed" name="post-text" cols="92" rows="15" tabindex="101" data-min-length=""></textarea>

  system(command.mac)
}


.so <- function(script.file=NULL, text=clipPaste(), copy.to.clipboard=TRUE) {
## original function by @flodel 

   # run the code and store the output in a character vector
   tmp <- tempfile()
   if (is.null(script.file)) {
      script.file <- tempfile(pat="source_", fileext=".r")
      con.sf <- file(script.file, open="w")
      # writeLines(text, con=script.file, sep="\n")
      writeLines(text, con=con.sf, sep="\n")
      close(con.sf)
   }

   capture.output(
          source(script.file
              , echo = TRUE
              , prompt.echo =  "> " # getOption("prompt")
              , continue.echo =  "+ " # getOption("continue")
              , max.deparse.length = 10000
          )
      , file = tmp)

   out <- readLines(tmp)

   # identify lines that are comments, code, results
   idx.comments <- grep("^> [#]{2}", out)
   idx.code     <- grep("^[>+] ", out)
   idx.blank    <- grep("^[[:space:]]*$", out)
   idx.results  <- setdiff(seq_along(out),
                           c(idx.comments, idx.code, idx.blank))
   # reformat
   out[idx.comments] <- sub("^> [#]{2} ", "", out[idx.comments])
   out[idx.code]     <- sub("^[>+] ", "    ", out[idx.code])
   out[idx.results]  <- sub("^", "    # ", out[idx.results])

  if (copy.to.clipboard)
    clipCopy(out)

  ## Check for stackoverflow URL at first non-blank line.  
  first_nonblank <- 1 + if (length(idx.blank)) min(idx.blank) else 0
  web.address <- gsub("^(\\s*(#)*)\\s*|\\s*$", "", out[first_nonblank])
  ## If the line matches a stackoverflow address, remove it from answer. 
  ## Otherwise, set web.address to NULL (which will be tested against next)
  if (grepl("^(http://)?(www.)?stackoverflow", web.address))
    out <- out[-first_nonblank]
  else 
    web.address  <- NULL

  if (!is.null(web.address)) 
    stackQuestion(web.address)

   # output
   cat(out, sep = "\n", file = stdout())
}



.sorepro <- function() {
  ## copies the bitly link to reproduce.r
  link <- "http://bit.ly/SORepro"
  out <- paste0("Hello and welcome to SO.  To help make a reproducible example, you can use   `reproduce(<your data>)` . Instructions are here: ", link, "   -  [How to make a great R reproducible example](", link, ")")
  cat("\n", out, "\n\n")
  clipCopy(out)
  return(link)
}




## Additional clipboard functions.  These are macosx specific and need to be modified for other platforms
if (!exists("clipPaste")) 
    clipPaste <- function(flat=TRUE)  {
      
      con <- pipe("pbpaste", open="rb")
      ret <- readLines(con, warn=FALSE)

      if (flat)
        ret <- paste0(ret, collapse="\n")

      close(con)
      return(ret)
    }


if (!exists("clipCopy")) 
    clipCopy <- function(txt, sep="", undo.save=TRUE) { 
    # equivalent of highlighting txt and hitting  CMD+C 

      if (!is.character(txt))
        txt <- capture.output(txt)

      txt <- paste(txt, collapse="\n")

      if (undo.save && exists(".undo.bank"))
        .undo.bank()

      # Currently, this works only Mac OSX
      if(Sys.info()[['sysname']] != "Darwin")
        return(txt)

      con <- pipe("pbcopy", "w")
      writeLines(txt, con, sep=sep)
      close(con)

      return(invisible(txt))
    }
