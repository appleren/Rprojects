require(tools)
options(warn=2)

DmdFcstException<-setClass("DmdFcstException",
                   slots=c(message="character"))

handleError<-function(e) {
  c<-attr(e, "class")
  if(length(c)!=0 & ("error" %in% c | "simpleError" %in% c)){
    DmdFcstException(message=conditionMessage(e))
  }
}

# w<-tryCatch(warning("this is a warning"), error =handleError)
# e<-tryCatch(stop("this is an error"), error =handleError)


war<-function(){
  warning("2323")
  warning("lalal")
}