## THESE ARE A COLLECTION OF INTROSPECTIONS
## ADD TO THIS LIST

## ALSO, CMT(.) SHOULD BE HERE


intr <- introspect <- function(obj) {
  ret <- list()
  ret[["i.have"]]<- whatHaveI(obj)
  ret[["i.am"]] <- whatAmI(obj)
  ret[["CMT"]] <- CMT(obj)

  ret
}

#----------------------

whatAmI <- function(obj, all=FALSE) {

iam <- c(
        list= is.list(obj), 
        vector= is.vector(obj), 
        matrix= is.matrix(obj), 
        array= is.array(obj), 
        data.frame= is.data.frame(obj), 
        integer= is.integer(obj), 
        numeric= is.numeric(obj), 
        character= is.character(obj), 
        object= is.object(obj), 

        formula= is.formula(obj), 
        call= is.call(obj), 
        language= is.language(obj), 
        symbol= is.symbol(obj), 
        'function'= is.function(obj), 

        NULL)

if (!all)
  iam <- names(iam[which(iam)])

return(iam)

}

#----------------------
whatHaveI <- function(obj, all=FALSE) {

  ihave <- c(

    length = !is.null(length(obj)),
    dim = !is.null(dim(obj)),
    names= !is.null(names(obj)),
    colnames= !is.null(colnames(obj)),
    rownames= !is.null(rownames(obj)),

        NULL)

if (!all)
  ihave <- names(ihave[which(ihave)])

return(ihave)

}



