# Custom Terms used for the US Supreme Court Citation Network
# See: https://github.com/desmarais-lab/Supreme_Court_Citation_Network

InitErgmTerm.wttriple<-function (nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=TRUE,
                      varnames = c("alpha"),
                      vartypes = c("numeric"),
                      defaultvalues = list(1),
                      required = c(TRUE))
  alpha <- a$alpha
  coef.names <- "wttriple"
  inputs <- c(alpha)
  list(name="wttriple", 
       coef.names=coef.names, 
       inputs=inputs, 
       minval = 0, 
       pkgname = "tc.ergmterms")
}

InitErgmTerm.wttriple2<-function (nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=TRUE,
                      varnames = c("attrname", "diff", "levels", "alpha"),
                      vartypes = c("character", "logical", "character,numeric,logical", "numeric"),
                      defaultvalues = list(NULL, FALSE, NULL, 1),
                      required = c(FALSE, FALSE, FALSE, FALSE))
  attrname <- a$attrname
  diff <- a$diff
  alpha <- a$alpha
  tn <- summary_formula(nw~ttriple)
  if(!is.null(attrname)) {
    nodecov <- get.node.attr(nw, attrname, "wttriple2")
    u <- NVL(a$levels, sort(unique(nodecov)))
    if(any(is.na(nodecov))){u<-c(u,NA)}
    nodecov <- match(nodecov,u,nomatch=length(u)+1)
    ui <- seq(along=u)
    if (length(u)==1)
      stop ("Attribute given to wttriple2() has only one value", call.=FALSE)
    if (!diff) {
      coef.names <- paste("wttriple2",attrname,sep=".")
      inputs <- c(nodecov)
    } else {
      coef.names <- paste("wttriple2",attrname, u, sep=".")
      inputs <- c(ui, nodecov)
      attr(inputs, "ParamsBeforeCov") <- length(ui)
    }
  }else{
    coef.names <- "wttriple2"
    inputs <- c(alpha, tn)
  }
  list(name="wttriple2", coef.names=coef.names, inputs=inputs, minval = 0, pkgname = "tc.ergmterms")
}