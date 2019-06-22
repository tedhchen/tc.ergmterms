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

InitErgmTerm.difftransties<-function (nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=TRUE,
                      varnames = c("attrname", "diff", "levels"),
                      vartypes = c("character", "logical", "character,numeric,logical"),
                      defaultvalues = list(NULL, FALSE, NULL),
                      required = c(FALSE, FALSE, FALSE))
  if (a$diff) stop("diff=TRUE is not currently implemented in difftransties")
  attrname <- a$attrname
  diff <- a$diff
  if(!is.null(attrname)) {
    nodecov <- get.node.attr(nw, attrname, "difftransties")
    u<-sort(unique(nodecov))
    if(any(is.na(nodecov))){u<-c(u,NA)}
    nodecov <- match(nodecov,u,nomatch=length(u)+1)
    ui <- seq(along=u)
    if (length(u)==1)
      warning ("Attribute given to difftransties() has only one value", call.=FALSE)
    if (!diff) {
      coef.names <- paste("difftransties",attrname,sep=".")
      inputs <- c(nodecov)
    } else {
      coef.names <- paste("difftransties",attrname, u, sep=".")
      inputs <- c(ui, nodecov)
      attr(inputs, "ParamsBeforeCov") <- length(ui)
    }
  }else{
    coef.names <- "difftransties"
    inputs <- NULL
  }
  list(name="difftransties", coef.names=coef.names, inputs=inputs, minval=0)
}

InitErgmTerm.edgecov.nodeattr <- function(nw, arglist, ...){
  a <- check.ErgmTerm(nw, arglist, 
                      varnames = c("x", "attrname", "node_attr", "value"),
                      vartypes = c("matrix,network", "character", "character", "character,numeric,logical"),
                      defaultvalues = list(NULL, NULL, NULL, NULL),
                      required = c(TRUE, FALSE, TRUE, TRUE))
  node_attr <- get.node.attr(nw, a$node_attr)
  node_attr <- ifelse(node_attr == a$value, 1, 0)
  
  if(is.network(a$x)){
    xm <- as.matrix.network(a$x,matrix.type="adjacency",a$attrname)
  } else {xm <- as.matrix(a$x)}
  
  ### Construct the list to return
  if(!is.null(a$attrname)) {
    # Note: the sys.call business grabs the name of the x object from the 
    # user's call.  Not elegant, but it works as long as the user doesn't
    # pass anything complicated.
    cn<-paste("edgecov.sender.attr", as.character(a$attrname), sep = ".")
  } else {
    cn<-paste("edgecov.sender.attr", as.character(sys.call(0)[[3]][2]), sep = ".")
  }
  
  inputs <- c(NCOL(xm), as.double(xm), node_attr)
  attr(inputs, "ParamsBeforeCov") <- 1
  list(name="edgecov_nodeattr", coef.names = cn, inputs = inputs, dependence=FALSE, pkgname = "tc.ergmterms",
       minval = sum(c(xm)[c(xm)<0 & c(matrix(node_attr, nrow = length(node_attr), ncol = length(node_attr), byrow = F)) == 1]),
       maxval = sum(c(xm)[c(xm)>0 & c(matrix(node_attr, nrow = length(node_attr), ncol = length(node_attr), byrow = F)) == 1])
  )
  
  
}















