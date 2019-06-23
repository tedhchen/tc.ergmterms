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
  list(name="difftransties", coef.names=coef.names, inputs=inputs, minval=0, pkgname = "tc.ergmterms")
}

InitErgmTerm.edgecov_senderattr <- function(nw, arglist, ...){
  a <- check.ErgmTerm(nw, arglist, directed=TRUE,
                      varnames = c("x", "attrname", "sender_attr", "value"),
                      vartypes = c("matrix,network", "character", "character", "character,numeric,logical"),
                      defaultvalues = list(NULL, NULL, NULL, NULL),
                      required = c(TRUE, FALSE, TRUE, TRUE))
  sender_attr <- get.node.attr(nw, a$sender_attr)
  sender_attr <- ifelse(sender_attr == a$value, 1, 0)
  
  if(is.network(a$x)){
    xm <- as.matrix.network(a$x,matrix.type="adjacency",a$attrname)
  } else {xm <- as.matrix(a$x)}
  
  ### Construct the list to return
  if(!is.null(a$attrname)) {
    # Note: the sys.call business grabs the name of the x object from the 
    # user's call.  Not elegant, but it works as long as the user doesn't
    # pass anything complicated.
    cn<-paste("edgecov_senderattr", as.character(a$attrname), sep = ".")
  } else {
    cn<-paste("edgecov_senderattr", as.character(sys.call(0)[[3]][2]), sep = ".")
  }
  
  inputs <- c(NCOL(xm), as.double(xm), sender_attr)
  attr(inputs, "ParamsBeforeCov") <- 1
  list(name="edgecov_senderattr", coef.names = cn, inputs = inputs, dependence=FALSE, pkgname = "tc.ergmterms",
       minval = sum(c(xm)[c(xm)<0 & c(matrix(sender_attr, nrow = length(sender_attr), ncol = length(sender_attr), byrow = F)) == 1]),
       maxval = sum(c(xm)[c(xm)>0 & c(matrix(sender_attr, nrow = length(sender_attr), ncol = length(sender_attr), byrow = F)) == 1])
  )
}

InitErgmTerm.istar_senderattr <- function(nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=TRUE,
                      varnames = c("k", "attr", "levels", "sender_attr", "value"),
                      vartypes = c("numeric", ERGM_VATTR_SPEC, ERGM_LEVELS_SPEC, "character", "character,numeric,logical"),
                      defaultvalues = list(NULL, NULL, NULL, NULL, NULL),
                      required = c(TRUE, FALSE, FALSE, TRUE, TRUE))
  attrarg <- a$attr
  levels <- a$levels    
  
  sender_attr <- get.node.attr(nw, a$sender_attr)
  sender_attr <- ifelse(sender_attr == a$value, 1, 0)
  
  k <- a$k
  if(!is.null(attrarg)) {
    nodecov <- ergm_get_vattr(attrarg, nw)
    attrname <- attr(nodecov, "name")
    u <- ergm_attr_levels(levels, nodecov, nw, levels = sort(unique(nodecov)))
    if(any(is.na(nodecov))){u<-c(u,NA)}
    #     Recode to numeric if necessary
    nodecov <- match(nodecov,u,nomatch=length(u)+1)
  }else{
  }
  lk<-length(k)
  if(lk==0){return(NULL)}
  if(!is.null(attrarg)){
    coef.names <- paste("istar",k,".",attrname, "_senderattr",sep="")
    inputs <- c(k, nodecov, sender_attr)
    attr(inputs, "ParamsBeforeCov") <- lk
  }else{
    coef.names <- paste("istar",k, "_senderattr",sep="")
    inputs <- c(k, sender_attr)
  }
  list(name="istar_senderattr", coef.names=coef.names, inputs=inputs, minval = 0, conflicts.constraints="idegreedist", pkgname = "tc.ergmterms")
}


InitErgmTerm.ostar_senderattr <- function(nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=TRUE,
                      varnames = c("k", "attr", "levels", "sender_attr", "value"),
                      vartypes = c("numeric", ERGM_VATTR_SPEC, ERGM_LEVELS_SPEC, "character", "character,numeric,logical"),
                      defaultvalues = list(NULL, NULL, NULL, NULL, NULL),
                      required = c(TRUE, FALSE, FALSE, TRUE, TRUE))
  attrarg <- a$attr
  levels <- a$levels    
  
  sender_attr <- get.node.attr(nw, a$sender_attr)
  sender_attr <- ifelse(sender_attr == a$value, 1, 0)
  
  k<-a$k
  if(!is.null(attrarg)) {
    nodecov <- ergm_get_vattr(attrarg, nw)
    attrname <- attr(nodecov, "name")
    u <- ergm_attr_levels(levels, nodecov, nw, levels = sort(unique(nodecov)))
    if(any(is.na(nodecov))){u<-c(u,NA)}
    # Recode to numeric
    nodecov <- match(nodecov,u,nomatch=length(u)+1)
  }
  lk<-length(k)
  if(lk==0){return(NULL)}
  
  if(!is.null(attrarg)){
    coef.names <- paste("ostar",k,".",attrname, "_senderattr",sep="")
    inputs <- c(k, nodecov, sender_attr)
    attr(inputs, "ParamsBeforeCov") <- lk
  }else{
    coef.names <- paste("ostar",k, "_senderattr",sep="")
    inputs <- c(k, sender_attr)
  }
  list(name="ostar_senderattr", coef.names=coef.names, inputs=inputs, minval=0, conflicts.constraints="odegreedist", pkgname = "tc.ergmterms")  
}

InitErgmTerm.mutual_senderattr <- function (nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=TRUE, bipartite=NULL,
                      varnames = c("sender_attr", "value"),
                      vartypes = c("character", "character,numeric,logical"),
                      defaultvalues = list(NULL, NULL),
                      required = c(FALSE, FALSE))
  
  sender_attr <- get.node.attr(nw, a$sender_attr)
  sender_attr <- ifelse(sender_attr == a$value, 1, 0)
  
  ### Construct the list to return
  list(name="mutual_senderattr",                      #name: required
       coef.names = "mutual_senderattr",        #coef.names: required
       inputs=sender_attr,
       minval = 0,
       pkgname = "tc.ergmterms") 
}

InitErgmTerm.gwesp_senderattr <- function(nw, arglist, ...) {
  # the following line was commented out in <InitErgm.gwesp>:
  #   ergm.checkdirected("gwesp", is.directed(nw), requirement=FALSE)
  # so, I've not passed 'directed=FALSE' to <check.ErgmTerm>  
  a <- check.ErgmTerm(nw, arglist, directed = T,
                      varnames = c("decay","sender_attr", "value"),
                      vartypes = c("numeric","character", "character,numeric,logical"),
                      defaultvalues = list(0,NULL, NULL),
                      required = c(FALSE,TRUE, TRUE))
  
  sender_attr <- get.node.attr(nw, a$sender_attr)
  sender_attr <- ifelse(sender_attr == a$value, 1, 0)
  
  decay<-a$decay
  list(name="gwtesp_senderattr", coef.names=paste("gwesp.fixed.",decay,"_senderattr",sep=""), inputs=c(decay, sender_attr), pkgname = "tc.ergmterms")
}

InitErgmTerm.difftransties_senderattr <- function (nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=TRUE,
                      varnames = c("attrname", "diff", "levels", "sender_attr", "value"),
                      vartypes = c("character", "logical", "character,numeric,logical","character", "character,numeric,logical"),
                      defaultvalues = list(NULL, FALSE, NULL, NULL, NULL),
                      required = c(FALSE, FALSE, FALSE, TRUE, TRUE))
  if (a$diff) stop("diff=TRUE is not currently implemented in difftransties")
  attrname <- a$attrname
  diff <- a$diff
  
  sender_attr <- get.node.attr(nw, a$sender_attr)
  sender_attr <- ifelse(sender_attr == a$value, 1, 0)
  
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
  list(name="difftransties_senderattr", coef.names=paste(coef.names, "_senderattr", sep = ""), 
       inputs=c(inputs, sender_attr), minval=0, pkgname = "tc.ergmterms")
}

InitErgmTerm.nodeicov_senderattr <- function (nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm(nw, arglist, directed=TRUE,
                      varnames = c("attr", "sender_attr", "value"),
                      vartypes = c(ERGM_VATTR_SPEC, "character", "character,numeric,logical"),
                      defaultvalues = list(NULL, NULL, NULL),
                      required = c(TRUE, TRUE, TRUE))
  ### Process the arguments
  sender_attr <- get.node.attr(nw, a$sender_attr)
  sender_attr <- ifelse(sender_attr == a$value, 1, 0)
  
  nodecov <- ergm_get_vattr(a$attr, nw, accept="numeric", multiple="matrix")
  coef.names <- paste("nodeicov",attr(nodecov, "name"),sep=".")
  if(is.matrix(nodecov)) coef.names <- paste(coef.names, NVL(colnames(nodecov), seq_len(ncol(nodecov))), sep=".")
  
  list(name="nodeicov_senderattr", coef.names=paste(coef.names, "_senderattr", sep = ""), inputs=c(nodecov, sender_attr), 
       dependence=FALSE, pkgname = "tc.ergmterms")
}

InitErgmTerm.nodeocov_senderattr <- function (nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=TRUE,
                      varnames = c("attr", "sender_attr", "value"),
                      vartypes = c(ERGM_VATTR_SPEC, "character", "character,numeric,logical"),
                      defaultvalues = list(NULL, NULL, NULL),
                      required = c(TRUE, TRUE, TRUE))
  ### Process the arguments
  sender_attr <- get.node.attr(nw, a$sender_attr)
  sender_attr <- ifelse(sender_attr == a$value, 1, 0)
  
  nodecov <- ergm_get_vattr(a$attr, nw, accept="numeric", multiple="matrix")
  coef.names <- paste("nodeocov",attr(nodecov, "name"),sep=".")
  if(is.matrix(nodecov)) coef.names <- paste(coef.names, NVL(colnames(nodecov), seq_len(ncol(nodecov))), sep=".")
  
  list(name="nodeocov_senderattr", coef.names=paste(coef.names, "_senderattr", sep = ""), inputs=c(nodecov, sender_attr), 
       dependence=FALSE, pkgname = "tc.ergmterms")
}

InitErgmTerm.nodemix_senderattr <- function (nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm(nw, arglist, directed = TRUE,
                      varnames = c("attr", "base", "levels", "levels2", "sender_attr", "value"),
                      vartypes = c(ERGM_VATTR_SPEC, "numeric", ERGM_LEVELS_SPEC, ERGM_LEVELS_SPEC, "character", "character,numeric,logical"),
                      defaultvalues = list(NULL, NULL, NULL, NULL, NULL, NULL),
                      required = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE),
                      dep.inform = list(FALSE, "levels2", FALSE, FALSE, FALSE, FALSE))
  attrarg <- a$attr
  sender_attr <- get.node.attr(nw, a$sender_attr)
  sender_attr <- ifelse(sender_attr == a$value, 1, 0)

  ### Process the arguments
  nodecov <- ergm_get_vattr(attrarg, nw)
  attrname <- attr(nodecov, "name")
  
  # So one mode, but could be directed or undirected
  u <- ergm_attr_levels(a$levels, nodecov, nw, sort(unique(nodecov)))
  namescov <- u
  
  if(any(is.na(nodecov))){u<-c(u,NA)}
  
  nr <- length(u)
  nc <- length(u)
  
  levels2.list <- transpose(expand.grid(row = u, col = u, stringsAsFactors=FALSE))
  indices2.grid <- expand.grid(row = 1:nr, col = 1:nc)
  uun <- as.vector(outer(u,u,paste,sep="."))
  
  
  levels2.sel <- ergm_attr_levels(a$levels2, list(row = nodecov, col = nodecov), nw, levels2.list)
  if((!hasName(attr(a,"missing"), "levels2") || attr(a,"missing")["levels2"]) && any(NVL(a$base,0)!=0)) levels2.sel <- levels2.sel[-a$base]
  
  rows2keep <- match(levels2.sel,levels2.list, NA)
  rows2keep <- rows2keep[!is.na(rows2keep)]
  
  u <- indices2.grid[rows2keep,]
  uun <- uun[rows2keep]
  
  nodecov <- match(nodecov,namescov,nomatch=length(namescov)+1)
  
  name <- "nodemix_senderattr"
  cn <- paste("mix", paste(attrname,collapse="."), uun, sep=".")
  inputs <- c(u[,1], u[,2], nodecov)
  attr(inputs, "ParamsBeforeCov") <- 2*length(uun)
  
  ### Construct the list to return
  list(name = name, coef.names = paste(cn, "_senderattr", sep = ""), # required
       inputs = c(inputs, sender_attr), 
       dependence = FALSE, # So we don't use MCMC if not necessary
       minval = 0, pkgname = "tc.ergmterms")
}

