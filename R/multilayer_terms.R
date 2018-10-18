# ERGM terms used for multilayer networks.
# For introduction to ERGM for multilayer networks, see paper at https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3189835.

# Section 1) Terms for within-layer dependence.

# 1.1) Edgecov within layer.
InitErgmTerm.edgecov_layer <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm(nw, arglist, 
                      varnames = c("x", "attrname", "layer"),
                      vartypes = c("matrix,network,character", "character", "numeric"),
                      defaultvalues = list(NULL, NULL, NULL),
                      required = c(TRUE, FALSE, TRUE))
  ### Process the arguments
  layer.mem <- get.node.attr(nw, "layer.mem")
  layer <- a$layer
  in.layer <- which(layer.mem == layer)
  layer.size <- length(in.layer)

  if(is.character(a$x)){
    if(a$x == "edges"){
      xm <- matrix(1, ncol = layer.size, nrow = layer.size)
    } else {
      xm<-get.network.attribute(nw,a$x)
      if(is.null(xm)){stop("There is no network attribute named ",a$x,call.=FALSE)}
    }
  } else {
    if(is.network(a$x)){
      xm<-as.matrix.network(a$x,matrix.type="adjacency",a$attrname)
    } else {
      xm<-as.matrix(a$x)
    }
  }
  
  # To multilayer 
  xm.l <- matrix(0, ncol = network.size(nw), nrow = network.size(nw))
  xm.l[in.layer,in.layer] <- xm
  
  ### Construct the list to return
  if(!is.null(a$attrname)) {
    # Note: the sys.call business grabs the name of the x object from the 
    # user's call.  Not elegant, but it works as long as the user doesn't
    # pass anything complicated.
    cn<-paste("edgecov.layer.l", layer, as.character(a$attrname), sep = ".")
  } else {
    cn<-paste("edgecov.layer", layer, as.character(sys.call(0)[[3]][2]), sep = ".")
  }
  inputs <- c(NCOL(xm.l), as.double(xm.l))
  attr(inputs, "ParamsBeforeCov") <- 1
  list(name="edgecov", coef.names = cn, inputs = inputs, dependence=FALSE,
       minval = sum(c(xm.l)[c(xm.l)<0]),
       maxval = sum(c(xm.l)[c(xm.l)>0]),
       pkgname = "ergm"
  )
}

# 1.2) Nodeifactor within layer
InitErgmTerm.nodeifactor_layer<-function (nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm(nw, arglist, directed=TRUE, 
                      varnames = c("attrname", "base", "levels", "layer"),
                      vartypes = c("character", "numeric", "character,numeric,logical", "numeric"),
                      defaultvalues = list(NULL, 1, NULL, NULL),
                      required = c(TRUE, FALSE, FALSE, TRUE))
  ### Process the arguments
  
  nodecov <-
    if(length(a$attrname)==1)
      get.node.attr(nw, a$attrname)
  else{
    do.call(paste,c(sapply(a$attrname,function(oneattr) get.node.attr(nw,oneattr),simplify=FALSE),sep="."))
  }
  
  u <- NVL(a$levels, sort(unique(nodecov)))
  if (any(NVL(a$base,0)!=0)) {
    u <- u[-a$base]
    if (length(u)==0) { # Get outta here!  (can happen if user passes attribute with one value)
      return()
    }
  }
  #   Recode to numeric
  nodepos <- match(nodecov,u,nomatch=0)-1
  
  # Layers
  layer <- a$layer
  layer.mem <- get.node.attr(nw, "layer.mem")
  
  ### Construct the list to return
  inputs <- nodepos
  list(name="nodeifactor_layer", #required
       coef.names = paste("nodeifactor_layer", layer, paste(a$attrname,collapse="."), u, sep="."), #required
       inputs = c(inputs, layer, layer.mem),
       dependence = FALSE, # So we don't use MCMC if not necessary
       minval = 0
  )
}

# 1.3) Nodeofactor within layer
InitErgmTerm.nodeofactor_layer<-function (nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm(nw, arglist, directed=TRUE, 
                      varnames = c("attrname", "base", "levels", "layer"),
                      vartypes = c("character", "numeric", "character,numeric,logical", "numeric"),
                      defaultvalues = list(NULL, 1, NULL, NULL),
                      required = c(TRUE, FALSE, FALSE, TRUE))
  ### Process the arguments
  nodecov <-
    if(length(a$attrname)==1)
      get.node.attr(nw, a$attrname)
  else{
    do.call(paste,c(sapply(a$attrname,function(oneattr) get.node.attr(nw,oneattr),simplify=FALSE),sep="."))
  }
  
  u <- NVL(a$levels, sort(unique(nodecov)))
  if (any(NVL(a$base,0)!=0)) {
    u <- u[-a$base]
    if (length(u)==0) { # Get outta here!  (can happen if user passes attribute with one value)
      return()
    }
  }
  #   Recode to numeric
  nodepos <- match(nodecov,u,nomatch=0)-1
  
  # Layers
  layer <- a$layer
  layer.mem <- get.node.attr(nw, "layer.mem")
  
  ### Construct the list to return
  inputs <- nodepos
  list(name="nodeofactor_layer",  #required
       coef.names = paste("nodeofactor_layer", layer, paste(a$attrname,collapse="."), u, sep="."), #required
       inputs = c(inputs, layer, layer.mem),
       dependence = FALSE, # So we don't use MCMC if not necessary
       minval = 0
  )
}  


# 1.1) GWDSP within layer.
InitErgmTerm.gwdsp_layer<-function(nw, arglist, initialfit=FALSE, ...) {
  # the following line was commented out in <InitErgm.gwdsp>:  
  #   ergm.checkdirected("gwdsp", is.directed(nw), requirement=FALSE)
  # so, I've not passed 'directed=FALSE' to <check.ErgmTerm>  
  a <- check.ErgmTerm(nw, arglist,
                      varnames = c("decay","fixed","cutoff","alpha","layer"),
                      vartypes = c("numeric","logical","numeric","numeric","numeric"),
                      defaultvalues = list(0, FALSE, 30, NULL, NULL),
                      required = c(FALSE, FALSE, FALSE, FALSE, FALSE))
  if(!is.null(a$alpha)){
    stop("For consistency with gw*degree terms, in all gw*sp and dgw*sp terms the argument ", sQuote("alpha"), " has been renamed to " ,sQuote("decay"), ".", call.=FALSE)
  }
  
  decay<-a$decay;fixed<-a$fixed
  cutoff<-a$cutoff
  layer<-a$layer
  layer.mem <- get.node.attr(nw, "layer.mem")
  if(!initialfit && !fixed){ # This is a curved exponential family model
    #   d <- 1:(network.size(nw)-1)
    maxesp <- min(cutoff,network.size(nw)-2)
    d <- 1:maxesp
    ld<-length(d)
    if(ld==0){return(NULL)}
    map<- function(x,n,...) {
      i <- 1:n
      x[1]*exp(x[2])*(1-(1-exp(-x[2]))^i)
    }
    gradient <- function(x,n,...) {
      i <- 1:n
      a <- 1-exp(-x[2])
      exp(x[2]) * rbind(1-a^i, x[1] * (1 - a^i - i*a^(i-1) ) )
    }
    if(is.directed(nw)){dname <- "tdsp"}else{dname <- "dsp"}
    list(name=dname, coef.names=paste("gwdsp#",d,sep=""), 
         inputs=c(d, layer, layer.mem), params=list(gwdsp=NULL,gwdsp.decay=decay),
         map=map, gradient=gradient)
  }else{
    if (initialfit && !fixed) # First pass to get MPLE coefficient
      coef.names <- "gwdsp"   # must match params$gwdsp above
    else  # fixed == TRUE
      coef.names <- paste("gwdsp.layer.",layer,".fixed.",decay,sep="")
    if(is.directed(nw)){dname <- "gwtdsp_layer"}else{dname <- "gwdsp"}
    list(name=dname, coef.names=coef.names, inputs=c(decay, layer, layer.mem), pkgname = "tc.ergmterms")
  }
}

# 1.2) GWESP within layer.
InitErgmTerm.gwesp_layer<-function(nw, arglist, initialfit=FALSE, ...) {
  # the following line was commented out in <InitErgm.gwesp>:
  #   ergm.checkdirected("gwesp", is.directed(nw), requirement=FALSE)
  # so, I've not passed 'directed=FALSE' to <check.ErgmTerm>  
  a <- check.ErgmTerm(nw, arglist,
                      varnames = c("decay","fixed","cutoff", "alpha","layer"),
                      vartypes = c("numeric","logical","numeric", "numeric","numeric"),
                      defaultvalues = list(0, FALSE, 30, NULL, NULL),
                      required = c(FALSE, FALSE, FALSE, FALSE, FALSE))
  if(!is.null(a$alpha)){
    stop("For consistency with gw*degree terms, in all gw*sp and dgw*sp terms the argument ", sQuote("alpha"), " has been renamed to " ,sQuote("decay"), ".", call.=FALSE)
  }
  
  decay<-a$decay;fixed<-a$fixed
  cutoff<-a$cutoff
  layer<-a$layer
  layer.mem <- get.node.attr(nw, "layer.mem")
  decay=decay[1] # Not sure why anyone would enter a vector here, but...
  if(!initialfit && !fixed){ # This is a curved exponential family model
    #   d <- 1:(network.size(nw)-2)
    maxesp <- min(cutoff,network.size(nw)-2)
    d <- 1:maxesp
    ld<-length(d)
    if(ld==0){return(NULL)}
    map <- function(x,n,...){
      i <- 1:n
      x[1]*exp(x[2])*(1-(1-exp(-x[2]))^i)
    }
    gradient <- function(x,n,...){
      i <- 1:n
      a <- 1-exp(-x[2])
      exp(x[2]) * rbind(1-a^i, x[1] * (1 - a^i - i*a^(i-1) ) )
    }
    if(is.directed(nw)){dname <- "tesp"}else{dname <- "esp"}
    list(name=dname, coef.names=paste("esp#",d,sep=""), 
         inputs=c(d, layer, layer.mem), params=list(gwesp=NULL,gwesp.decay=decay),
         map=map, gradient=gradient)
  }else{
    if (initialfit && !fixed)  # First pass to get MPLE coefficient
      coef.names <- "gwesp"
    else # fixed == TRUE
      coef.names <- paste("gwesp.layer.",layer,".fixed.",decay,sep="")
    if(is.directed(nw)){dname <- "gwtesp_layer"}else{dname <- "gwesp"}
    list(name=dname, coef.names=coef.names, inputs=c(decay, layer, layer.mem), pkgname = "tc.ergmterms")
  }
}

# Section 2) Terms for Cross-layer dependence.

# 2.1) Duplex-dyad terms
# Initializes an ergm term to count any combination of five cross-layer configurations in the duplex dyad census.
# There are five configurations: e, f, g, h, i. 
# (See pp 12-13 in https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3189835)

# The resulting duplexdyad() function to be used in the ergm framework takes three arguments:
# 1) "nw": the network object, which needs to be directed and not bipartite; and include a nodal attribute denoting layer membership called "layer.mem"
# 2) "type": the cross-layer configurations to include in the ergm model
# 3) "layers": the two layers to include in the duplex
InitErgmTerm.duplexdyad <- function(nw, arglist, ...) {
  # Initial check
  a <- check.ErgmTerm(nw, arglist, directed = TRUE, bipartite = FALSE,
                      varnames = c("type", "layers"),
                      vartypes = c("character", "numeric"),
                      defaultvalues = list(NULL, c(1,2)),
                      required = c(TRUE, FALSE))
  
  # Error messages for misspecified arguments.
  if(length(unique(a$layers))!=2){stop("duplexdyad() requires two different layers.")}
  if(any(!a$type %in% c(LETTERS[5:9],letters[5:9]))){stop("Invalid dyad structure type supplied to duplexdyad().")}
  
  # Preparing data for the C side.
  layer.mem <- get.node.attr(nw, "layer.mem")
  type <- match(tolower(a$type),letters)-4
  list(name = "duplexdyad", 
       coef.names = paste("duplexdyad.", a$type, sep=""),
       pkgname = "tc.ergmterms",
       inputs = c(a$layers, type, layer.mem),
       dependence = TRUE
  )
}

