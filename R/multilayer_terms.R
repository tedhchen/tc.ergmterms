# ERGM terms used for multilayer networks.
# For introduction to ERGM for multilayer networks, see paper at https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3189835.

# Section 1) Terms for within-layer dependence.

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
      coef.names <- paste("gwdsp.layer.l",layer,".fixed.",decay,sep="")
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
      coef.names <- paste("gwesp.layer.l",layer,".fixed.",decay,sep="")
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

