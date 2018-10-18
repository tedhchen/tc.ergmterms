.onAttach <- function(libname, pkgname) {
  desc<- packageDescription(pkgname, libname)
  packageStartupMessage(
    '======================\n',
    'Package:  tc.ergmterms\n', 
    'Version:  ', desc$Version, '\n', 
    'Date:     ', desc$Date, '\n', 
    'Authors:  Ted Hsuan Yun Chen (Pennsylvania State University)\n'
  )
  packageStartupMessage("Based on 'statnet' project software (statnet.org). For license and citation information see statnet.org/attribution.")
}


# Function to duplicate networks into multiple layers. Three arguments: 
# 1) nw is the network to be duplicated; 
# 2) layers is the number of layers; 
# 3) which layers to duplicate network on (default all)
to.multiplex <- function(nw, layers, keep = 1:layers){
  ml.nw <- matrix(0, ncol = ncol(nw)*layers, nrow = nrow(nw)*layers)
  for(i in keep){
    ml.nw[(1:nrow(nw))+((i-1)*nrow(nw)), (1:ncol(nw))+((i-1)*ncol(nw))] <- nw
  }
  ml.nw
}