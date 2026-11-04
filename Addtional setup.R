#Additional set-up codes to host the app in Github

#Exporting App to WebAssembly Files
install.packages("shinylive")
install.packages(c("callr", "curl", "httr2", "openssl", "processx", "zip"))

library(shinylive)
shinylive::export(appdir = ".", destdir = "docs")


#Testing App locally
install.packages("httpuv")
httpuv::runStaticServer("docs")
