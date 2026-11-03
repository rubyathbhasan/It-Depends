#Additional set-up codes to host the app in Github

#Exporting App to WebAssembly Files
install.packages("shinylive")
install.packages(c("callr", "curl", "httr2", "openssl", "processx", "zip"))
# Since you are in your R Project, "." refers to the current folder
shinylive::export(appdir = ".", destdir = "docs")


#Testing App locally
install.packages("httpuv")
httpuv::runStaticServer("docs")
