# Thank you Rami Krispin
# Below is from his tutorial
# https://github.com/RamiKrispin/shinylive-r/tree/main


# install.packages("pak")
pak::pak("posit-dev/r-shinylive")
pak::pak("rstudio/httpuv")

# Version for the packages
# Uncomment the next two lines to see versions
# packageVersion("shinylive")
# packageVersion("httpuv")

shinylive::export(appdir = "Default Application", destdir = "docs")


# Thank you Ellis Hughes and Patrick Ward for your tutorial
# https://www.youtube.com/watch?v=B3Nggr9X4rY&t=239s

#See deployed application locally
httpuv::runStaticServer( dir="docs", port= 8888)

