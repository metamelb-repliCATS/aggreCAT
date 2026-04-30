# install.packages("W3CMarkupValidator")
# install.packages("vnu.jar", type = "source",
#                  repos = "https://datacube.wu.ac.at/")

# MUST: devtools::document()
# MUST: devtools::build()

src <- here::here("../aggreCAT_1.0.1.tar.gz")   # or just PKG if installed
out <- here::here("aggreCAT.html")

stopifnot(
    all(fs::file_exists(c(src, out)))
)

tools::pkg2HTML(src, out)
library(W3CMarkupValidator)

results <- w3c_markup_validate(file = out, jar = TRUE)

if (length(results$type) > 0) results |> tibble::as_tibble()
