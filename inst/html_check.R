# install.packages("W3CMarkupValidator")
# install.packages("vnu.jar", type = "source",
#                  repos = "https://datacube.wu.ac.at/")

# MUST: devtools::document()
# MUST: devtools::build()

PKG_name_version <- glue::glue('{packageDescription("aggreCAT")$Package}_{packageDescription("aggreCAT")$Version}')

src <- glue::glue("../{PKG_name_version}.tar.gz")   # or just PKG if installed
out <- "aggreCAT.html"

tools::pkg2HTML(src, out)
library(W3CMarkupValidator)

results <- w3c_markup_validate(file = out, jar = TRUE)

if (length(results$type) > 0) results |> tibble::as_tibble()

stopifnot(
    all(fs::file_exists(c(src, out)))
)

file.remove(out)
