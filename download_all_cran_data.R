devtools::install_github("gaborcsardi/gh")

library("gh")
my_repos <- gh("/users/cran/repos", type = "public")
vapply(my_repos, "[[", "", "name")


dd <- gh("GET /search/code/:user/:extension/:path", user = "cran", extension = "rda", path = "data")
user:cran extension:rda path:/data



my_repos2 <- gh("GET /users/:username/repos", username = "gaborcsardi",
                type = "public", page = 3)
vapply(my_repos2, "[[", "", "name")





gh("/cran/repos", type = "public", 
   .api_url = "https://github.com/search?utf8=%E2%9C%93&q=user%3Acran+extension%3ARdata+path%3A%2Fdata&type=Code")


gh(endpoint = "/users/cran/repos",
   .api_url = "https://api.github.com/search/code?q=user:cran+extension:rda+path:/data")



cran_repos_gh <- gh("/users/cran/repos")
cran_repos <- vapply(cran_repos_gh, "[[", "", "name")

get_data_url <- function(repo) {
  ## TODO: make sure error is not due to PI rate limit
  dat_gh <- try(gh("/repos/:owner/:repo/contents/:path",
                   owner = "cran", repo = repo, path = "/data"),
                silent = TRUE)
  if("gh_response" %in% class(dat_gh)) {
    return(vapply(dat_gh, "[[", "", "download_url"))
  } else {
    return(NULL)
  }
}

cran_data_url <- sapply(cran_repos, get_data_url, .progress = "text")
cran_data_url

dat_gh <- gh("/repos/:owner/:repo/contents/:path",
   owner = "cran", repo = "RadialPlotter", path = "/data")

vapply(dat_gh, "[[", "", "download_url")
