##' @title Remove a package source or binary file into a drat repository
##' @aliases drat:::remove
##' @param file An R package in source or binary format,
##' @param repodir A local directory corresponding to the repository
##' top-level directory.
##' @param commit Either boolean toggle to select automatic git operations
##' \sQuote{add}, \sQuote{commit}, and \sQuote{push} or, alternatively,
##' a character variable can be used to specify a commit message; this also
##' implies the \sQuote{TRUE} values in other contexts.
##' @param pullfirst Boolean toggle to call \code{git pull} before removing the package.
##' @param action A character string containing one of: \dQuote{none}
##' (the default; add the new package into the repo, effectively masking
##' previous versions), \dQuote{archive} (place any previous versions into
##' a package-specific archive folder, creating such an archive if it does
##' not already exist), or \dQuote{prune} (calling \code{\link{pruneRepo}}).
##' @param ... For \code{insert} the aliases variant, a catch-all collection of
##' parameters. For \code{insertPackage} arguments passed to \code{write_PACKAGES}.
##' @return NULL is returned.
##' @examples
##' \dontrun{
##'   removePackage("foo_0.2.3.tar.gz")   # Removes foo_0.2.3.tar.gz from repo in cwd
##' }
##' @author Dirk Eddelbuettel, Neal Fultz
removePackage <- function(file,
                          commit=FALSE,
                          pullfirst=FALSE,
                          ...) {
  
  if (!file.exists(file)) stop("File ", file, " not found\n", call.=FALSE)
  
  ## check for the optional git2r package
  haspkg <- requireNamespace("git2r", quietly=TRUE)
  hascmd <- length(Sys.which("git")) > 0
  
  curwd <- getwd()
  on.exit(setwd(curwd))               # restore current working directory
  
  file <- normalizePath(file, mustWork = TRUE) # Error if result cannot be determinded
  pkg <- basename(file)
  pkgdir <- dirname(file)
  setwd(pkgdir)
  
  
  msg <- if (isTRUE(commit)) sprintf("Removing %s from drat", pkg) else ""
  ## special case of commit via message: not TRUE, and character
  if (!isTRUE(commit) && typeof(commit) == "character" && nchar(commit) > 0) {
    msg <- commit
    commit <- TRUE
  }
  
  branch <- getOption("dratBranch", "gh-pages")
  if (commit && haspkg) {
    repo <- git2r::repository(pkgdir, discover=TRUE)
    if (isTRUE(pullfirst)) git2r::pull(repo)
    git2r::checkout(repo, branch)
  } else if (commit && hascmd) {
    setwd(pkgdir)
    if (isTRUE(pullfirst)) system("git pull")
    system2("git", c("checkout", branch))
    setwd(curwd)
  }
  
  pkgtype <- identifyPackageType(file)
  file.remove(file)
  write_PACKAGES(pkgdir, type=pkgtype, ...)  

  ## update index
  
  if (commit) {
    if (haspkg) {
      repo <- git2r::repository(pkgdir, discover=TRUE)
      setwd(pkgdir)
      git2r::add(repo, sub(git2r::workdir(repo), "", file))
      git2r::add(repo, "PACKAGES")
      git2r::add(repo, "PACKAGES.gz")
      git2r::add(repo, "PACKAGES.rds")
      tryCatch({
        git2r::commit(repo, msg)
        #TODO: authentication woes?   git2r::push(repo)
        message("Removed and committed ", pkg, " plus PACKAGES files. Still need to push.\n")
        }, error = function(e) warning(e))
    } else if (hascmd) {
      setwd(pkgdir)
      pkgfs <- "PACKAGES PACKAGES.gz PACKAGES.rds"
      cmd <- sprintf(paste("git add %s;",
                           "git add %s;",
                           "git commit -m\"%s\";",
                           "git push"), pkg, pkgfs, msg)
      system(cmd) ## TODO: error checking
      message("Removed, committed and pushed ", pkg, " plus PACKAGES files.\n")
    } else {
      warning("Commit skipped as both git2r package and git command missing.",
              call.=FALSE)
    }
  } 
  
  invisible(NULL)
}



##' @rdname insertPackage
remove <- removePackage


