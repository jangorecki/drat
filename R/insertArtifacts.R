pkgName = function(file) {
    pkg = basename(file)
    pkgname = gsub("\\.tar\\..*$", "", pkg)
    strsplit(pkgname, "_", fixed=TRUE)[[1L]][1L]
}

artifactsManual = function(file, pkg, repodir) {
    tmp.lib = file.path(tempdir(), "library")
    if (!file.exists(tmp.lib)) dir.create(tmp.lib, recursive = TRUE)
    # install package required to render html
    r = tryCatch( # `utils::` to avoid RStudio issue
        utils::install.packages(file, lib = tmp.lib, type = "source", repos = NULL, quiet = TRUE, INSTALL_opts = c("--html")),
        error = function(e) e,
        warning = function(w) w
    )
    # stop on warning too
    if(inherits(r, "error") || inherits(r, "warning")) stop(sprintf("Installation of %s into temporary library '%s' failed with %s: %s.", pkg, tmp.lib, if(inherits(r, "error")) "error" else "warning", as.character(r$message)), call. = FALSE)
    on.exit(try(remove.packages(pkg, lib = tmp.lib), silent=TRUE))
    
    inst.pkg = system.file(package = pkg, lib.loc = tmp.lib)
    html.source = file.path(inst.pkg, "html")
    if(!file.exists(html.source)) return(character())
    html.target = file.path(repodir, "library", pkg, "html")
    if(file.exists(html.target)) unlink(html.target, force = TRUE)
    dir.create(html.target, recursive = TRUE)
    file.copy(list.files(html.source, full.names = TRUE), html.target, overwrite = TRUE)
    html.files = file.path("library", pkg, "html", basename(list.files(html.target)))
    html.files[file.exists(html.target)]
}

artifactsDoc = function(pkg, repodir) {
    rcheck.source = paste0(pkg, ".Rcheck")
    doc.source = file.path(rcheck.source, pkg, "doc")
    if(!file.exists(doc.source)) return(character())
    doc.target = file.path(repodir, "library", pkg, "doc")
    if(file.exists(doc.target)) unlink(doc.target, force = TRUE)
    dir.create(doc.target, recursive = TRUE)
    file.copy(list.files(doc.source, full.names = TRUE), doc.target, overwrite = TRUE)
    doc.files = file.path("library", pkg, "doc", basename(list.files(doc.target)))
    doc.files[file.exists(doc.target)]
}

artifactsDescription = function(pkg, repodir) {
    rcheck.source = paste0(pkg, ".Rcheck")
    desc.source = file.path(rcheck.source, pkg, "DESCRIPTION")
    if(!file.exists(desc.source)) return(character())
    desc.target = file.path(repodir, "library", pkg, "DESCRIPTION")
    file.copy(desc.source, desc.target, overwrite = TRUE)
    desc.file = file.path("library", pkg, basename(desc.target))
    desc.file[file.exists(desc.target)]
}

artifactsRcheck = function(pkg, repodir, files = c("00install.out", "00check.log")) {
    rcheck.source = paste0(pkg, ".Rcheck")
    files = unique(files)
    src.file.exists = file.exists(file.path(rcheck.source, files))
    if (!all(src.file.exists)) {
        warning(sprintf("Files not present in %s: '%s'", rcheck.source, paste(files[!src.file.exists], collapse="', '")))
        files = files[src.file.exists]
    }
    rcheck.target = file.path(repodir, rcheck.source)
    if (!file.exists(rcheck.target)) dir.create(rcheck.target, recursive = TRUE)
    # populate dirs to handle any logs from Rcheck
    dirs = file.path(rcheck.target, unique(dirname(files)))
    sapply(dirs[!file.exists(dirs)], dir.create, recursive = TRUE)
    # copy files
    file.copy(
        file.path(rcheck.source, files),
        files.target <- file.path(rcheck.target, files),
        overwrite = TRUE
    )
    files[file.exists(files.target)]
}

artifactsGit = function() {
    
    # try get git ref info from gitlab-ci env vars
    gitrefname = Sys.getenv("CI_BUILD_REF_NAME", unset = NA)
    gitref = Sys.getenv("CI_BUILD_REF", unset = NA)
    
    # try travis-ci git ref info if above not available
    if (is.na(gitrefname)) gitrefname = Sys.getenv("TRAVIS_BRANCH", unset = NA)
    if (is.na(gitref)) gitref = Sys.getenv("TRAVIS_COMMIT", unset = NA)
    
    # if none then make 0 length chars
    gitrefname = na.omit(gitrefname)
    gitref = na.omit(gitref)
    
    c(gitrefname = gitrefname, gitref = gitref)
}

artifactsIndex = function(pkg, repodir, repo.url = character(), repo.cran = character(), man = character(), doc = character(), desc = character(), log = character(), git = character()) {
    
    pkgdesc = file.path(repodir, "library", pkg, "DESCRIPTION") # already should be copied here by insertManual, optionally as source of the build
    stopifnot(
        file.exists(pkgdesc),
        file.exists(repodir),
        is.character(repo.url),
        is.character(repo.cran),
        is.character(man),
        is.character(doc),
        is.character(log),
        is.character(git)
    )
    
    # extract package metadata
    dcf = read.dcf(pkgdesc)
    if (!identical(dcf.pkg <- unname(dcf[,"Package"]), pkg)) {
        warning(sprintf("Argument provided as package name is '%s' while loaded from corresponding DESCRIPTION is '%s', looks like you've made some badly manipulation between directories.", pkg, dcf.pkg))
        pkg = dcf.pkg
    }
    pkgtitle = if ("Title" %in% colnames(dcf)) unname(dcf[,"Title"])
    pkgversion = if ("Version" %in% colnames(dcf)) unname(dcf[,"Version"])
    upstream = if ("URL" %in% colnames(dcf)) unname(dcf[,"URL"])
    additional.repo.url = if ("Additional_repositories" %in% colnames(dcf)) strsplit(unname(dcf[,"Additional_repositories"]), split = ",", fixed = TRUE)[[1L]]
    # remove leading and trailing whitespaces
    additional.repo.url = gsub("^\\s+|\\s+$", "", additional.repo.url)
    
    # Additional_repositories are included into install string
    repo.url = c(repo.url, additional.repo.url)
    
    # build html file
    html = c(
        "<!DOCTYPE html>",
        "<html>",
        sprintf("<head><title>%s</title></head>", pkg),
        "<body>",
        sprintf("<h3>%s</h3>", pkg),
        sprintf("<h4>%s</h4>", pkgtitle),
        if (length(pkgversion)) sprintf("<p>Package version: %s</p>", pkgversion),
        if (length(git)) {
            gitlabel = c(gitrefname = "Git branch:", gitref = "Git commit:")
            if (!all(names(git) %in% names(gitlabel))) stop("only 'gitrefnames' and 'gitref' names supported as 'git' argument")
            sprintf("<p>%s</p>", paste(paste(gitlabel[names(git)], git), collapse = "<br/>"))
        },
        if (length(upstream)) sprintf('<p><a href="%s">upstream repo</a></p>', upstream),
        if (length(man) | length(doc)) {
            sprintf("<p>%s</p>", paste(sprintf(c('<a href="library/%s/html/00Index.html">manual</a>'[as.logical(length(man))], '<a href="library/%s/doc/index.html">doc</a>'[as.logical(length(doc))]), pkg), collapse="<br/>"))
        },
        if (length(repo.url)) c(
            "<p>Install from R:</p>",
            sprintf('<pre><code>install.packages("%s", repos = %s)</code></pre>', pkg, paste(deparse(repo.url, width.cutoff = 500), collapse=""))
        ),
        if (length(log)) {
            sprintf("<p>%s</p>", paste(sprintf('<a href="%s.Rcheck/%s">%s</a>', pkg, log, log), collapse = "<br/>"))
        },
        "<br/>",
        sprintf("<p><small>generated at %s</small></p>", as.character(Sys.time())),
        "</body>",
        "</html>"
    )
    
    # write index.html
    html.path = file.path(repodir, "index.html")
    writeLines(html, html.path)
    
    "index.html"[file.exists(html.path)]
}

##' @title Generate R package artifacts after Build and Check
##' @description Generate basic html website with basic package info, includes html manuals, package installation string, links to check logs. Additionally \emph{CI} environment variables for git branch and git commit will be included if available.
##' @param file An R package in source or binary format.
##' @param repodir A local directory corresponding to the repository top-level directory.
##' @param repo.url character, url to be used in \emph{repos} argument in generated \code{install.packages} call for html website.
##' @param repo.cran logical or character, default FALSE, if TRUE it will append repository list with CRAN repo in generated \code{install.packages} call.
##' @param log.files character vector of files to copy from \emph{*.Rcheck} dir, for drat that could be \code{tests/simpleTests.Rout}.
##' @details Function assumes the working directory contains package source tar.gz file made by \code{R CMD build} and package \emph{.Rcheck} directory made by \code{R CMD check}. To produce html manual it will install the package to temporary library.
##' @return TRUE invisibly if \code{index.html} generated successfully.
##' @examples
##' \dontrun{
##'   insertArtifacts(file = "drat_0.1.0.1.tar.gz",
##'                   repodir = "drat", 
##'                   repo.url = "https://eddelbuettel.github.io/drat")
##' }
##' @author Jan Gorecki
insertArtifacts = function(file = list.files(pattern = "*\\.tar\\.gz$"), repodir = "public", repo.url = character(), repo.cran = FALSE, log.files = character()) {
    
    pkg = pkgName(file)
    rcheck.source = paste0(pkg, ".Rcheck")
    if(!file.exists(repodir)) dir.create(repodir, recursive = TRUE)
    
    if (!is.character(repo.cran)) repo.cran = "https://cran.rstudio.com"[isTRUE(repo.cran)]
    
    stopifnot(
        file.exists(file),
        file.exists(rcheck.source),
        file.exists(repodir),
        is.character(repo.url),
        is.character(repo.cran),
        is.character(log.files)
    )

    # insertPackage
    drat = insertPackage(file = file, repodir = repodir)
    
    # artifactsManual - html manuals (from installed pkg with '--html')
    man = artifactsManual(file = file, pkg = pkg, repodir = repodir)
    
    # artifactsDoc - vignettes (from Rcheck)
    doc = artifactsDoc(pkg = pkg, repodir = repodir)
    
    # artifactsDescription - DESCRIPTION file (from Rcheck)
    desc = artifactsDescription(pkg = pkg, repodir = repodir)
    
    # artifactsRcheck - 00install.out, 00check.log (from Rcheck)
    log = artifactsRcheck(pkg = pkg, repodir = repodir, files = c("00install.out", "00check.log", log.files))
    
    # git commit and ref (from CI env vars)
    git = artifactsGit()
    
    # artifactsIndex
    invisible(identical(
        artifactsIndex(pkg=pkg, repodir=repodir, repo.url=repo.url, repo.cran=repo.cran, man=man, doc=doc, desc=desc, log=log, git=git),
        "index.html"
    ))
}
