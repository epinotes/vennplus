intersectDiagram2 <- function (x, pct = FALSE, show.nulls = FALSE, xnames = NULL,
          sep = "+", mar = c(0, 0, 3, 0), main = "Intersection Diagram",
          cex = 1, col = NULL, minspacing = NA, all.intersections = FALSE,
          include = NULL, bg = ifelse(match(par("bg"), "transparent", 0), "white", par("bg")),
          border = TRUE)
{
  matchParts <- function(x, table, ignore.case = TRUE) {
    for (pattern in 1:length(x)) {
      match_index <- grep(x[pattern], table, ignore.case = ignore.case)
      if (length(match_index))
        return(match_index)
    }
    return(0)
  }
  if (!match(class(x), "intersectList", 0)) {
    if (is.matrix(x) || is.data.frame(x)) {
      if (is.data.frame(x))
        x <- as.matrix(x)
      x <- makeIntersectList(x, xnames = xnames, sep = sep)
    }
    if (!match(class(x), "intersectList", 0))
      stop("x must be a matrix, data frame or intersectList")
  }
  oldmar <- par("mar")
  par(mar = mar)
  attributes <- x[[length(x)]]
  if (is.null(include))
    include <- attributes
  nattributes <- length(attributes)
  x[[length(x)]] <- NULL
  nobjects <- x[[length(x)]]
  x[[length(x)]] <- NULL
  nlevels <- length(x)
  if (is.null(col))
    col <- c(rainbow(nattributes), NA)
  else if (length(col) < nattributes)
    col <- rep(col, length.out = nattributes)
  objectsums <- sapply(x, sum)
  maxlevel <- which.max(objectsums)
  nNonZero <- function(x) return(sum(x > 0))
  if (all.intersections)
    nintersects <- sapply(x, length)
  else nintersects <- sapply(x, nNonZero)
  maxintersections <- max(nintersects)
  maxn <- max(unlist(x))
  if (is.na(minspacing))
    minspacing <- 0.1 * maxn
  maxx <- objectsums[maxlevel] + minspacing * maxintersections
  attsep <- paste("[", sep, "]", sep = "")
  plot(0, xlim = c(0, maxx), ylim = c(0, nlevels + show.nulls),
       main = main, xlab = "", ylab = "", type = "n", axes = FALSE)
  for (level in 1:nlevels) {
    intersectLevel <- length(unlist(strsplit(names(x[[level]][1]),
                                             attsep)))
    if (all.intersections)
      intersections <- 1:nintersects[[level]]
    else intersections <- which(x[[level]] > 0)
    blocknames <- names(x[[level]])[intersections]
    spacing <- (maxx - objectsums[level])/nintersects[level]
    leftx <- c(0, cumsum(x[[level]][intersections] + spacing)) +
      spacing/2
    for (intersect in 1:length(intersections)) {
      if (matchParts(include, blocknames[intersect])) {
        cellqnt <- ifelse(pct, paste(round(100 * x[[level]][intersections[intersect]]/nobjects,
                                           1), "%", sep = ""), x[[level]][intersections[intersect]])
        colindex <- which(attributes %in% unlist(strsplit(blocknames[intersect],
                                                          attsep)))
        ncol <- length(colindex)
        xinc <- x[[level]][intersections[intersect]]/ncol
        slicecol <- col[colindex]
        offset <- 0
        for (slice in 1:ncol) {
          rect(leftx[intersect] + offset, nlevels - level +
                 show.nulls + 0.1, leftx[intersect] + offset +
                 xinc, nlevels - level + show.nulls + 0.9,
               col = slicecol[slice], border = NA)
          offset <- offset + xinc
        }
        rect(leftx[intersect], nlevels - level + show.nulls +
               0.1, leftx[intersect] + x[[level]][intersections[intersect]],
             nlevels - level + show.nulls + 0.9)
        boxed.labels2(leftx[intersect] + x[[level]][intersections[intersect]]/2,
                      nlevels - level + show.nulls + 0.5, paste(blocknames[intersect],
                                                                cellqnt, sep = "\n"), cex = cex, bg=bg, border=border)
      }
    }
  }
  if (show.nulls) {
    nonset <- as.numeric(nobjects - sum(objectsums))
    leftnulls <- sum(par("usr")[1:2])/2 - nonset/2
    if (nonset)
      rect(leftnulls, 0.1, leftnulls + nonset, 0.9)
    xpos <- leftnulls + nonset/2
    if (pct)
      nonset <- paste(round(100 * nonset/nobjects, 1),
                      "%", sep = "")
    boxed.labels2(xpos, 0.5, paste("Non-members", nonset,
                                   sep = "\n"), cex = cex)
  }
  par(mar = oldmar)
  x[[length(x) + 1]] <- nobjects
  x[[length(x) + 1]] <- attributes
  invisible(x)
}