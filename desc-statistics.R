# Descriptive statistics

# Frequency table for un-grouped distribution
freq <- function(x, digits = 0) {
   if(typeof(x) == "integer" | typeof(x) == "character"){
      tab <- table(x)
      df <- data.frame(Values = names(tab), Freq = as.vector(tab), Perc = round(as.vector(prop.table(tab))*100, digits = digits
                                                                                ))
      df
   } else stop("typeof(x) must be integer or character")
} 

# Frequency table for grouped continuous distribution
freq_continuous <- function(x, breaks = NULL, labels = NULL, digits = 0, ...) {
   if(typeof(x) != "double") {
      if(class(x) != "factor")
          stop('typeof(x) must be "double" or "factor"')
   }
   if(typeof(x) == "double") {
      if(!any(grepl("\\d*[.]\\d+", x))) stop("x must be a continuous vector")
      if(is.null(breaks)) {
         breaks <- hist(x, plot = FALSE)$breaks
      }
      groups <- cut(x, breaks, ...)
   } else {
      groups <- x
   }
   tab <- table(groups)
   if(is.null(labels)) {
      labels <- names(tab)
   }
   df <- data.frame(Values = labels, Freq = as.vector(tab), Perc = round(as.vector(prop.table(tab))*100, digits = digits))
   df
}

contigency_tab <- function(row, col, dimnmes = NULL, digits = 0) {
   tab <- table(row, col)
   perc <- round(prop.table(tab, 2)*100, digits)
   n <- ncol(tab)
   if(is.null(dimnmes)) {
       dimnms <- dimnames(tab)
   }
   colnms <- paste(dimnms[[2]], "%")
   dimnms <- list(dimnms[[1]], unlist(strsplit(colnms, " ")))
   names(dimnms) <- c(substitute(row), substitute(col))
   cont_list <- lapply(1:n, function(i) cbind(tab[,i, drop = FALSE], perc[, i, drop = FALSE]))
   matrix(unlist(cont_list), nrow = n, dimnames = dimnms)
}

slantAxisPlot <- function(x, labels = names(x), xlab = NULL, ...){
   origPar <- par("mar")
   par(mar = c(7, 4, 4, 2) + 0.1)
   plot(x, xaxt = "n", xlab = "", ...)
   axis(1, labels = FALSE)
   labs <- labels
   text(1:length(labs), par("usr")[3] - 0.25, srt = 45, adj = 1, labels = labs, xpd = TRUE)
   if (!is.null(xlab)){
      mtext(text = xlab, side = 1, line = 6)
   }
   par(mar = origPar)
}



slantAxisBarPlot <- function(x, labs = names(x), xlab = NULL, ..., line = 6){
   n <- length(labs)
   if (n > 15) warning("There too many categories, they might not be displayed well")
   origPar <- par("mar")
   par(mar = c(7, 4, 4, 2) + 0.1)
   barplot(x, xaxt = "n", xlab = "", ylab = "Frequency", ...)
   tcks <- barplot(x, plot = FALSE)
   #axis(1, at = tcks, labels = FALSE)
   text(tcks, par("usr")[3] - 0.25, srt = 45, adj = 1, labels = labs, xpd = TRUE)
   if (!is.null(xlab)){
      mtext(text = xlab, side = 1, line = line)
   }
   par(mar = origPar)
}
