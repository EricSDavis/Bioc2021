## -----------------------------------------------------------------------------
knitr::opts_knit$set(global.device = TRUE)

## ---- message=FALSE, warning=FALSE--------------------------------------------
set.seed(123)
x <- Bioc2021::makeExampleMatchedDataSet(type = "GRanges")
x

## ---- message=FALSE, warning=FALSE--------------------------------------------
focal <- x[x$feature1]
pool <- x[!x$feature1]

## ---- message=FALSE, warning=FALSE, results='hold'----------------------------
library(nullranges)
mgr <- matchRanges(focal = focal,
                   pool = pool,
                   covar = ~color + length,
                   method = 'stratified',
                   replace = FALSE)

mgr

## -----------------------------------------------------------------------------
overview(mgr)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(BentoBox)
region <- bbParams(chrom = "chr1", chromstart = 1, chromend = 1000)

## ---- message=FALSE, warning=FALSE, fig.width=8.5, fig.height=6.5-------------
bbPageCreate(width = 8.5, height = 6.5, default.units = 'inches')

## ---- message=FALSE, warning=FALSE, results = 'hold', fig.width=8.5, fig.height=6.5----
poolSet <- bbPlotRanges(data = pool,
                        params = region,
                        x = 1,
                        y = 1,
                        width = 2.5,
                        height = 2.5,
                        fill = pool$color)

bbAnnoGenomeLabel(plot = poolSet,
                  x = 1,
                  y = 3.55)

bbPlotText(label = "Pool Set",
           x = 2.25,
           y = 0.9,
           just = c("center", "bottom"),
           fontcolor = "#33A02C",
           fontface = "bold",
           fontfamily = 'mono')



## ---- message=FALSE, warning=FALSE, results = 'hold', fig.width=8.5, fig.height=6.5----
focalSet <- bbPlotRanges(data = focal,
                         params = region,
                         x = 5,
                         y = 1,
                         width = 2.5,
                         height = 1,
                         fill = focal$color)

bbAnnoGenomeLabel(plot = focalSet,
                  x = 5,
                  y = 2.05)

bbPlotText(label = "Focal Set",
           x = 6.25,
           y = 0.9,
           just = c("center", "bottom"),
           fontcolor = "#1F78B4",
           fontface = "bold",
           fontfamily = 'mono')

## ---- message=FALSE, warning=FALSE, results = 'hold', fig.width=8.5, fig.height=6.5----
## Matched set
matchedSet <- bbPlotRanges(data = mgr,
                           params = region,
                           x = 5,
                           y = 2.5,
                           width = 2.5,
                           height = 1,
                           fill = mgr$color)

bbAnnoGenomeLabel(plot = matchedSet,
                  x = 5,
                  y = 3.55)

bbPlotText(label = "Matched Set",
           x = 6.25,
           y = 2.75,
           just = c("center", "bottom"),
           fontcolor = "#A6CEE3",
           fontface = "bold",
           fontfamily = 'mono')

## ---- message=FALSE, warning=FALSE, results='hold', fig.width=8.5, fig.height=6.5----
library(ggplot2)
smallText <- theme(legend.title = element_text(size=8),
                   legend.text=element_text(size=8),
                   title = element_text(size=8),
                   axis.title.x = element_text(size=8),
                   axis.title.y = element_text(size=8))

## Plot propensity scores ggplot in BentoBox
propensityPlot <-
  plotPropensity(mgr, sets=c('f','m','p')) +
  smallText +
  theme(legend.key.size = unit(0.5, 'lines'),
        title = element_blank())

bbPlotGG(plot = propensityPlot,
          x = 1, y = 4.5, width = 2.5, height = 1.5,
          just = c("left", "top"))

## Text labels
bbPlotText(label = "plotPropensity()",
            x = 1.10, y = 4.24,
            just = c("left", "bottom"),
            fontface = "bold",
            fontfamily = 'mono')
bbPlotText(label = "~color + length",
            x = 1.25, y = 4.5,
            just = c("left", "bottom"),
            fontsize = 10,
            fontfamily = "mono")

## ---- message=FALSE, warning=FALSE, results='hold', fig.width=8.5, fig.height=6.5----

## Plot "color" covariate
covColor <-
  plotCovariate(x=mgr, covar=covariates(mgr)[1], sets=c('f','m','p')) +
  smallText +
  theme(legend.text = element_blank(),
        legend.position = 'none')

bbPlotGG(plot = covColor,
          x = 3.50, y = 4.5, width = 1.8, height = 1.5,
          just = c("left", "top"))


## Plot "length" covariate
covLength <-
  plotCovariate(x=mgr, covar=covariates(mgr)[2], sets=c('f','m','p'))+
  smallText + 
  theme(legend.key.size = unit(0.5, 'lines'))

bbPlotGG(plot = covLength,
          x = 5.30, y = 4.5, width = 2.75, height = 1.5,
          just = c("left", "top"))


## Text labels
bbPlotText(label = "plotCovariate()",
           x = 3.75,
           y = 4.24,
           just = c("left", "bottom"),
           fontface = "bold",
           fontfamily = "mono")
bbPlotText(label = covariates(mgr),
           x = c(4, 5.9),
           y = 4.5,
           just = c("left", "bottom"),
           fontsize = 10,
           fontfamily = "mono")

