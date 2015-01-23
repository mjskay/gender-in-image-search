### Various helpers and utility functions used by the analysis in README.Rmd


### Generate p stars for a vector of p values
p.stars = function(p.values) {
    unclass(symnum(p.values, corr = FALSE, na = FALSE, cutpoints = c(0, 
            0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", 
            "*", ".", " ")))
}

### pander RMarkdown output for summary.merMod objects
### (summaries of models fit by lmer / glmer)
pander.summary.merMod = function(x, ...) {
    #random effects
    cat("```\nRandom effects:\n")
    print(x$varcor)
    cat("```\n\n")
    
    #fixed effects
    x.coef = as.data.frame(coef(x))
    if (!is.null(x.coef$P)) {
        x.coef = cbind(x.coef, ` `=p.stars(x.coef$P))    
    }

    pandoc.table(x.coef, caption=paste("Fixed effects.", x$methTitle), emphasize.rownames=FALSE, ...)
}

### pander RMarkdown output for summary.clmm objects
### (summaries of models fit by clmm)
pander.summary.clmm = function(x, ...) {
    #random effects
    pandoc.table(ordinal:::formatVC(ordinal:::varcov(x)), caption="Random effects", ...)
    
    #fixed effects and thresholds
    fixed.coef = threshold.coef = as.data.frame(coef(x))
    if (!is.null(fixed.coef$P)) {
        #ignore p values for threshold coefs
        threshold.coef = threshold.coef[,-length(fixed.coef)]
        #add p stars for fixed effect coefs
        fixed.coef = cbind(fixed.coef, ` `=p.stars(fixed.coef$P))
    }
    threshold.coef = threshold.coef[names(x$alpha),]
    fixed.coef = fixed.coef[names(x$beta),] 
        
    pandoc.table(fixed.coef, caption=paste("Fixed effects"), emphasize.rownames=FALSE, ...)
    pandoc.table(threshold.coef, caption=paste("Threshold coefficients"), emphasize.rownames=FALSE, ...)
}
