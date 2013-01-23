# UI-elements for DOE.R

# variable selection - CRD Analysis
output$crd_depvar <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "crd_depvar", label = "Select dependent variable:", choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection - CRD Analysis
output$crd_var2 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "crd_var2", label = "Variables (select one):", choices = vars[-which(vars == input$crd_var1)], selected = NULL, multiple = TRUE)
})

ui_crdAnalysis <- function() {
  wellPanel(
    # tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }")),
    # radioButtons(inputId = "cm_paired", label = "Test type:", c("Paired" = "paired", "Independent" = "independent"), selected = ""),
    uiOutput("crd_depvar"),
    uiOutput("crd_var2"),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
                     # selectInput(inputId = "cm_alternative", label = "Alternative hypothesis", choices = alt, selected = "Two sided"),
                     sliderInput('crd_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01)
    )
  )
}

summary.crdAnalysis <- function(result) {
  # if(class(result)[1] == "aov") {
  print(summary(result))
  cat("\n")
  print(model.tables(result,"means"),digits=3) 
  cat("\n")
  TukeyHSD(result, ordered = TRUE, conf.level = input$crd_sigLevel)
  # } else {
  # 	result
  # }
}

plot.crdAnalysis <- function(result) {
  
  var1 <- input$crd_depvar
  var2 <- input$crd_var2
  
  dat <- getdata()[,c(var1,var2)]
  
  # dat[,var1] < as.factor(dat[,var1])
  
  plots <- list()
  plots[["Boxplot"]] <- ggplot(dat, aes_string(x=var1, y=var2, fill=var1)) + 
    geom_boxplot() + geom_jitter()
  
  plots[["Density"]] <- ggplot(dat, aes_string(x=var2, fill=var1)) +
    geom_density(alpha=.3)
  
  print(do.call(grid.arrange, c(plots, list(ncol = 1))))
}

extra.crd <- function(result) {
  # nothing here yet, could put in test variance equality
  cat("Under development\n")
}

crdAnalysis <- reactive(function() {
  if(is.null(input$crd_var2)) return("Please select a variable")
  var1 <- input$crd_depvar
  var2 <- input$crd_var2
  dat <- getdata()[,c(var1,var2)]
  if(!is.factor(dat[,var1])) {
    dat <- data.frame(cbind(as.factor(c(rep(var1,nrow(dat)),rep(var2,nrow(dat)))),c(dat)))
    # colnames(dat) <- c(var1,var2)
    var1 <- "Numeric"
    var2 <- "Factor"
    colnames(dat) <- c(var1,var2)
  }
  
  formula <- as.formula(paste(var2[1], "~", var1))
  aov(formula, data = dat, conf.level = input$crd_sigLevel)
})