# ui file
# STAT 420
# app for CLT

shinyUI(
     fluidPage(
          title = 'Stat 420: Central Limit Theorem',
          
          sidebarPanel(
               
               # For LaTeX
               tags$head(tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                         tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')),
               
               # message
               h3('Demonstration of the central limit theorem [CLT] and the sampling distribution of the mean.', align = 'center'),
               br(), 
               p("This is a Shiny app. This Shiny app allows you to sample from a population of size ", HTML('$N$'), 
                 "from several distributions (Normal, t-distribution, uniform, chi-squared, F-distribution, exponential, 
                 binomial, gamma, and beta. A histogram of the sample means is provided with a superimposed normal 
                 density curve. In addition, the Shapiro-Wilk test for normality is performed."),
               
               br(), 
               
               # distributions to sample from
               selectInput('dist', 'Select Sampling Distribution',
                           c('Normal' = 'norm', 't-Distribution' = 't', 'Uniform' = 'unif',
                             'Chi-squared' = 'chisq', 'F-Distribution' = 'F', 'Exponential' = 'exp', 
                             'Binomial' = 'binom', 'Gamma' = 'gamma', 'Beta' = 'beta')),
               
               numericInput('N', HTML('$N$'), value = 10, min = 10, max = 1000, step = 10),
               
               # parameters for Normal distribution
               conditionalPanel(
                    condition = "input.dist == 'norm'",
                    numericInput('mu', HTML('$\\mu$'), value = 0, min = -100, max = 100, step = .1),
                    numericInput('sigma', HTML('$\\sigma$'), value = 1, min = .1, max = 10, step = .1)
               ),
               
               # parameters for t distribution
               conditionalPanel(
                    condition = "input.dist == 't'",
                    numericInput('dft', HTML('$\\nu$'), value = 2, min = 2, max = 100, step = 1)
               ),
               
               # parameters for uniform distribution
               conditionalPanel(
                    condition = "input.dist == 'unif'",
                    numericInput('a', HTML('$a$'), value = 0, min = -10, max = 0, step = .1),
                    numericInput('b', HTML('$b$'), value = 1, min = .1, max = 10, step = .1)
               ),
               
               # parameters for chi-squared distribution
               conditionalPanel(
                    condition = "input.dist == 'chisq'",
                    numericInput('dfq', HTML('$\\nu$'), value = 1, min = 1, max = 100, step = 1)
               ),
               
               # parameters for F distribution
               conditionalPanel(
                    condition = "input.dist == 'F'",
                    numericInput('nu1', HTML('$\\nu_1$'), value = 1, min = 1, max = 100, step = 1),
                    numericInput('nu2', HTML('$\\nu_2$'), value = 3, min = 3, max = 100, step = 1)
               ),
               
               # parameters for exponential distribution
               conditionalPanel(
                    condition = "input.dist == 'exp'",
                    numericInput('lambda', HTML('$\\lambda$'), value = 1, min = .1, max = 10, step = .1)
               ),
               
               # parameters for binomial distribution
               conditionalPanel(
                    condition = "input.dist == 'binom'",
                    numericInput('p', HTML('$p$'), value = .5, min = .05, max = .95, step = .05),
                    numericInput('n', HTML('$n$'), value = 10, min = 1, max = 100, step = 1)
               ),
               
               # parameters for gamma distribution
               conditionalPanel(
                    condition = "input.dist == 'gamma'",
                    numericInput('alpha1', HTML('$\\alpha$'), value = 1, min = .5, max = 10, step = .5),
                    numericInput('beta1', HTML('$\\beta$'), value = 2, min = .5, max = 20, step = .5)
               ),
               
               # parameters for gamma distribution
               conditionalPanel(
                    condition = "input.dist == 'beta'",
                    numericInput('alpha2', HTML('$\\alpha$'), value = 1, min = .5, max = 10, step = .5),
                    numericInput('beta2', HTML('$\\beta$'), value = 2, min = .5, max = 20, step = .5)
               ),
               
               actionButton('action', 'Simulate Data!')
               
               
          ),
          
          mainPanel(
               
               plotOutput('CLThist'),
               htmlOutput('text')
               
          )
          
     )
)
