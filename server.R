# server file
# STAT 420
# app for CLT

library(ggplot2)

shinyServer(
     function(input, output) {    
          
          sampleMeans <- reactive({
               input$action
               
               if (input$dist == 'norm') { #N(mu,sigma^2)
                    
                    isolate(return(data.frame(xbar = replicate(1000, mean(rnorm(input$N, input$mu, input$sigma))))))
                    
               } else if (input$dist == 't') { #t(dft)
                    
                    isolate(return(data.frame(xbar = replicate(1000, mean(rt(input$N, input$dft))))))
                    
               } else if (input$dist == 'unif') { #U(a,b)
                    
                    isolate(return(data.frame(xbar = replicate(1000, mean(runif(input$N, input$a, input$b))))))
                    
               } else if (input$dist == 'chisq') { #\chi_{dfq}^2)
                    
                    isolate(return(data.frame(xbar = replicate(1000, mean(rchisq(input$N, input$dfq))))))
                    
               } else if (input$dist == 'F') { #F(nu1,nu2)
                    
                    isolate(return(data.frame(xbar = replicate(1000, mean(rf(input$N, input$nu1, input$nu2))))))
                    
               } else if (input$dist == 'exp') { #exp(lambda)
                    
                    isolate(return(data.frame(xbar = replicate(1000, mean(rexp(input$N, input$lambda))))))
                    
               } else if (input$dist == 'binom') { #Bin(n,p)
                    
                    isolate(return(data.frame(xbar = replicate(1000, mean(rbinom(input$N, input$n, input$p))))))
                    
               } else if (input$dist == 'gamma') { #\Gamma(alpha,beta)
                    
                    isolate(return(data.frame(xbar = replicate(1000, mean(rgamma(input$N, input$alpha1, input$beta1))))))
                    
               } else if (input$dist == 'beta') { #\Beta(alpha2,beta2)
                    
                    isolate(return(data.frame(xbar = replicate(1000, mean(rbeta(input$N, input$alpha2, input$beta2))))))
                    
               }      
               
               
          })
          
          means <- reactive({
               input$action
               
               if (input$dist == 'norm') { #N(mu,sigma^2)
                    
                    reject = isolate(shapiro.test(sampleMeans()$xbar)$p < .05)
                    isolate(return(list(reject = reject, sample = mean(sampleMeans()$xbar), expected = input$mu)))
                    
               } else if (input$dist == 't') { #t(dft)
                    
                    reject = isolate(shapiro.test(sampleMeans()$xbar)$p < .05)
                    isolate(return(list(reject = reject, sample = mean(sampleMeans()$xbar), expected = 0)))
                    
               } else if (input$dist == 'unif') { #U(a,b)
                    
                    reject = isolate(shapiro.test(sampleMeans()$xbar)$p < .05)
                    isolate(return(list(reject = reject, sample = mean(sampleMeans()$xbar), expected = (input$b + input$a)/2)))
                    
               } else if (input$dist == 'chisq') { #\chi_{dfq}^2)
                    
                    reject = isolate(shapiro.test(sampleMeans()$xbar)$p < .05)
                    isolate(return(list(reject = reject, sample = mean(sampleMeans()$xbar), expected = input$dfq)))
                    
               } else if (input$dist == 'F') { #F(nu1,nu2)
                    
                    reject = isolate(shapiro.test(sampleMeans()$xbar)$p < .05)
                    isolate(return(list(reject = reject, sample = mean(sampleMeans()$xbar), expected = input$nu2/(input$nu2 - 2))))
                    
               } else if (input$dist == 'exp') { #exp(lambda)
                    
                    reject = isolate(shapiro.test(sampleMeans()$xbar)$p < .05)
                    isolate(return(list(reject = reject, sample = mean(sampleMeans()$xbar), expected = input$lambda^(-1))))
                    
               } else if (input$dist == 'binom') { #Bin(n,p)
                    
                    reject = isolate(shapiro.test(sampleMeans()$xbar)$p < .05)
                    isolate(return(list(reject = reject, sample = mean(sampleMeans()$xbar), expected = input$n*input$p)))
                    
               } else if (input$dist == 'gamma') { #\Gamma(alpha,beta)
                    
                    reject = isolate(shapiro.test(sampleMeans()$xbar)$p < .05)
                    isolate(return(list(reject = reject, sample = mean(sampleMeans()$xbar), expected = input$alpha1/input$beta1)))
                    
               } else if (input$dist == 'beta') { #\Beta(alpha2,beta2)
                    
                    reject = isolate(shapiro.test(sampleMeans()$xbar)$p < .05)
                    isolate(return(list(reject = reject, sample = mean(sampleMeans()$xbar), expected = input$alpha2/(input$alpha2 + input$beta2))))
                    
               }      
               
               
          })
          
          # histogram
          output$CLThist <- renderPlot({
               
               input$action
               CLThist = isolate(ggplot(data = sampleMeans(), aes(x = xbar, y = ..density..)) +
                                      geom_histogram(fill = 'white', col = 'black') +
                                      geom_density() +
                                      xlab(bquote(bar(X)))+ 
                                      ggtitle('Sampling Distribution of the Mean'))
               print(CLThist)
               
          })
          
          output$text <- renderUI({
               
               input$action
               string1 = "The sample mean was estimated as "
               string2 = "The expected value of the sample mean is "
               string3 = "The Shapiro-Wilk test for normality is "
               
               isolate(return(HTML('<br/>', '<br/>', 
                                   paste(h6(string1), sprintf('%.4f', mean(sampleMeans()$xbar))), 
                                   '<br/>', '<br/>',
                                   paste(h6(string2)), sprintf('%.2f', means()$expected), 
                                   '<br/>', '<br/>', 
                                   paste(h6(string3, ifelse(means()$reject, 'REJECTED', 'NOT rejected.'))))))
               
               
          })
          
     })
