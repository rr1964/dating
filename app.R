#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Access this app at
##########################################
# https://rr1964.shinyapps.io/dating/
##########################################

library(shiny)

if(!require("RColorBrewer"))
{
  install.packages("RColorBrewer")
}

library(RColorBrewer)


ourRed = colorRampPalette(brewer.pal(9,"Reds"))(6)
ourBlue = colorRampPalette(brewer.pal(9,"Blues"))(6)

ui <- fluidPage(
   
   # Application title
   titlePanel("The Dating Game"),
   
   
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId = "age",
                     label = h4("Age:"),
                     min = 18,
                     max = 100,
                     value = 30),
         sliderInput(inputId = "hot",
                     label = h4("Perceived Hotness:"),
                     min = 1,
                     max = 10,
                     value = 5),
         radioButtons("gender", h4("Gender:"),
                      choices = list("Female" = 1, "Male" = 2),selected = 1),
         checkboxInput(inputId = "clooney",
                       label ="George Clooney Option:", 
                       value = FALSE), ##Start off with GC option not checked. 
      
         
         span(textOutput("checkClooney"), style = "color:red"), ##This means the output text will be red.
         ##There are many ways (such as using CSS or something) to obtain red output text.
         ##I feel this is the simplest way to do such. 
         
         actionButton("goButton", "Submit") ###input name, display text.
         
         ),
      
      
      mainPanel(
         h4(textOutput("ageRange"), align = "middle"),
         h5(textOutput("clooneyApplied"), align = "middle", style = paste0("color:",ourBlue[4])),###You can use h1, h2, h3, h4....as they are used in HTML.
        plotOutput("available")
  
        
        
      )
   )
)


server <- function(input, output) {
   
  
      ageRangeM<-function(yourAge , clooneyAllowed)
      {
        lowBound = (sqrt(yourAge + 5)-9/11)^2 - (clooneyAllowed*input$clooney)*(yourAge/2 -7)
        upBound = (sqrt(yourAge + 3)+2/23)^2
        interval = c(lowBound, upBound)
        return(interval)
      }
      
      ageRangeF<-function(yourAge , clooneyAllowed)
      {
        lowBound = (sqrt(yourAge - 1)-1/19)^2 
        upBound = (sqrt(yourAge + 5) + 3/23)^2 + (clooneyAllowed*input$clooney)*(yourAge/2 -2)
        interval = c(lowBound, upBound)
        return(interval)
      }
      
     
       output$checkClooney <- renderText({
         
         if(input$clooney & ((input$age < 40 & input$gender == 2) | (input$age <25  & input$gender == 1)))
         { 
           ##The Clooney option can only be selected for males if the user is over age 40
           ##The Clooney option can only be selected for females if they are between age 25 and 56 (inclusive)
           return("You are too young to check the George Clooney option.") 
         } 
         
         else if(input$clooney & input$gender ==1 & input$age >56)
           
         {return("You are too old to select the George Clooney option.")}##GC is only 56 himself.
         ##I actually thought he was like 65! Not that he looks a day over 40, right?
         
         
         
         return("")
       })    
       
       output$ageRange <- renderText({
         
         input$goButton
         
         isolate({
         ####clooneyAllowed = TRUE
                 if(input$clooney & ((input$age < 40 & input$gender == 2) |
                                     (input$age <25  & input$gender == 1) |
                                     (input$age >56  & input$gender == 1)))
                 {
                   clooneyAllowed = FALSE
                 }
                 else{clooneyAllowed = TRUE}
                 
                 if(input$gender == 1)
                 {
                   datableAges = round(ageRangeF(input$age, clooneyAllowed), digits = 2)
                 }
                 else if(input$gender == 2)
                 {
                   datableAges = round(ageRangeM(input$age, clooneyAllowed), digits = 2)
                 }
                 
                 
                 return(paste("Your dating range is:", datableAges[1],"years to ",datableAges[2], " years."))
         })##End of Isolate.   
       })
       
       output$clooneyApplied <- renderText({
         
         input$goButton
         
         isolate({
           ####clooneyAllowed = TRUE
           if(input$clooney & ((input$age < 40 & input$gender == 2) |
                               (input$age <25  & input$gender == 1) |
                               (input$age >56  & input$gender == 1)))
           {
             #clooneyAllowed = FALSE
             return("Clooney option cannot be applied.")
           }
         else if(input$clooney)
          {
             return("Clooney option applied.")
          }
           
           
           
          
         })##End of Isolate.
         
         
       })
       
       
       output$available <- renderPlot({
         
         ####ourRed = colorRampPalette(brewer.pal(9,"Reds"))(6)
         
         input$goButton
         isolate({
               if(input$clooney & ((input$age < 40 & input$gender == 2) |
                                   (input$age <25  & input$gender == 1) |
                                   (input$age >56  & input$gender == 1)))
               {
                 clooneyAllowed = FALSE
               }
               else{clooneyAllowed = TRUE}
               
               
               if(input$gender == 1)
               {
                 ages = round(ageRangeF(input$age, clooneyAllowed), digits = 2)
                 hotness = input$hot
                 #sample(1:10, 1, prob = c(0.05, 0.05, 0.05, 0.1, 0.15, 0.2, 0.15, 0.15, 0.05, 0.05))
                 
                 
                 
                 randAges = runif(400, min = ages[1], max = ages[2])
                 randHot = runif(400, min = hotness-1.5, max = hotness + 1.5) + rnorm(400, mean = 0, sd = 0.25)
                 ##we add the rnorm part to avoid the strict boundaries. 
                 
                 
                 hubris = -0.025*randAges + randHot + rnorm(400, sd = 0.85)
                 #craziness
                 
                 plot(x = abs(randHot), y = abs(hubris), xlim = c(-0.5, 10.5), ylim = c(-0.5, 10.5),
                      main = "400 randomly sampled \n people in your dating range", col = ourRed[5],
                      pch = 1, cex=.85, xlab = "Hotness", ylab = "Hubris")
               }
               
               if(input$gender == 2)
               {
                 ages = round(ageRangeM(input$age, clooneyAllowed), digits = 2)
                 hotness = input$hot
                 #sample(1:10, 1, prob = c(0.05, 0.05, 0.05, 0.1, 0.15, 0.2, 0.15, 0.15, 0.05, 0.05))
                 
                 
                 
                 randAges = runif(400, min = ages[1], max = ages[2])
                 randHot = runif(400, min = hotness-1.5, max = hotness + 1.5) + rnorm(400, mean = 0, sd = 0.25)
                 ##we add the rnorm part to avoid the strict boundaries. 
                 
                 
                 craziness = -0.015*randAges + randHot + rnorm(400, sd = 0.85)
                 #craziness
                 
                 plot(x = abs(randHot), y = abs(craziness), xlim = c(-0.5, 10.5), ylim = c(-0.5, 10.5),
                      main = "400 randomly sampled \n people in your dating range", col = ourRed[5],
                      pch = 1, cex=.85, xlab = "Hotness", ylab = "Craziness")
                 
               }
         })###End of isolate()   
       })
       
       
       
   
      
}

# Run the application 
shinyApp(ui = ui, server = server)

