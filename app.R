
library(shiny)
library(shinythemes)



if (interactive()){
ui <- fluidPage( width= 5,align="center",
   
   theme = shinytheme("cerulean"),
   titlePanel("PASSWORD GENERATOR"),
   
      
      mainPanel(  width = 13,
        selectInput("length","Password Length",c(4,5,6,7,8,9,10,11,12,13,14),FALSE),
        checkboxInput("capitals","Use Capital Letters e.g A,H,B,E,U only",FALSE),
        checkboxInput("small","Use Lower Case Letters e.g a,e,y,f,g only",FALSE),
        checkboxInput("special","Use Special Characters e.g #,@,%,^ only",FALSE),
        checkboxInput("numbers","Use numbers e.g 3,4,5,1,6 only",FALSE),
        textOutput("password")
      )
   )



server <- function(input, output) {
  
   output$password <- renderText({
     
     if (isTRUE(input$capitals)  ) 
           
       {
       
       n <- input$length
       
       
       generate.pass<- function(n)
         
         return(paste0(sample(LETTERS,n, TRUE, prob = 0.1 : 26), collapse = "")) 
       
       
       
       generate.pass(n)
        
     } else if (isTRUE(input$small)){
       n <- input$length
       
       
       generate.pass<- function(n)
         
         return(paste0(sample(letters,n, TRUE, prob = 0.1 : 26), collapse = "")) 
       
       
       
       generate.pass(n)
       
       
       
     } else if (isTRUE(input$numbers)){
       n <- input$length
       nums <- c(0:9)
       
       generate.pass<- function(n)
         
         return(paste0(sample(nums,n, TRUE, prob = 0.1 : 10), collapse = "")) 
       
       
       
       generate.pass(n)
       
       
     } else if  (isTRUE(input$special)){
       n <- input$length
       
       punct <- c("!",  "#", "$", "%", "&", "(", ")", "*",  "+", "-", "/", ":", 
                  ";", "<", "=", ">", "?", "@", "[", "^", "_", "{", "|", "}", "~")
       generate.pass<- function(n)
         
         return(paste0(sample(punct,n, TRUE, prob = 0.1 : 25), collapse = "")) 
       
       generate.pass(n)
       
       
       showNotification("not the best choice of password")
       
       
     } else {
       
       n <- input$length
         generate.pass<- function(n)
             
         punct <- c("!",  "#", "$", "%", "&", "(", ")", "*",  "+", "-", "/", ":", 
                        ";", "<", "=", ">", "?", "@", "[", "^", "_", "{", "|", "}", "~")
         nums <- c(0:9)
           
         chars <- c(punct,nums,letters,LETTERS)
           
          return(paste0(sample(chars,n, TRUE, prob = 0.1 : 87), collapse = ""))
           
          generate.pass(n)
     
       
     }
     
     
})
 
  

  
  
   
   
}
}

# Run the application 
shinyApp(ui = ui, server = server)

