#could be submitButton instead of goButton, check notes from Data Products
shinyUI(
    pageWithSidebar(
        # Application title
        headerPanel("Whatcha gon type next?"),
        sidebarPanel(
            textInput('words', 'You typed' , ""),
            #numericInput('spins', 'Plays on radio in Area', 0),
            #numericInput('likes', 'Likes on Facebook in Area', 0),
            actionButton("goButton", "Go!")
        ),
        mainPanel(
            h3('Whatcha gon type'),
            h4('You gon type'),
            verbatimTextOutput("pick")
            #h4('People who heard you on the radio'),
            #verbatimTextOutput("spinspt"),
            #h4('People who like you on Facebook'),
            #verbatimTextOutput("likespt"),
            #h2('Total Expected Tickets Sales'),
            #verbatimTextOutput("prediction")
        )
    )
)
