library(shiny)
shinyUI(pageWithSidebar(
# Application title
headerPanel(Tweets hunter),

sidebarPanel( textInput(term, Enter a term, ),
numericInput(cant, Select a number of tweets,1,0,200),
radioButtons(lang,Select the language,c(
English=en,
Castellano=es,
Deutsch=de)),
submitButton(text=Run)),

mainPanel(
h4(Last 5 Tweets),
tableOutput(table),
plotOutput(wordcl))
))