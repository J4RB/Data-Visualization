# UI
intro_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Introduction",
    
    # Container with padding for narrower text blocks
    div(style = "padding: 20px; max-width: 800px; margin: auto;",
        
        HTML('
          <h1>Danish Residential Housing Prices</h1>
          <p>Understanding how housing prices evolve over time is essential for anyone navigating the real estate market, whether as a prospective homeowner, investor, or policymaker. In this project, we explore the dynamics of Danish residential housing prices from 1992 to 2024, using a comprehensive dataset that captures both national trends and regional differences.</p>
          <p>Through a series of interactive geographic and graphical visualizations, this project seeks to transform raw data into meaningful insights. Our goal is not only to analyze long-term fluctuations, but also to shed light on practical questions faced by buyers and analysts today, such as which property types dominate the market, how prices differ across regions, and whether negotiation plays a significant role in final sale outcomes.</p>
        '),
        
        # Team Members
        HTML('
          <h3>Team Members</h3>
          <p>Group 16 consists of the following members:</p>
          <ul>
            <li>Platon Dimitriadis</li>
            <li>Md Al Imran Khan</li>
            <li>Jacob Bregndahl Larsen</li>
          </ul>
        '),
        
        # Data
        HTML('
          <h3>Data</h3>
          <p>
            The dataset used in this project comes from Kaggle and is called
            <a href="https://www.kaggle.com/datasets/martinfrederiksen/danish-residential-housing-prices-1992-2024" target="_blank">
              “Danish Residential Housing Prices 1992–2024” by Martin Frederiksen.
            </a>
          </p>
          <p>For this project, we used the one hundred thousand sample version of the dataset.</p>
        '),
        
        # Research Questions
        HTML('
          <h3>Research Questions</h3>
          <p>In the following tabs, a number of visualizations will be used to answer the below research questions.</p>
          <ul>
            <li>What are the most popular types of houses on sales in Denmark? </li>
            <li>What is the most purchased type of houses in Denmark in the past decades? </li>
            <li>Does the house purchasing uprising or downsizing through the years in Denmark? </li>
            <li>Does the number of rooms effect the sales of houses in Denmark and if yes then how? </li>
            <li>What was the house price index through the years? </li>
            <li>How do square-meter prices vary by area for Danish residential housing? </li>
            <li>Does the negotiation play a crucial part when purchasing a house in Denmark?</li>
          </ul>
        '),
        
        # Report
        HTML('
          <h3>Report</h3>
          <p>Click the button below to download the project report:</p>
        '),
        downloadButton(ns("download_pdf"), "Download Report")
    )
  )
}

# Server
intro_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        "report-group-16.pdf"
      },
      content = function(file) {
        file.copy("Documents/Report/report.pdf", file)
      },
      contentType = "application/pdf"
    )
  })
}
