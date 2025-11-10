# UI
intro_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Introduction",
    
    # Container with padding for narrower text blocks
    div(style = "padding: 20px; max-width: 800px; margin: auto;",
        
        HTML('
          <h1>Danish Residential Housing Prices</h1>
          <p>[add introduction text here]</p>
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
          <p>[insert research questions]</p>
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
