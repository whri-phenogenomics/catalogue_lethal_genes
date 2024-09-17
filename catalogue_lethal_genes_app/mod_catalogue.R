literature_intro <- function() {
  fluidRow(tags$p(
    tags$h4(
      "This catalogue of genes was collated through a
      literature review of clinical research papers investigating the
      genetic causes of lethal phenotypes between the
      embryonic-neonatal periods using next-generation sequencing
      methods.",
      align = "left",
      id = "page-text"
    )
  ), tags$i(
    tags$h5(
      "For multiple search terms, use  '|' between the
        words, e.g. biallelic|preterm",
      align = "right"
    )
  ))
}

OMIM_intro <- function() {
  tags$div(fluidRow(tags$p(
    tags$h4(
      style = "font-size:20px;",
      style = "line-height:1.5",
      "This catalogue of genes was curated from OMIM, identifying reports
      of lethality through different fields including 'Description',
      'Clinical Synopsis' and 'Clinical Features'. The entire
        list of search terms can be found in the 'Data Curation' tab.
        We have included all disorders with specific mention to a
        lethal phenotype and assigned each gene-disorder to one of seven
        lethality categories according to the earliest age at which
        death occured according to OMIM records. The age brackets for
        these categories are based on definitions provided by
        Human Phenotype Ontology (HPO) terms.
        The details for these categories are listed below:",
      align = "left",
      id = "page-text"
    )
  )), fluidRow(
    box(
      title = "Lethality categories according to HPO:",
      status = "primary",
      solidHeader = TRUE,
      style = 'font-size:14px',
      #side = "center",
      #height = "30em",
      width = 12,
      tags$ul(
        tags$li("L1: Prenatal death (HP:0034241); Death before birth"),
        tags$ul(
          tags$li(
            "L1.1: Miscariage (HP:0005268); Spontaneous loss of a fetus
                              before the 22th week of pregnancy"
          ),
          tags$li(
            "L1.2: Stillbirth (HP:0003826); Death of the fetus in utero
                              after at least 22 weeks of gestation"
          )
        ),
        tags$li(
          "L2: Neonatal death (HP:0003811); Death within the
                              first 28 days of life"
        ),
        tags$li(
          "L3: Death in infancy (HP:0001522); Death within the
                              first 24 months of life"
        ),
        tags$li(
          "L4: Death in childhood (HP:0003819); Death during childhood,
                              defined here as between the ages of 2 and 10 years"
        ),
        tags$li(
          "L5: Death in adolescence (HP:0011421); Death during adolescence,
                              the period between childhood and adulthood (roughly between
                              the ages of 10 and 19 years)"
        ),
        tags$li(
          "L6: Death in adulthood (HP:0033763); Cessation of life at
                              the age of 16 years or later"
        ),
        # tags$ul(
        #   tags$li("L6.1: Death in early adulthood (HP:0100613); Death between the age
        #                   of 16 and 40 years"),
        #   tags$li("L6.2: Death in middle age (HP:0033764); Death between the age
        #                   of 40 and 60 years"),
        #   tags$li("L6.3: Death in late adulthood (HP:0033765); Death at an age of
        #                   at least 60 years")),
        tags$li("LU: Age of death undetermined"),
        id = "page-text"
      ),
    ),
    # tabbox
  ), #fluid row
  fluidRow(tags$i(
    tags$h5(
      "For multiple search terms, use '|' between the words, e.g. syndrome|demise",
      align = "right"
    )
  )))
}

mod_catalogue_ui <- function(id) {
  ns <- NS(id)
  
  title <- sprintf("Catalogue of lethal phenotypes in humans curated from %s",
                   switch(id, "literature" = "the literature", "OMIM" = "OMIM"))
  
  intro <- switch(id, "literature" = literature_intro, "OMIM" = OMIM_intro)
  
  tagList(
    fluidRow(
      tags$h3(tags$strong(title), align = "center", id = "page-subheading")
    ),
    tags$br(),
    intro(),
    fluidRow(reactableOutput(ns("cat_table"))),
    fluidRow(downloadButton(
      ns("save_button"), sprintf("Download %s catalogue", id)
    ))
  )
}

mod_catalogue_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("read_data.R")
    
    table <- switch(id, "literature" = create_literature_table(), "OMIM" = create_omim_table())
    
    output$cat_table <- renderReactable({
      reactable(
        table,
        rownames = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        striped = TRUE,
        theme = reactableTheme(backgroundColor = "#f7fbff")
      )
    })
    
    output$save_button <- downloadHandler(
      filename = sprintf("%s_catalogue_lethal_genes.csv", id),
      content = function(file) {
        write.csv(table, file, row.names = FALSE)
      }
    )
  })
}
