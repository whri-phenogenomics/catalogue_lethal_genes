mod_curation_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    tags$h3(
      tags$strong("OMIM querying and curation"),
      align = "center",
      id = "page-subheading"
    )
  ), tags$br(), fluidRow(column(
    5,
    box(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = h4("OMIM Curation", style = "font-size:20px;"),
      tags$p(
        tags$h4(
          style = "font-size:20px;",
          style = "line-height:1.5",
          "OMIM clinical records were data mined using a series of API queries
        of terms linked to lethality. The complete list of terms
        is illustrated in the Figure. Manual curation of all the hits
        was performed and ambiguous reports of lethality were discarded. Each
        unique disease-gene association  was assigned to a ‘lethality category’
        based on the earliest age of death reported, with categories grouped by
        age ranges defined by the HPO age of death terms described in the
        OMIM Catalogue tab."
        )
      ),
      tags$br(),
      tags$p(
        tags$h4(
          style = "font-size:20px;",
          style = "line-height:1.5",
          "Following the manual curation process and exclusion of ambiguous
        entries from OMIM, 57% (2,133/3,773) of genes associated with a
        subset of OMIM disorders: single-gene,with strict Mendelian phenotypes
        and molecular basis known were not captured by the queries. This
        indicates a lack of clinical records pertaining to lethality. Moreover,
        33% (1,239/3,773) are exclusively linked to disorders that have
        documented lethal phenotypes. The remaining 11% (401/3,773) are
        associated with both lethal and non-lethal phenotypes. In terms of
        lethality categories, 975 genes (59% of all lethal genes, 26% of
        all disease genes catalogued), have records of prenatal,
        neonatal or infant death (classified as pre-infant-lethal) as opposed to
        post-infant-lethal genes, where the earliest reported age of death
        spans from childhood to adulthood."
        )
      ),
      tags$br(),
      tags$p(
        tags$h4(
          style = "font-size:20px;",
          style = "line-height:1.5",
          "*Non lethal means no records of early death in OMIM OR
                       not captured by our OMIM API queries."
        )
      )
    )
  ), column(7, box(
    width = 12, tags$div(
      style = "text-align: center;",
      tags$img(
        src = "catalogue_omim.jpg",
        width = 800,
        height = 700
      )
    )
  ))))
}
    
mod_curation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  })
}
