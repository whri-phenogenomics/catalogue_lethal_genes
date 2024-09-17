mod_content_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$h3(
        tags$strong("Content of the database"),
        align = "center",
        id = "page-subheading"
      )
    ),
    tags$br(),
    fluidRow(column(
      6,
      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = h4("Data Curation", style = "font-size:20px;"),
        tags$p(
          tags$h4(
            style = "font-size:20px;",
            style = "line-height:1.5",
            "Contains a brief description of the methods used in the data curation
        process when creating this app. These include the search terms used as
        part of the OMIM query strategy and subsequent manual curation to
        identify genes associated to lethal phenotypes in humans, the criteria
        used to assign genes to different lethality categories, and summary
        information on the resulting dataset."
          )
        ),
        tags$a(href = "https://www.omim.org/", "OMIM")
      )
    ), column(
      6,
      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = h4("OMIM Catalogue", style = "font-size:20px;"),
        tags$p(
          tags$h4(
            style = "font-size:20px;",
            style = "line-height:1.5",
            "Data in tabular format available for download that displays information
          on all the gene-disease pairs curated from OMIM records, whether they
          were identifed as lethal hits through our queries or not, the source of
          evidence (proband versus affected family members), and gene summary
          information on the presence of lethal phenotypes and the earliest age of
          death recorded. HPO age of death categories and definitions are also
          listed here."
          )
        ),
        tags$a(href = "https://hpo.jax.org", "HPO")
      )
    )),
    fluidRow(column(
      6,
      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = h4("Mouse Evidence", style = "font-size:20px;"),
        tags$p(
          tags$h4(
            style = "font-size:20px;",
            style = "line-height:1.5",
            "Mouse evidence on lethality/viability for those genes with a
          one-to-one human ortholog included in the catalogue. Information
          on lethality from mouse knockout lines as collected by the
          Mouse Genome Informatics Database (MGI) and the International
          Mouse Phenotyping Consortium (IMPC, DR 20.1). Human cell
          proliferation scores from the 'Gene Effect Score' tab are also
          included to compare essentiality at the cellular level."
          )
        ),
        tags$a(href = "https://www.mousephenotype.org/", "IMPC"),
        tags$a(href = "https://www.informatics.jax.org/", "MGI")
      )
    ), column(
      6,
      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = h4("LOEUF Score", style = "font-size:20px;"),
        tags$p(
          tags$h4(
            style = "font-size:20px;",
            style = "line-height:1.5",
            "Displays the distribution of gnomAD v4.0 loss-of-function (LoF)
              observed/expected upper bound fraction (LOEUF) scores.Lower LOEUF
              values indicate a higher probability of being intolerant to
              heterozygous LoF variation. A threshold of 0.35 is suggested by
              the authors."
          )
        ),
        tags$a(href = "https://gnomad.broadinstitute.org/", "gnomAD")
      )
    )),
    fluidRow(column(
      6,
      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = h4("Shet Score", style = "font-size:20px;"),
        tags$p(
          tags$h4(
            style = "font-size:20px;",
            style = "line-height:1.5",
            "Displays the distribution of RGC-ME mean Shet scores,
                                a selection coefficient on relative fitness loss due
                                to heterozygous pLOF variation. A suggested shet cutoff
                                > 0.075 can be used to identify highly constrained genes."
          )
        ),
        tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/37214792/", "Shet publication")
      )
    ), column(
      6,
      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = h4("Gene Effect Score", style = "font-size:20px;"),
        tags$p(
          tags$h4(
            style = "font-size:20px;",
            style = "line-height:1.5",
            "Displays the distribution of mean DepMap 23Q4 CRISPR Gene Effect score
                  which were derived from cancer cell lines as part of the Broad
                   Institute's Dependency Map. Lower Gene Effect scores indicate
                  higher essentiality at the cellular level. A threshold of -0.5 was
                  used to categorise genes as 'Cellular Essential' (<-0.5)."
          )
        ),
        tags$a(href = "https://depmap.org/portal/", "DepMap")
        
      )
    ))
    
  )
}

mod_content_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  })
}
