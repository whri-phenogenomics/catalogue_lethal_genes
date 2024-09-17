mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    tags$h2(
      tags$strong("Lethal Phenotypes Portal"),
      style = "font-size:40px;",
      align = "center",
      id = ns("page-title")
    )
  ),
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      tags$p(
        tags$h4(
          tags$strong(("About")),
          style = "font-size:25px;",
          align = "left",
          id = "page-text"
        )
      ),
      tags$p(
        tags$h4(
          style = "font-size:20px;",
          style = "line-height:1.5",
          "The Lethal Phenotypes Portal is a curated online
        resource containing a catalogue of genes associated to lethal phenotypes
        in humans. More detailed information regarding the contents of this
        resource and the methods that were used to collate the information
        presented here can be found in the 'Database
        Content' and 'Data Curation' tabs.",
          align = "left",
          id = "page-text"
        )
      ),
      tags$br(),
      fluidRow(
        valueBoxOutput(ns("Disease_Lethal_Phenotypes_Box"), width = 4),
        valueBoxOutput(ns(
          "Disease_Early_Lethal_Phenotypes_Box"
        ), width = 4),
        valueBoxOutput(ns(
          "Disease_Mouse_Lethal_Phenotypes_Box"
        ), width = 4)
      )
    )
  ),
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      fluidRow(column(8, tags$p(
        tags$h4(
          tags$strong(("Gene summary")),
          style = "font-size:25px;",
          align = "left",
          id = "page-text"
        )
      ), #tags$p(tags$h4(style = "font-size:20px;",
      #"Gene based search to display a summary of the information
      #contained on the catalogue.",
      #               align = "left", id = "page-text")),
      #tags$p(tags$h4(style = "font-size:20px;",
      #               "Type the gene symbol in the Search Gene box.",
      #               align = "left", id = "page-text")),
      tags$p(
        tags$h4(
          style = "font-size:20px;",
          "Gene based search to display a summary of the information
    contained on the catalogue.
    Type the gene symbol in the Search Gene box.
    For more detailed information and access to the entire
    catalogue, go to the 'OMIM Catalogue' tab.",
          align = "left",
          id = "page-text"
        )
      )), column(
        4, box(
          title = "Search Gene",
          status = "warning",
          textInput(
            inputId = ns("gene_search"),
            label = NULL,
            placeholder = "FGFR2",
            width = 200
          )
        )
      ), ),
      tags$br(),
      fluidRow(
        valueBoxOutput(ns("Gene_in_Catalogue_Box"), width = 4),
        valueBoxOutput(ns("Lethality_Category_Box"), width = 4),
        valueBoxOutput(ns("Earliest_Lethaliy_Category_Box"), width = 4),
        valueBoxOutput(ns("Mouse_Orthologue_Box"), width = 4),
        valueBoxOutput(ns("Cell_Essentiality_Box"), width = 4),
        valueBoxOutput(ns("Gene_Constraint_Box"), width = 4)
        
      )
    )
  ),
  fluidRow(tags$p(
    tags$h4(
      "Last updated 11.01.2024. For  more information please contact us at",
      tags$a(href = "https://whri-phenogenomics.github.io/", "https://whri-phenogenomics.github.io/"),
      style = "font-size:20px;",
      align = "left",
      id = "page-text"
    )
  ), ))
  
}


    
mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("read_data.R")
    
    omim_curation <- load_omim()
    
    gene_annotations <- load_gene()
    
    
    ## total number of genes with lethal phenotypes in OMIM
    number_omimlethal_genes <- omim_curation %>%
      filter(earliest_lethality_category %in% c("L1", "L2", "L3", "L4", "L5", "L6", "LU")) %>%
      summarise(number_omim_lethal_genes = length(unique(hgnc_id)))
    
    ## total number of genes with early lethal phentoypes in OMIM (L1,L2,L3)
    number_omimearlylethal_genes <- omim_curation %>%
      filter(earliest_lethality_category %in% c("L1", "L2", "L3")) %>%
      summarise(number_omim_lethal_genes = length(unique(hgnc_id)))
    
    
    ## total number of OMIM genes with mouse lethal prototypes
    number_omimmouselethalgenes <- omim_curation %>%
      inner_join(gene_annotations, by = c("hgnc_id" = "hgnc_id")) %>%
      filter(viability_mgi == "lethal" |
               viability_impc %in% c("lethal", "subviable")) %>%
      summarise(number_omim_mouse_lethal_genes = length(unique(hgnc_id)))
    
    
    ## box for the number of OMIM lethal genes curated
    output$Disease_Lethal_Phenotypes_Box <- renderValueBox({
      valueBox(
        number_omimlethal_genes,
        "Genes with lethal phenotypes curated from OMIM",
        icon = icon("box-archive", lib = "font-awesome"),
        color = "light-blue"
      )
    })
    
    ## box for the number of OMIM early lethal genes curated
    output$Disease_Early_Lethal_Phenotypes_Box <- renderValueBox({
      valueBox(
        number_omimearlylethal_genes,
        "Genes with early lethal phenotypes curated from OMIM",
        icon = icon("filter", lib = "font-awesome"),
        color = "light-blue"
        
      )
    })
    
    
    ## box for the number of OMIM genes with mouse lethal phenotypes
    output$Disease_Mouse_Lethal_Phenotypes_Box <- renderValueBox({
      valueBox(
        number_omimmouselethalgenes,
        "OMIM disease genes with mouse lethal phenotypes",
        icon = icon("code-merge", lib = "font-awesome"),
        color = "light-blue"
      )
    })
    
    gene_selected <- reactive({
      omim_curation %>%
        filter(gene_symbol %in% c(toupper(as.character(
          input$gene_search
        )))) %>%
        select(gene_symbol,
               gene_lethal_summary,
               earliest_lethality_category) %>%
        distinct()
      
    })
    
    
    gene_selected_annotations <- reactive({
      omim_curation %>%
        filter(gene_symbol %in% c(toupper(as.character(
          input$gene_search
        )))) %>%
        select(hgnc_id,
               gene_symbol,
               gene_lethal_summary,
               earliest_lethality_category) %>%
        distinct() %>%
        inner_join(gene_annotations, by = c("hgnc_id" = "hgnc_id")) %>%
        mutate(mean_depmap_gene_effect_score =
                 round(mean_depmap_gene_effect_score, 3)) %>%
        mutate(oe_lof =
                 round(lof_oe, 3)) %>%
        mutate(shet_rgcme_mean =
                 round(shet_rgcme_mean, 3))
      
    })
    
    
    ## box for gene summary: gene in catalogue
    output$Gene_in_Catalogue_Box <- renderValueBox({
      valueBox(
        "Gene in Catalogue",
        ifelse(
          dim(gene_selected())[1] == 0,
          "Gene not in catalogue",
          "Gene in catalogue"
        ),
        color = "blue"
      )
    })
    
    ## box for gene summary: lethality category
    output$Lethality_Category_Box <- renderValueBox({
      valueBox(
        "Lethality Category",
        ifelse(
          dim(gene_selected())[1] == 0,
          "-",
          gene_selected()$gene_lethal_summary
        ),
        color = "blue"
      )
    })
    
    
    ## box for gene summary: earliest lethality category
    output$Earliest_Lethaliy_Category_Box <- renderValueBox({
      valueBox(
        "Earliest age of death",
        ifelse(
          dim(gene_selected())[1] == 0,
          "-",
          gene_selected()$earliest_lethality_category
        ),
        color = "blue"
      )
    })
    
    ## box for gene summary: mouse viability
    output$Mouse_Orthologue_Box <- renderValueBox({
      valueBox("Mouse viability", ifelse(
        dim(gene_selected())[1] == 0,
        "-",
        paste0(
          "IMPC",
          "  ",
          gene_selected_annotations()$viability_impc,
          " | ",
          "MGI",
          "  ",
          gene_selected_annotations()$viability_mgi
        )
      ), color = "aqua")
    })
    
    ## box for gene summary: cell essentiality
    output$Cell_Essentiality_Box <- renderValueBox({
      valueBox("Cell essentiality",
               ifelse(
                 dim(gene_selected())[1] == 0,
                 "-",
                 paste0(
                   "Depmap Gene Effect",
                   "   ",
                   gene_selected_annotations()$mean_depmap_gene_effect_score
                 )
               ),
               color = "aqua")
    })
    
    
    ## box for gene summary: gene constraint
    output$Gene_Constraint_Box <- renderValueBox({
      valueBox("Gene constraint", ifelse(
        dim(gene_selected())[1] == 0,
        "-",
        paste0(
          "OE LOF",
          "  ",
          gene_selected_annotations()$lof_oe,
          " | ",
          "Shet RGC-ME",
          "  ",
          gene_selected_annotations()$shet_rgcme_mean
        )
      ), color = "aqua")
    })
    
    
  })
}
