mod_mouse_evidence_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$h3(
        tags$strong("Mouse Evidence for Pre-infant lethal genes"),
        align = "center",
        id = "page-subheading"
      )
    ),
    tags$br(),
    fluidRow(tags$p(
      tags$h4(
        style = "font-size:20px;",
        style = "line-height:1.5",
        "Mouse viability data was obtained from two
      different sources of evidence: 1) Primary viability screen
      performed on knockout lines by the International Mouse
      Phenotyping Consortium (IMPC), and 2) Evidence of
      lethal phenotypes in knockout mouse as collected from the
      literature in the Mouse Genomes Informatics
      (MGI) resource.",
        align = "left",
        id = "page-text"
      )
    )),
    fluidRow(
      box(
        plotlyOutput(ns("MGI_Sunburst")),
        height = 400,
        width = 6,
        background = "light-blue"
      ),
      box(
        plotlyOutput(ns("IMPC_Sunburst")),
        height = 400,
        width = 6,
        background = "light-blue"
      ),
    ),
    br(),
    fluidRow(
      box(title = "Mouse Evidence for OMIM Catalogue genes", width = 12, reactableOutput(ns("mouse_table")))
    ),
    fluidRow(downloadButton(
      ns("save_button"), "Download mouse evidence"
    )),
    br(),
    fluidRow(
      box(
        plotlyOutput(ns("plotly_MGI")),
        height = 400,
        title = "MGI",
        width = 6
      ),
      box(
        plotlyOutput(ns("plotly_IMPC")),
        height = 400,
        title = "IMPC",
        width = 6
      )
    )
  )
}
    
mod_mouse_evidence_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("read_data.R")
    
    omim_annotations <- load_omim()
    
    gene_annotations <- load_gene()
    
    omim_annotations_l1l2l3 <- omim_annotations %>%
      filter(merged_earliest_lethality_category  == "pre-infant-lethal") %>%
      select(hgnc_id) %>%
      distinct()
    
    MOUSE_REF <- gene_annotations %>%
      filter(hgnc_id %in% unique(omim_annotations$hgnc_id)) %>%
      filter(!hgnc_id %in% omim_annotations_l1l2l3$hgnc_id) %>%
      select(hgnc_id, viability_mgi, viability_impc)
    
    MOUSE_REF$REF_OR_CAT <- "Other"
    
    MOUSE_CAT <- gene_annotations %>%
      filter(hgnc_id %in% omim_annotations_l1l2l3$hgnc_id) %>%
      select(hgnc_id, viability_mgi, viability_impc)
    
    MOUSE_CAT$REF_OR_CAT <- "pre-infant-lethal"
    
    MOUSE_ALL <- rbind(MOUSE_REF, MOUSE_CAT)
    MOUSE_ALL[MOUSE_ALL == "-"] <- "unknown"
    
    #MGI Values
    MGI_Viable_Ref <- nrow(filter(MOUSE_ALL, viability_mgi == "viable" &
                                    REF_OR_CAT == "Other"))
    MGI_Lethal_Ref <- nrow(filter(MOUSE_ALL, viability_mgi == "lethal" &
                                    REF_OR_CAT == "Other"))
    MGI_Unknown_Ref <- nrow(filter(MOUSE_ALL, viability_mgi == "unknown" &
                                     REF_OR_CAT == "Other"))
    MGI_Viable_Cat <- nrow(filter(
      MOUSE_ALL,
      viability_mgi == "viable" &
        REF_OR_CAT == "pre-infant-lethal"
    ))
    MGI_Lethal_Cat <- nrow(filter(
      MOUSE_ALL,
      viability_mgi == "lethal" &
        REF_OR_CAT == "pre-infant-lethal"
    ))
    MGI_Unknown_Cat <- nrow(filter(
      MOUSE_ALL,
      viability_mgi == "unknown" &
        REF_OR_CAT == "pre-infant-lethal"
    ))
    #IMPC Values
    IMPC_Viable_Ref <- nrow(filter(MOUSE_ALL, viability_impc == "viable" &
                                     REF_OR_CAT == "Other"))
    IMPC_Subviable_Ref <- nrow(filter(
      MOUSE_ALL,
      viability_impc == "subviable" &
        REF_OR_CAT == "Other"
    ))
    IMPC_Lethal_Ref <- nrow(filter(MOUSE_ALL, viability_impc == "lethal" &
                                     REF_OR_CAT == "Other"))
    IMPC_Unknown_Ref <- nrow(filter(
      MOUSE_ALL,
      viability_impc == "unknown" &
        REF_OR_CAT == "Other"
    ))
    IMPC_Viable_Cat <- nrow(filter(
      MOUSE_ALL,
      viability_impc == "viable" &
        REF_OR_CAT == "pre-infant-lethal"
    ))
    IMPC_Subviable_Cat <- nrow(filter(
      MOUSE_ALL,
      viability_impc == "subviable" &
        REF_OR_CAT == "L1L2L3"
    ))
    IMPC_Lethal_Cat <- nrow(filter(
      MOUSE_ALL,
      viability_impc == "lethal" &
        REF_OR_CAT == "pre-infant-lethal"
    ))
    IMPC_Unknown_Cat <- nrow(
      filter(
        MOUSE_ALL,
        viability_impc == "unknown" &
          REF_OR_CAT == "pre-infant-lethal"
      )
    )
    #Total Values
    MGI_Cat <- sum(MGI_Viable_Cat, MGI_Lethal_Cat, MGI_Unknown_Cat)
    MGI_Ref <- sum(MGI_Viable_Ref, MGI_Lethal_Ref, MGI_Unknown_Ref)
    MGI_Total <- sum(MGI_Cat, MGI_Ref)
    IMPC_Cat <- sum(IMPC_Viable_Cat,
                    IMPC_Subviable_Cat,
                    IMPC_Lethal_Cat,
                    IMPC_Unknown_Cat)
    IMPC_Ref <- sum(IMPC_Viable_Ref,
                    IMPC_Subviable_Ref,
                    IMPC_Lethal_Ref,
                    IMPC_Unknown_Ref)
    IMPC_Total <- sum(IMPC_Cat, IMPC_Ref)
    
    ##SUNBURST CHARTS
    
    #MGI MOUSE
    MGI_df <- data.frame(
      labels = c(
        "Total",
        "pre-infant-lethal",
        "Other",
        "Viable",
        "Lethal",
        "Unknown",
        "Viable",
        "Lethal",
        "Unknown"
      ),
      values = c(
        MGI_Total,
        MGI_Cat,
        MGI_Ref,
        MGI_Viable_Cat,
        MGI_Lethal_Cat,
        MGI_Unknown_Cat,
        MGI_Viable_Ref,
        MGI_Lethal_Ref,
        MGI_Unknown_Ref
      ),
      parents = c(
        " ",
        "Total",
        "Total",
        "Total - pre-infant-lethal",
        "Total - pre-infant-lethal",
        "Total - pre-infant-lethal",
        "Total - Other",
        "Total - Other",
        "Total - Other"
      ),
      ids = c(
        "Total",
        "Total - pre-infant-lethal",
        "Total - Other",
        "Total - pre-infant-lethal - Viable",
        "Total - pre-infant-lethal - Lethal",
        "Total - pre-infant-lethal - Unknown",
        "Total - Other - Viable",
        "Total - Other - Lethal",
        "Total - Other - Unknown"
      ),
      colors = c(
        "#ADE8F4",
        "#90E0EF",
        "#48CAE4",
        "#00B4D8",
        "#0096C7",
        "#0077B6",
        "#00B4D8",
        "#0096C7",
        "#0077B6"
      )
    )
    
    #IMPC MOUSE
    IMPC_df <- data.frame(
      labels = c(
        "Total",
        "pre-infant-lethal",
        "Other",
        "Viable",
        "Subviable",
        "Lethal",
        "Unknown",
        "Viable",
        "Subviable",
        "Lethal",
        "Unknown"
      ),
      values = c(
        IMPC_Total,
        IMPC_Cat,
        IMPC_Ref,
        IMPC_Viable_Cat,
        IMPC_Subviable_Cat,
        IMPC_Lethal_Cat,
        IMPC_Unknown_Cat,
        IMPC_Viable_Ref,
        IMPC_Subviable_Ref,
        IMPC_Lethal_Ref,
        IMPC_Unknown_Ref
      ),
      parents = c(
        " ",
        "Total",
        "Total",
        "Total - pre-infant-lethal",
        "Total - pre-infant-lethal",
        "Total - pre-infant-lethal",
        "Total - pre-infant-lethal",
        "Total - Other",
        "Total - Other",
        "Total - Other",
        "Total - Other"
      ),
      ids = c(
        "Total",
        "Total - pre-infant-lethal",
        "Total - Other",
        "Total - pre-infant-lethal - Viable",
        "Total - pre-infant-lethal - Subviable",
        "Total - pre-infant-lethal - Lethal",
        "Total - pre-infant-lethal - Unknown",
        "Total - Other - Viable",
        "Total - Other - Subviable",
        "Total - Other - Lethal",
        "Total - Other - Unknown"
      ),
      colors = c(
        "#ADE8F4",
        "#90E0EF",
        "#48CAE4",
        "#00B4D8",
        "#0096C7",
        "#0077B6",
        "#023e8a",
        "#00B4D8",
        "#0096C7",
        "#0077B6",
        "#023E8A"
      )
    )
    mouse_evidence_table <- omim_annotations %>%
      select(
        hgnc_id,
        gene_lethal_summary,
        earliest_lethality_category,
        merged_earliest_lethality_category
      ) %>%
      distinct() %>%
      inner_join(gene_annotations, by = c("hgnc_id" = "hgnc_id")) %>%
      select(
        hgnc_id,
        gene_symbol,
        gene_lethal_summary,
        earliest_lethality_category,
        merged_earliest_lethality_category,
        viability_mgi,
        viability_impc,
        mean_depmap_gene_effect_score,
        depmap_essential_05
      ) %>%
      distinct() %>%
      mutate(mean_depmap_gene_effect_score = round(mean_depmap_gene_effect_score, 3)) %>%
      rename(
        'Gene symbol' = gene_symbol,
        'HGNC id' = hgnc_id,
        'Gene lethality summary' = gene_lethal_summary,
        'Gene earliest lethality category' = earliest_lethality_category,
        'Grouped gene earliest lethality category' = merged_earliest_lethality_category,
        'Mouse viability MGI' = viability_mgi,
        'Mouse viability IMPC' = viability_impc,
        'Human cell CRISPR gene effect DepMap' = mean_depmap_gene_effect_score,
        'Cell core essential' = depmap_essential_05
      ) %>%
      distinct() %>%
      mutate(
        category = ifelse(
          `Grouped gene earliest lethality category`  ==
            "pre-infant-lethal",
          "OMIM pre-infant-lethal",
          ifelse(
            `Grouped gene earliest lethality category` ==
              "post-infant-lethal",
            "OMIM post-infant-lethal",
            "OMIM non-lethal"
          )
        )
      )
    
    impc_tally <- mouse_evidence_table %>%
      mutate(
        `Mouse viability IMPC` = ifelse(
          `Mouse viability IMPC` == "subviable",
          "lethal",
          `Mouse viability IMPC`
        )
      ) %>%
      group_by(category, `Mouse viability IMPC`) %>%
      tally() %>%
      filter(`Mouse viability IMPC` != "-") %>%
      arrange(`Mouse viability IMPC`, category)
    
    mgi_tally <- mouse_evidence_table %>%
      group_by(category, `Mouse viability MGI`) %>%
      tally() %>%
      filter(`Mouse viability MGI` != "-") %>%
      arrange(`Mouse viability MGI`, category)
    
    IMPC <- data.frame(
      OMIM_Category = c(
        "OMIM non-lethal",
        "OMIM pre-infant-lethal",
        "OMIM post-infant-lethal"
      ),
      Lethal = impc_tally$n[1:3],
      Viable = impc_tally$n[4:6]
    ) %>%
      mutate(OMIM_Category = factor(
        OMIM_Category,
        levels = c(
          "OMIM pre-infant-lethal",
          "OMIM post-infant-lethal",
          "OMIM non-lethal"
        )
      ))
    
    MGI <- data.frame(
      OMIM_Category = c(
        "OMIM non-lethal",
        "OMIM pre-infant-lethal",
        "OMIM post-infant-lethal"
      ),
      Lethal = mgi_tally$n[1:3],
      Viable = mgi_tally$n[4:6]
    ) %>%
      mutate(OMIM_Category = factor(
        OMIM_Category,
        levels = c(
          "OMIM pre-infant-lethal",
          "OMIM post-infant-lethal",
          "OMIM non-lethal"
        )
      ))
    
    output$MGI_Sunburst <- renderPlotly({
      plot_ly(
        data = MGI_df,
        ids = ~ ids,
        labels = ~ labels,
        parents = ~ parents,
        values = ~ values,
        type = 'sunburst',
        branchvalues = 'total',
        marker = list(colors = ~ colors)
      ) %>% layout(
        title = "Viability Breakdown (MGI)",
        paper_bgcolor = "#f7fbff",
        plot_bgcolor = "#f7fbff"
      )
    })
    
    output$IMPC_Sunburst <- renderPlotly({
      plot_ly(
        data = IMPC_df,
        ids = ~ ids,
        labels = ~ labels,
        parents = ~ parents,
        values = ~ values,
        type = 'sunburst',
        branchvalues = 'total',
        marker = list(colors = ~ colors)
      ) %>% layout(
        title = "Viability Breakdown (IMPC)",
        paper_bgcolor = "#f7fbff",
        plot_bgcolor = "#f7fbff"
      )
    })
    
    output$mouse_table <- renderReactable({
      reactable(
        mouse_evidence_table,
        rownames = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        striped = TRUE,
        theme = reactableTheme(backgroundColor = "#f7fbff")
      )
    })
    
    output$save_button <- downloadHandler(
      filename = "catalogue_lethal_genes_mouse_evidence.csv",
      content = function(file) {
        write.csv(mouse_evidence_table, file, row.names = FALSE)
      }
    )
    
    
    
    output$plotly_MGI <- renderPlotly({
      plot_ly(
        MGI,
        x = ~ OMIM_Category,
        y = ~ Lethal,
        type = "bar",
        hovertemplate = "Number of Genes: %{y} <extra></extra>",
        name = "Lethal",
        marker = list(
          color = "rgb(0, 119, 182)",
          line = list(color = "rgb(202, 240, 248)", width = 1)
        )
      ) %>%
        add_trace(
          y = ~ Viable,
          name = "Viable",
          marker = list(
            color = "rgb(72, 202, 228)",
            line = list(color = "rgb(202, 240, 248)", width = 1)
          )
        ) %>%
        layout(
          title = "MGI",
          barmode = "group",
          xaxis = list(title = "OMIM Lethality Category"),
          yaxis = list(title = "Number of Genes"),
          paper_bgcolor = "rgb(247,251,255)",
          plot_bgcolor = "rgb(247,251,255)"
        )
    })
    
    output$plotly_IMPC <- renderPlotly({
      plot_ly(
        IMPC,
        x = ~ OMIM_Category,
        y = ~ Lethal,
        type = "bar",
        hovertemplate = "Number of Genes: %{y} <extra></extra>",
        name = "Lethal",
        marker = list(
          color = "rgb(0, 119, 182)",
          line = list(color = "rgb(202, 240, 248)", width = 1)
        )
      ) %>%
        add_trace(
          y = ~ Viable,
          name = "Viable",
          marker = list(
            color = "rgb(72, 202, 228)",
            line = list(color = "rgb(202, 240, 248)", width = 1)
          )
        ) %>%
        layout(
          title = "IMPC",
          barmode = "group",
          xaxis = list(title = "OMIM Lethality Category"),
          yaxis = list(title = "Number of Genes", range = c(1, 1000)),
          paper_bgcolor = "rgb(247,251,255)",
          plot_bgcolor = "rgb(247,251,255)"
        )
    })
    
  })
}
