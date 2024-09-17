LOEUF_desc <- "The histograms and violin plots show the distribution of gnomAD's
loss-of-function (LoF) observed/expected upper bound fraction (LOEUF) scores.
Lower LOEUF values indicate a higher probability of being intolerant to heterozygous
LoF variation. A threshold LOEUF < 0.35 is suggested by the authors to identify constrained genes.
The dropdown menu allows the selection of different lethality categories and modes
of inheritance for comparison."

Shet_desc <- "The histograms and violin plots show the distribution of RGC-ME mean Shet score, a selection coefficient on relative fitness loss due to heterozygous pLOF variation.
A suggested shet cutoff shet > 0.075 can be used to identify highly constrained genes.
The dropdown menu allows the selection of different lethality categories and modes
of inheritance for comparison."

DepMap_desc <- "Displays the distribution of mean Gene Effect scores
        which were derived from cancer cell lines as part of the Broad
        Institute's Dependency Map. Lower Gene Effect scores indicate
        higher essentiality at the cellular level. A threshold of -0.5 was
        used to categorise genes as 'Cellular Essential' (<-0.5). The dropdown menu
        allows the selection of different  lethality categories and modes of
        inheritance for comparison."

mod_score_ui <- function(id) {
  ns <- NS(id)
  
  heading <- sprintf("%s Comparisons", id)
  
  description = switch(id,
                       "LOEUF" = LOEUF_desc,
                       "Shet" = Shet_desc,
                       "DepMap" = DepMap_desc)
  tagList(
    tags$h3(tags$strong(heading), align = "center", id = "page-subheading"),
    tags$br(),
    tags$p(
      tags$h4(
        description,
        align = "left",
        id = "page-text",
        style = "font-size:20px;",
        style = "line-height:1.5"
      )
    ),
    fluidRow(column(4, box(
      width = 10,
      selectInput(
        ns("lethality_category_1"),
        "Select lethality category (1):",
        choices = c(
          "L1 (prenatal)",
          "L1 (prenatal) Autosomal Dominant",
          "L1 (prenatal) Autosomal Recessive",
          "L2 (neonatal)",
          "L2 (neonatal) Autosomal Dominant",
          "L2 (neonatal) Autosomal Recessive",
          "L3 (infant)",
          "L3 (infant) Autosomal Dominant",
          "L3 (infant) Autosomal Recessive",
          "L4 (childhood)",
          "L4 (childhood) Autosomal Dominant",
          "L4 (childhood) Autosomal Recessive",
          "L5 (adolescence)",
          "L5 (adolescence) Autosomal Dominant",
          "L5 (adolescence) Autosomal Recessive",
          "L6 (adulthood)",
          "L6 (adulthood) Autosomal Dominant",
          "L6 (adulthood) Autosomal Recessive",
          "LU (undetermined)",
          "LU (undetermined) Autosomal Dominant",
          "LU (undetermined) Autosomal Recessive",
          "NL (non-lethal)",
          "NL (non-lethal) Autosomal Dominant",
          "NL (non-lethal) Autosomal Recessive",
          "NC (not-in-catalogue)"
        ),
        selected = "L1 (prenatal)"
      )
    )), column(4, box(
      width = 10,
      selectInput(
        ns("lethality_category_2"),
        "Select lethality category (2):",
        choices = c(
          "L1 (prenatal)",
          "L1 (prenatal) Autosomal Dominant",
          "L1 (prenatal) Autosomal Recessive",
          "L2 (neonatal)",
          "L2 (neonatal) Autosomal Dominant",
          "L2 (neonatal) Autosomal Recessive",
          "L3 (infant)",
          "L3 (infant) Autosomal Dominant",
          "L3 (infant) Autosomal Recessive",
          "L4 (childhood)",
          "L4 (childhood) Autosomal Dominant",
          "L4 (childhood) Autosomal Recessive",
          "L5 (adolescence)",
          "L5 (adolescence) Autosomal Dominant",
          "L5 (adolescence) Autosomal Recessive",
          "L6 (adulthood)",
          "L6 (adulthood) Autosomal Dominant",
          "L6 (adulthood) Autosomal Recessive",
          "LU (undetermined)",
          "LU (undetermined) Autosomal Dominant",
          "LU (undetermined) Autosomal Recessive",
          "NL (non-lethal)",
          "NL (non-lethal) Autosomal Dominant",
          "NL (non-lethal) Autosomal Recessive",
          "NC (not-in-catalogue)"
        ),
        selected = "NL (non-lethal)"
      )
    )), ),
    fluidRow(column(
      6,
      box(
        plotlyOutput(ns("hist")),
        height = 400,
        width = 12,
        background = 'light-blue'
      )
    ), column(
      6,
      box(
        plotlyOutput(ns("violin")),
        height = 400,
        width = 12,
        background = 'light-blue'
      )
    ), fluidRow(column(
      6, box(
        sliderInput(
          ns("bins"),
          "Select bin size:",
          min = 0.1,
          max = 1,
          value = 0.5,
          step = 0.1
        ),
        height = 150,
        width = 12
      )
    )))
  )
  
}


LOEUF_ref_cat <- function() {
  gene_annotations <- load_gene()
  
  omim_annotations <- load_omim()
  
  omim_annotations_moi  <- omim_annotations %>%
    select(hgnc_id, mode_of_inheritance) %>%
    group_by(hgnc_id) %>%
    summarise(moin = paste0(unique(mode_of_inheritance), collapse = "|")) %>%
    mutate(moi = ifelse(
      moin == "Autosomal dominant",
      "Autosomal Dominant",
      ifelse(
        moin == "Autosomal recessive",
        "Autosomal Recessive",
        ifelse(
          grepl("dominant", moin) &
            grepl("recessive", moin),
          "AD/AR",
          "other"
        )
      )
    )) %>%
    distinct() %>%
    ungroup()
  
  
  omim_annotations_cat <- omim_annotations %>%
    filter(gene_lethal_summary %in% c("lethal", "lethal|nonlethal", "nonlethal")) %>%
    select(hgnc_id, gene_lethal_summary, earliest_lethality_category) %>%
    distinct() %>%
    mutate(
      lethality = ifelse(
        gene_lethal_summary == "nonlethal",
        "NL",
        earliest_lethality_category
      )
    ) %>%
    select(hgnc_id, lethality) %>%
    distinct() %>%
    mutate(
      lethality = recode(
        lethality,
        L1 = "L1 (prenatal)",
        L2 = "L2 (neonatal)",
        L3 = "L3 (infant)",
        L4 = "L4 (childhood)",
        L5 = "L5 (adolescence)",
        L6 = "L6 (adulthood)",
        LU = "LU (undetermined)",
        NL = "NL (non-lethal)"
      )
    ) %>%
    left_join(omim_annotations_moi , by = c("hgnc_id" = "hgnc_id")) %>%
    mutate(lethality_moi = paste0(lethality, " ", moi))
  
  non_omim_annotations_cat <- gene_annotations %>%
    filter(!hgnc_id %in% omim_annotations_cat$hgnc_id) %>%
    mutate(lethality = "NC (not-in-catalogue)") %>%
    mutate(lethality_moi = lethality) %>%
    select(hgnc_id, lethality, lethality_moi)
  
  all_annotations_cat <- omim_annotations_cat %>%
    bind_rows(non_omim_annotations_cat) %>%
    mutate(threshold = 0.35) %>%
    distinct()
  
  
  SCORES_ALL <- all_annotations_cat %>%
    inner_join(gene_annotations, by = c("hgnc_id" = "hgnc_id")) %>%
    filter(!is.na(lof_oe_upper)) %>%
    rename(score = lof_oe_upper) %>%
    select(gene_symbol, score, lethality, lethality_moi, threshold) %>%
    distinct()
  
  
  SCORES_ALL_NO_MOI <- SCORES_ALL %>%
    filter(lethality_moi != "NC (not-in-catalogue)") %>%
    mutate(lethality_moi = lethality)
  
  
  SCORES_ALL_DUP <- SCORES_ALL %>%
    bind_rows(SCORES_ALL_NO_MOI) %>%
    select(gene_symbol, score, lethality_moi, threshold) %>%
    rename(lethality = lethality_moi)
  
  
  return(list(SCORES_ALL_DUP))
  
}

Shet_ref_cat <- function() {
  gene_annotations <- load_gene()
  
  omim_annotations <- load_omim()
  
  omim_annotations_moi  <- omim_annotations %>%
    select(hgnc_id, mode_of_inheritance) %>%
    group_by(hgnc_id) %>%
    summarise(moin = paste0(unique(mode_of_inheritance), collapse = "|")) %>%
    mutate(moi = ifelse(
      moin == "Autosomal dominant",
      "Autosomal Dominant",
      ifelse(
        moin == "Autosomal recessive",
        "Autosomal Recessive",
        ifelse(
          grepl("dominant", moin) &
            grepl("recessive", moin),
          "AD/AR",
          "other"
        )
      )
    )) %>%
    distinct() %>%
    ungroup()
  
  
  
  omim_annotations_cat <- omim_annotations %>%
    filter(gene_lethal_summary %in% c("lethal", "lethal|nonlethal", "nonlethal")) %>%
    select(hgnc_id, gene_lethal_summary, earliest_lethality_category) %>%
    distinct() %>%
    mutate(
      lethality = ifelse(
        gene_lethal_summary == "nonlethal",
        "NL",
        earliest_lethality_category
      )
    ) %>%
    select(hgnc_id, lethality) %>%
    distinct() %>%
    mutate(
      lethality = recode(
        lethality,
        L1 = "L1 (prenatal)",
        L2 = "L2 (neonatal)",
        L3 = "L3 (infant)",
        L4 = "L4 (childhood)",
        L5 = "L5 (adolescence)",
        L6 = "L6 (adulthood)",
        LU = "LU (undetermined)",
        NL = "NL (non-lethal)"
      )
    ) %>%
    left_join(omim_annotations_moi, by = c("hgnc_id" = "hgnc_id")) %>%
    mutate(lethality_moi = paste0(lethality, " ", moi))
  
  non_omim_annotations_cat <- gene_annotations %>%
    filter(!hgnc_id %in% omim_annotations_cat$hgnc_id) %>%
    mutate(lethality = "NC (not-in-catalogue)") %>%
    mutate(lethality_moi = lethality) %>%
    select(hgnc_id, lethality, lethality_moi)
  
  all_annotations_cat <- omim_annotations_cat %>%
    bind_rows(non_omim_annotations_cat) %>%
    mutate(threshold = 0.075)  %>%
    distinct()
  
  SCORES_ALL <- all_annotations_cat %>%
    inner_join(gene_annotations, by = c("hgnc_id" = "hgnc_id")) %>%
    filter(!is.na(shet_rgcme_mean)) %>%
    rename(score = shet_rgcme_mean) %>%
    select(gene_symbol, score, lethality, lethality_moi, threshold) %>%
    distinct()
  
  
  SCORES_ALL_NO_MOI <- SCORES_ALL %>%
    filter(lethality_moi != "NC (not-in-catalogue)") %>%
    mutate(lethality_moi = lethality)
  
  
  SCORES_ALL_DUP <- SCORES_ALL %>%
    bind_rows(SCORES_ALL_NO_MOI) %>%
    select(gene_symbol, score, lethality_moi, threshold) %>%
    rename(lethality = lethality_moi)
  
  
  return(list(SCORES_ALL_DUP))
}


DepMap_ref_cat <- function() {
  gene_annotations <- load_gene()
  
  omim_annotations <- load_omim()
  
  
  omim_annotations_moi  <- omim_annotations %>%
    select(hgnc_id, mode_of_inheritance) %>%
    group_by(hgnc_id) %>%
    summarise(moin = paste0(unique(mode_of_inheritance), collapse = "|")) %>%
    mutate(moi = ifelse(
      moin == "Autosomal dominant",
      "Autosomal Dominant",
      ifelse(
        moin == "Autosomal recessive",
        "Autosomal Recessive",
        ifelse(
          grepl("dominant", moin) &
            grepl("recessive", moin),
          "AD/AR",
          "other"
        )
      )
    )) %>%
    distinct() %>%
    ungroup()
  
  
  
  omim_annotations_cat <- omim_annotations %>%
    filter(gene_lethal_summary %in% c("lethal", "lethal|nonlethal", "nonlethal")) %>%
    select(hgnc_id, gene_lethal_summary, earliest_lethality_category) %>%
    distinct() %>%
    mutate(
      lethality = ifelse(
        gene_lethal_summary == "nonlethal",
        "NL",
        earliest_lethality_category
      )
    ) %>%
    select(hgnc_id, lethality) %>%
    distinct() %>%
    mutate(
      lethality = recode(
        lethality,
        L1 = "L1 (prenatal)",
        L2 = "L2 (neonatal)",
        L3 = "L3 (infant)",
        L4 = "L4 (childhood)",
        L5 = "L5 (adolescence)",
        L6 = "L6 (adulthood)",
        LU = "LU (undetermined)",
        NL = "NL (non-lethal)"
      )
    ) %>%
    left_join(omim_annotations_moi, by = c("hgnc_id" = "hgnc_id")) %>%
    mutate(lethality_moi = paste0(lethality, " ", moi))
  
  non_omim_annotations_cat <- gene_annotations %>%
    filter(!hgnc_id %in% omim_annotations_cat$hgnc_id) %>%
    mutate(lethality = "NC (not-in-catalogue)") %>%
    mutate(lethality_moi = lethality) %>%
    select(hgnc_id, lethality, lethality_moi)
  
  all_annotations_cat <- omim_annotations_cat %>%
    bind_rows(non_omim_annotations_cat) %>%
    mutate(threshold = -0.5) %>%
    distinct()
  
  SCORES_ALL <- all_annotations_cat %>%
    inner_join(gene_annotations, by = c("hgnc_id" = "hgnc_id")) %>%
    filter(!is.na(mean_depmap_gene_effect_score)) %>%
    rename(score = mean_depmap_gene_effect_score) %>%
    select(gene_symbol, score, lethality, lethality_moi, threshold) %>%
    distinct()
  
  SCORES_ALL_NO_MOI <- SCORES_ALL %>%
    filter(lethality_moi != "NC (not-in-catalogue)") %>%
    mutate(lethality_moi = lethality)
  
  
  SCORES_ALL_DUP <- SCORES_ALL %>%
    bind_rows(SCORES_ALL_NO_MOI) %>%
    select(gene_symbol, score, lethality_moi, threshold) %>%
    rename(lethality = lethality_moi)
  
  
  return(list(SCORES_ALL_DUP))
}

mod_score_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("read_data.R")
    
    get_ref_cat <- switch(id,
                          "LOEUF" = LOEUF_ref_cat,
                          "Shet" = Shet_ref_cat,
                          "DepMap" = DepMap_ref_cat)
    ref_cat <- get_ref_cat()
    
    lethality_selected_scores_1 <- reactive({
      ref_cat[[1]] %>%
        filter(lethality == input$lethality_category_1) %>%
        pull(score)
      
    })
    
    lethality_selected_category_1 <- reactive({
      ref_cat[[1]] %>%
        filter(lethality == input$lethality_category_1) %>%
        select(lethality) %>%
        distinct() %>%
        pull(lethality)
      
    })
    
    lethality_selected_scores_2 <- reactive({
      ref_cat[[1]] %>%
        filter(lethality == input$lethality_category_2) %>%
        pull(score)
      
    })
    
    lethality_selected_category_2 <- reactive({
      ref_cat[[1]] %>%
        filter(lethality == input$lethality_category_2) %>%
        select(lethality) %>%
        distinct() %>%
        pull(lethality)
      
    })
    
    
    
    threshold <-  unique(ref_cat[[1]]$threshold)
    
    output$hist <- renderPlotly({
      plot_ly(alpha = 0.7, xbins = list(size = input$bins)) %>%
        add_histogram(
          x = ~ lethality_selected_scores_1(),
          hovertemplate = "Number of genes in lethality category (1): %{y} <extra></extra>",
          name = lethality_selected_category_1(),
          marker = list(
            color = "rgb(0, 119, 182)",
            line = list(color = "rgb(202, 240, 248)", width = 1)
          )
        ) %>%
        add_histogram(
          x = ~ lethality_selected_scores_2(),
          hovertemplate = "Number of genes in lethality category (2): %{y} <extra></extra>",
          name = lethality_selected_category_2(),
          marker = list(
            color = "rgb(72, 202, 228)",
            line = list(color = "rgb(202, 240, 248)", width = 1)
          )
        ) %>%
        layout(
          barmode = "group",
          xaxis = list(title = sprintf("%s Score", id), zeroline = FALSE),
          yaxis = list(title = "Number of Genes", zeroline = FALSE),
          paper_bgcolor = "rgb(247,251,255)",
          plot_bgcolor = "rgb(247,251,255)",
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = 1.1
          )
        )
    })
    
    
    lethality_selected_scores_violin <- reactive({
      if (input$lethality_category_1 != input$lethality_category_2) {
        ref_cat[[1]] %>%
          filter(lethality %in% c(
            input$lethality_category_1,
            input$lethality_category_2
          )) %>%
          mutate(lethality = factor(
            lethality,
            levels = c(
              input$lethality_category_1,
              input$lethality_category_2
            )
          ))
        
      } else if (input$lethality_category_1 == input$lethality_category_2) {
        ref_cat[[1]] %>%
          filter(lethality %in% c(input$lethality_category_1)) %>%
          mutate(lethality = factor(lethality, levels = c(input$lethality_category_1)))
      }
    })
    
    output$violin <- renderPlotly({
      plot_ly(
        name = "",
        y = lethality_selected_scores_violin()$score,
        x = lethality_selected_scores_violin()$lethality,
        type = "violin",
        hoverinfo = "text",
        text = ~ paste0(
          lethality_selected_scores_violin()$gene_symbol,
          " ",
          round(lethality_selected_scores_violin()$score, 3)
        ),
        points = "all",
        color = lethality_selected_scores_violin()$lethality,
        colors = c("#0077B6", "#0077B6", "#48CAE4", "#48CAE4")
      ) %>%
        layout(
          barmode = "group",
          yaxis = list(title = sprintf("%s Score", id), zeroline = FALSE),
          xaxis = list(title = "lethality category"),
          paper_bgcolor = "rgb(247,251,255)",
          plot_bgcolor = "rgb(247,251,255)",
          showlegend = FALSE,
          shapes = list(
            type = "line",
            line = list(color = "grey"),
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = threshold,
            y1 = threshold
          )
        )
      
    })
    
  })
}
