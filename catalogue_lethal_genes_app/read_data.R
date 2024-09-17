load_omim <- function() {
  omim_df <- read.fst("./data/omim_curation.fst")
  
  
  return(omim_df)
  
}

load_gene <- function() {
  gene_df <- read.fst("./data/gene_annotations.fst")
  
  
  return(gene_df)
  
}

create_omim_table <- function() {
  omim_table  <- load_omim() %>%
    select(
      omim_id,
      omim_phenotype,
      mode_of_inheritance,
      gene_symbol,
      hgnc_id,
      api_lethal_hits,
      lethal_hit_phenotype_included,
      lethality_category_hpo,
      evidence_from_proband,
      evidence_from_family_members,
      disease_gene_lethal,
      gene_lethal_summary,
      all_lethality_categories,
      earliest_lethality_category,
      merged_earliest_lethality_category
    ) %>%
    rename(
      'OMIM id' = omim_id,
      'OMIM phenotype' = omim_phenotype,
      'Mode of inheritance' = mode_of_inheritance,
      'Gene symbol' = gene_symbol,
      'HGNC id' = hgnc_id,
      'Query lethal hit' = api_lethal_hits,
      'Hit included after curation' = lethal_hit_phenotype_included,
      'Lethality category' = lethality_category_hpo,
      'Evidence from proband' = evidence_from_proband,
      'Evidence from family members' = evidence_from_family_members,
      'Phenotype-Gene lethal' = disease_gene_lethal,
      'Gene lethality summary' = gene_lethal_summary,
      'Gene lethality categories' = all_lethality_categories,
      'Gene earliest lethality category' = earliest_lethality_category,
      'Grouped gene earliest lethality category' = merged_earliest_lethality_category
    ) %>%
    arrange('OMIM id', 'Gene symbol')
  
  
  return(omim_table)
  
}

create_literature_table <- function() {
  literature_table <- load_lit() %>%
    rename(
      mim_number = existing_mimnumber_association,
      reported_cases = number_of_reported_cases,
      fetal_demise = gestation_age_of_fetal_demise,
      death_postnatal = age_of_death_postnatal,
      selected = abstract_selected_strict_criteria
    ) %>%
    select(
      gene_symbol,
      previous_history,
      preterm_fullterm,
      observed_phenotype,
      mim_number,
      allelic_requirement,
      variant_classification,
      pmid,
      system_affected,
      selected
    )
  
  return(literature_table)
  
}