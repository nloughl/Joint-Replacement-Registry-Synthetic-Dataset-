# Documenting Rise in Cementless Implants: Validation Before Analysis

# Create dictionary with model names and intended fixation method 
## Depending on specififcity of ModelName may need to use UDI or ProductName 
model_fixation_lookup <- tibble(
  ModelName = c("NexGen LPS", "Attune", "Triathlon", "Persona", "Vanguard"),
  intended_fixation = c("cemented", "dual-purpose", "cementless", "cemented", "cemented")
)

implants <- implants %>%
  left_join(model_fixation_lookup, by = "ModelName")


# Define intended design using surface features if Fixation is missing
implants <- implants %>%
  mutate(
    intended_design = case_when(
        # Use model-level fixation if available -- take this out if not helpful 
        !is.na(intended_fixation) ~ intended_fixation,
      # Use Fixation if available
      Fixation == "Cemented" ~ "cemented",
      Fixation == "Cementless" ~ "cementless",
      Fixation == "Hybrid" ~ "hybrid",

      # Fallback to surface features
      is.na(Fixation) & FixationSurfaceCemented == TRUE & FixationSurfaceCementless != TRUE ~ "cemented",
      is.na(Fixation) & FixationSurfaceCementless == TRUE & FixationSurfaceCemented != TRUE ~ "cementless",
      is.na(Fixation) & FixationSurfaceCemented == TRUE & FixationSurfaceCementless == TRUE ~ "dual-purpose",  # may be either
      is.na(Fixation) & is.na(FixationSurfaceCemented) & is.na(FixationSurfaceCementless) ~ "unknown",

      TRUE ~ "unknown"
    )
  )

# Flag inconsistencies between actual and intended cement use
  implants <- implants %>%
  mutate(
    mismatch_flag = case_when(
      intended_design == "cemented" & !cement_used_any ~ "cement missing",
      intended_design == "cementless" & cement_used_any ~ "cement used in cementless",
      intended_design == "hybrid" ~ "check manually",
      intended_design == "dual-purpose" ~ "potentially appropriate",
      intended_design == "unknown" ~ "unknown design intent",
      TRUE ~ NA_character_
    )
  )

# Summarize Mismatches 
implants %>%
  count(ModelName, intended_design, cement_used_any, mismatch_flag) %>%
  arrange(desc(n))

# Flag Implatns with inferred intent 
implants <- implants %>%
  mutate(
    intent_inferred_from = case_when(
      !is.na(Fixation) ~ "Fixation",
      is.na(Fixation) & (!is.na(FixationSurfaceCemented) | !is.na(FixationSurfaceCementless)) ~ "SurfaceFeatures",
      TRUE ~ "Unknown"
    )
  )
