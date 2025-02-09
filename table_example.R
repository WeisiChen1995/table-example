tab <- c("{n}", "({p}%)") %>%
  map(
    ~data %>%
      # Remove missing data in the Diabetes variable for simplicity
      filter(!is.na(Diabetes)) %>%
      
      # Format the Diabetes variable
      mutate(
        Diabetes = case_when(
          Diabetes == "Yes" ~ "With Diabetes",
          Diabetes == "No" ~ "Without Diabetes"
        ),
        Diabetes = factor(Diabetes, levels = c("With Diabetes", "Without Diabetes"))
      ) %>%
      
      # Add total number
      mutate(total = TRUE) %>%
      
      # Select relevant variables
      select(
        total, Gender, Age, AgeDecade, Race1, BMI_WHO, Education, 
        MaritalStatus, HHIncome, Work, Diabetes
      ) %>%
      
      # Create a summary table by Diabetes group
      tbl_summary(
        by = Diabetes,
        type = all_continuous() ~ "continuous2",
        statistic = list(
          # Include additional summary statistics for continuous variables
          all_continuous() ~ c("{mean} ({sd})",
                               "{median} ({p25}, {p75})",
                               "{min}, {max}"),
          all_categorical() ~ .x
        ),
        label = list(
          total = "Total (column denominator)",
          AgeDecade = "Age group",
          Race1 = "Ethnicity",
          BMI_WHO = "BMI group",
          HHIncome = "Household income",
          Work = "Employment status"
        ),
        missing = "no",
        
        # Remove decimal places for all numbers and percentages
        digits = list(
          all_continuous() ~ c(0, 0),
          all_categorical() ~ c(0, 0)
        )
      ) %>%
      
      # Add total column
      add_overall() %>%
      
      # Bold labels for readability
      bold_labels()) %>%
  tbl_merge() %>%
  modify_spanning_header(everything()~NA) %>%
  
  # Add some subtitle for 'n' and 'col%'
  modify_table_body(
    ~ {
      new_row <- tibble(
        label = " ",  
        stat_0_1 = "n",  
        stat_0_2 = "col%",  
        stat_1_1 = "n",  
        stat_1_2 = "col%",  
        stat_2_1 = "n",  
        stat_2_2 = "col%"  
      )
      
      bind_rows(new_row, .x)
    }
  ) %>%
  
  # Re-arrange the number and percentage columns
  modify_table_body(
    ~ .x %>%
      dplyr::relocate(stat_1_2, .after=stat_1_1) %>%
      dplyr::relocate(stat_2_2, .after=stat_2_1) %>%
      dplyr::relocate(stat_0_1, .after=stat_2_2) %>%
      dplyr::relocate(stat_0_2, .after=stat_0_1)
    %>%
      # Change label name
      dplyr::mutate(
        label = ifelse(label == "Median, (Q1, Q3)", "Median, (IQR)", label)
      ) %>%
      dplyr::mutate(
        label = ifelse(label == "Min, Max", "Range", label)
      ) %>%
      
      # Remove the summary statistics for the continuous variable in the % column
      dplyr::mutate(
        stat_0_2 = ifelse(label == "Mean (SD)", "",stat_0_2 ),
        stat_0_2 = ifelse(label == "Median (Q1, Q3)", "",stat_0_2 ),
        stat_0_2 = ifelse(label == "Range", "",stat_0_2 ),
        stat_1_2 = ifelse(label == "Mean (SD)", "",stat_1_2 ),
        stat_1_2 = ifelse(label == "Median (Q1, Q3)", "",stat_1_2 ),
        stat_1_2 = ifelse(label == "Range", "",stat_1_2 ),
        stat_2_2 = ifelse(label == "Mean (SD)", "",stat_2_2 ),
        stat_2_2 = ifelse(label == "Median (Q1, Q3)", "",stat_2_2 ),
        stat_2_2 = ifelse(label == "Range", "",stat_2_2 ),
      )
  ) %>%
  
  # Modify the header
  modify_header(
    update = list(
      all_stat_cols(TRUE) ~ "**{level}**",
      label = "",
      stat_0_1 = "**Total**",
      stat_0_2 = "",
      stat_1_1 = "**{level}**",
      stat_1_2 = "",
      stat_2_1 = "**{level}**",
      stat_2_2 = ""
    )
  ) %>%
  
  # Modify footnotes
  modify_footnote(
    c(all_stat_cols()) ~ NA
  ) %>%
  
  # Add more footnotes to specific rows
  modify_table_styling(
    columns = label,
    row = label == list("Gender"),
    footnote = "This is a sample footnote 1."
  ) %>%
  modify_table_styling(
    columns = label,
    row = label == list("Age"),
    footnote = "This is a sample footnote 2."
  ) %>%
  
  # Convert to gt table
  as_gt() %>%
  
  # Add table header with title
  gt::tab_header(
    title = md("**Table 1: Sociodemographic Characteristics of Patients With and Without Diabetes in the Demo Dataset**")
  ) %>%
  
  # Prevent footnotes from being split across multiple lines
  tab_options(footnotes.multiline = FALSE) %>%
  
  # Right-align all columns except the label column
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_body(
      columns = !label
    )
  ) %>%
  
  # Apply bold formatting to "n" and "col%" in the added row
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = 1,  # First row, where "n" and "col%" are added
      columns = c(stat_0_1, stat_0_2, stat_1_1, stat_1_2, stat_2_1, stat_2_2)  # Specify the columns
    )
  ) 

# Adding some colors to the tables
tab %>%
  tab_style(
    style = cell_fill(color = "#E8E4E6"),  # Apply the background color
    locations = cells_body(
      rows = seq(2, nrow(tab$`_data`), by = 2)  # Select every second row (alternating)
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#DAE9F7"),
    locations = cells_column_labels()
  )