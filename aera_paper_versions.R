library(quarto)

# Define the academic years based on the required "last three years"
target_years <- c("2022-23", "2023-24", "2024-25")
input_document <- "aera_paper_code.qmd" # Update if your file name differs

for (yr in target_years) {
  # Construct a unique output filename
  out_name <- sprintf("AERA_Paper_%s.docx", yr)

  # Render the document with the specified parameter
  quarto_render(
    input = input_document,
    output_file = out_name,
    execute_params = list(year = yr)
  )

  message(sprintf("Successfully rendered %s", out_name))
}
