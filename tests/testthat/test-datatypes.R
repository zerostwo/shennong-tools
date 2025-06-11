test_that("Global datatypes can be loaded", {
  datatypes <- .sn_load_datatypes()

  expect_true(is.list(datatypes))
  expect_true("file_types" %in% names(datatypes))
  expect_true("value_types" %in% names(datatypes))

  # Check some common datatypes exist
  expect_true("fasta" %in% names(datatypes$file_types))
  expect_true("fastq" %in% names(datatypes$file_types))
  expect_true("bam" %in% names(datatypes$file_types))
  expect_true("integer" %in% names(datatypes$value_types))
  expect_true("string" %in% names(datatypes$value_types))
})

test_that("Example values can be generated from datatypes", {
  # Test file types
  expect_equal(.sn_get_example_value("fasta"), "\"path/to/genome.fa\"")
  expect_equal(.sn_get_example_value("fastq"), "\"path/to/reads.fastq.gz\"")
  expect_equal(.sn_get_example_value("bam", input_output = "output"), "\"output/alignment.bam\"")

  # Test value types
  expect_equal(.sn_get_example_value("integer"), "4")
  expect_equal(.sn_get_example_value("boolean"), "TRUE")

  # Test fallback for unknown types
  expect_equal(.sn_get_example_value("unknown_type"), "\"path/to/file\"")
})

test_that("Datatype validation works", {
  # Valid datatypes
  expect_true(.sn_validate_datatype("fasta"))
  expect_true(.sn_validate_datatype("integer"))
  expect_true(.sn_validate_datatype("boolean"))

  # Invalid datatype
  expect_false(.sn_validate_datatype("nonexistent_type"))

  # Vector of datatypes
  expect_true(.sn_validate_datatype(c("fasta", "fastq")))
  expect_false(.sn_validate_datatype(c("fasta", "nonexistent")))
})

test_that("Tool-specific datatypes can be merged", {
  # This test assumes the example-tool directory exists
  skip_if_not(dir.exists(system.file("tools", "example-tool", package = "ShennongTools")))

  datatypes <- .sn_load_datatypes("example-tool")

  # Should have global datatypes
  expect_true("fasta" %in% names(datatypes$file_types))

  # Should have tool-specific datatypes
  expect_true("blast_db" %in% names(datatypes$file_types))
  expect_true("evalue" %in% names(datatypes$value_types))
})

test_that("Usage example generation uses datatype registry", {
  cmd_config <- list(
    inputs = list(
      input_file = list(datatype = "fasta", required = TRUE)
    ),
    outputs = list(
      output_file = list(datatype = "bam", required = TRUE)
    ),
    params = list(
      threads = list(datatype = "integer", default = 4),
      sensitivity = list(datatype = "string")
    )
  )

  example <- .sn_generate_usage_example(cmd_config, "test_command", "test_tool")

  expect_true(is.character(example))
  expect_true(grepl("sn_run", example))
  expect_true(grepl("genome.fa", example))
  expect_true(grepl("output.*bam", example))
  expect_true(grepl("threads = 4", example))
})
