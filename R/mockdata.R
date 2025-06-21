#' Generate Mock Data Files
#'
#' Generate mock biological data files based on specified datatypes and parameters.
#' This function creates realistic but minimal test files for bioinformatics workflows
#' without requiring large example datasets in the package.
#'
#' @param datatype Character. The type of file to generate (from datatypes.yaml).
#' @param output_file Character. Path where the generated file should be saved.
#'   If NULL, a temporary file will be created in tempdir().
#' @param size Character. Size category: "minimal" (default), "small", "medium", "large".
#'   Ignored if n_records is specified.
#' @param n_records Integer. Number of records to generate (overrides size if specified).
#' @param seed Integer. Random seed for reproducible generation (default: 123).
#' @param compress Logical. Whether to compress output files (default: auto-detected from extension).
#'   For fastq files, compression is enabled by default. For fasta and gtf files, compression is disabled by default.
#' @param ... Additional datatype-specific options. Use `sn_mockdata_options()` to see available options.
#'   For FASTQ files, common options include:
#'   - `read_length`: Read length (50, 75, 100, 150, 250, 300, or "short"/"medium"/"long"/"extra_long"/"ultra_long")
#'   - `read_type`: "single", "R1", "R2" (affects filename when auto-generating)
#'   - `adapters`: "none", "illumina", "nextera", or TRUE/FALSE
#'   - `adapter_contamination_rate`: Fraction of reads with adapters (0-1)
#'   - `min_quality`, `max_quality`: Quality score range
#'   - `error_rate`: Sequencing error rate (0-1)
#'
#' @details
#' The function supports generating mock files for all datatypes defined in the
#' global datatypes.yaml configuration. Common supported datatypes include:
#'
#' **Sequence Files:**
#' - `fastq`: Raw sequencing reads with quality scores and adapters
#' - `fasta`: Nucleotide or protein sequences (properly formatted with line breaks)
#'
#' **Alignment Files:**
#' - `sam`: Sequence alignment (text format)
#' - `bam`: Binary alignment (requires samtools for realistic headers)
#'
#' **Annotation Files:**
#' - `gtf`: Gene annotation in GTF format
#' - `gff`: Gene annotation in GFF format
#' - `bed`: Genomic intervals in BED format
#'
#' **Variant Files:**
#' - `vcf`: Variant calls in VCF format
#'
#' **Data Tables:**
#' - `csv`, `tsv`, `txt`: Tabular data files
#' - `json`: JSON structured data
#' - `yaml`: YAML configuration files
#'
#' **Realistic Data Generation:**
#' The function automatically ensures that related biological files use consistent
#' reference information:
#' - FASTQ reads include realistic error rates and adapters
#' - GTF/GFF annotations use matching chromosome names with the reference
#' - VCF/SAM files reference the same genome coordinates
#' - Multiple genes per chromosome for realistic annotations
#'
#' @return Character. Path to the generated file (invisibly).
#' @family mock data generation
#' @concept mock data generation
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate minimal mock files
#' sn_generate_mockdata("fasta", "test_genome.fa")
#' sn_generate_mockdata("fastq", "test_reads.fastq", size = "small")
#' sn_generate_mockdata("gtf", "test_annotation.gtf", n_records = 100)
#'
#' # Generate temporary files with automatic R1/R2 naming
#' temp_r1 <- sn_generate_mockdata("fastq", read_type = "R1", size = "small")
#' temp_r2 <- sn_generate_mockdata("fastq", read_type = "R2", size = "small")
#'
#' # Generate compressed files (auto-detected or explicit)
#' sn_generate_mockdata("fastq", "reads.fastq.gz") # Auto-compressed
#' sn_generate_mockdata("vcf", "variants.vcf", compress = TRUE)
#'
#' # Generate FASTQ with specific options
#' sn_generate_mockdata("fastq", "reads_150bp.fastq.gz",
#'   read_length = 150, adapters = "illumina",
#'   adapter_contamination_rate = 0.1, min_quality = 30
#' )
#'
#' # Generate FASTQ with shortcuts
#' sn_generate_mockdata("fastq", "short_reads.fq.gz", read_length = "short") # 50bp
#' sn_generate_mockdata("fastq", "long_reads.fq.gz", read_length = "extra_long") # 250bp
#'
#' # Generate FASTQ without adapters (clean reads)
#' sn_generate_mockdata("fastq", "clean_reads.fq.gz", adapters = "none")
#'
#' # See available options for each datatype
#' sn_mockdata_options("fastq")
#' }
sn_generate_mockdata <- function(datatype,
                                 output_file = NULL,
                                 size = "minimal",
                                 n_records = NULL,
                                 seed = 123,
                                 compress = NULL,
                                 ...) {
  # Validate datatype
  if (!.validate_datatype(datatype)) {
    cli_abort("Unsupported datatype: {.val {datatype}}. Check available datatypes with {.fun .load_datatypes}")
  }

  # Collect options from ... with validation
  options <- .validate_and_collect_options(datatype, ...)

  # Set random seed for reproducibility
  set.seed(seed)

  # Determine final record count (n_records overrides size)
  final_n_records <- if (!is.null(n_records)) {
    .validate_n_records(n_records)
  } else {
    .get_size_records(size)
  }

  # Handle NULL output_file - create temporary file
  if (is.null(output_file)) {
    output_file <- .create_temp_file_path(datatype, options, compress)
  }

  # Auto-detect compression from file extension, with datatype-specific defaults
  final_compress <- .determine_compression(output_file, datatype, compress)

  # Create output directory if needed
  .ensure_output_directory(output_file)

  # Always create shared context for biological files
  context <- .create_biological_context_if_needed(datatype, seed)

  # Show modern generation start message
  cli_h2("Generating {.strong {datatype}} Mock Data")

  # Generate file based on datatype
  result <- switch(datatype,
    "fasta" = .generate_fasta(output_file, final_n_records, options, context, final_compress),
    "fastq" = .generate_fastq(output_file, final_n_records, options, context, final_compress),
    "sam" = .generate_sam(output_file, final_n_records, options, context, final_compress),
    "gtf" = .generate_gtf(output_file, final_n_records, options, context, final_compress),
    "gff" = .generate_gff(output_file, final_n_records, options, context, final_compress),
    "bed" = .generate_bed(output_file, final_n_records, options, context, final_compress),
    "vcf" = .generate_vcf(output_file, final_n_records, options, context, final_compress),
    "csv" = .generate_csv(output_file, final_n_records, options, final_compress),
    "tsv" = .generate_tsv(output_file, final_n_records, options, final_compress),
    "txt" = .generate_txt(output_file, final_n_records, options, final_compress),
    "json" = .generate_json(output_file, final_n_records, options, final_compress),
    "yaml" = .generate_yaml(output_file, final_n_records, options, final_compress),
    "mtx" = .generate_mtx(output_file, final_n_records, options, final_compress),
    cli_abort("Mock data generation for datatype {.val {datatype}} is not yet implemented")
  )

  # Display success message and file info
  .display_generation_summary(output_file, datatype, final_n_records, final_compress, options, context)

  return(invisible(output_file))
}

#' Show Available Options for Mock Data Generation
#'
#' Display the available options for a specific datatype in mock data generation.
#' This helper function makes it easier to discover what parameters can be used
#' with `sn_generate_mockdata()`.
#'
#' @param datatype Character. The datatype to show options for.
#'
#' @return Invisibly returns the options list.
#' @family mock data generation
#' @concept mock data generation
#' @export
#'
#' @examples
#' \dontrun{
#' # Show options for FASTQ files
#' sn_mockdata_options("fastq")
#'
#' # Show options for FASTA files
#' sn_mockdata_options("fasta")
#'
#' # Show options for CSV files
#' sn_mockdata_options("csv")
#' }
sn_mockdata_options <- function(datatype) {
  if (!.validate_datatype(datatype)) {
    cli_abort("Unsupported datatype: {.val {datatype}}")
  }

  options_info <- .get_datatype_options(datatype)

  if (length(options_info) == 0) {
    cli_alert_info("No specific options available for datatype {.strong {datatype}}")
    return(invisible(list()))
  }

  cli_h2("Available Options for {.strong {datatype}}")

  for (option_name in names(options_info)) {
    option_def <- options_info[[option_name]]
    cli_li("{.strong {option_name}}: {option_def$description}")
    if (!is.null(option_def$default)) {
      cli_text("  Default: {.val {option_def$default}}")
    }
    if (!is.null(option_def$choices)) {
      cli_text("  Choices: {.val {paste(option_def$choices, collapse = ', ')}}")
    }
    cli_text("")
  }

  return(invisible(options_info))
}

#' Generate Mock Data for Multiple Files
#'
#' Generate multiple mock data files based on a specification list.
#' Useful for creating complete test datasets for bioinformatics workflows.
#' This function automatically creates a shared genome context for biological files.
#'
#' @param spec List. Specification for files to generate. Each element should be
#'   a list with 'datatype', and optionally 'output_file', 'size', 'n_records', and other options.
#'   If 'output_file' is NULL, temporary files will be created.
#' @param base_dir Character. Base directory for output files (default: current directory).
#'   Only used when output_file is specified in spec.
#' @param seed Integer. Random seed for reproducible generation.
#' @param compress Logical. Default compression setting for files (default: auto-detect).
#'
#' @return Character vector. Paths to generated files (invisibly).
#' @family mock data generation
#' @concept mock data generation
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate a complete test dataset with specific files
#' spec <- list(
#'   list(datatype = "fasta", output_file = "reference.fa", size = "small"),
#'   list(datatype = "fastq", output_file = "reads_R1.fastq.gz", 
#'        read_type = "R1", size = "medium"),
#'   list(datatype = "fastq", output_file = "reads_R2.fastq.gz", 
#'        read_type = "R2", size = "medium"),
#'   list(datatype = "gtf", output_file = "annotation.gtf", size = "small")
#' )
#' files <- sn_generate_mockdata_batch(spec, base_dir = "test_data/")
#'
#' # Generate temporary compatible files
#' temp_spec <- list(
#'   list(datatype = "fasta", size = "small"),
#'   list(datatype = "fastq", read_type = "R1"),
#'   list(datatype = "fastq", read_type = "R2"),
#'   list(datatype = "gtf", size = "small")
#' )
#' temp_files <- sn_generate_mockdata_batch(temp_spec)
#' }
sn_generate_mockdata_batch <- function(spec,
                                       base_dir = ".",
                                       seed = 123,
                                       compress = NULL) {
  if (!is.list(spec) || length(spec) == 0) {
    cli_abort("spec must be a non-empty list")
  }

  # Validate specification
  for (i in seq_along(spec)) {
    item <- spec[[i]]
    if (!is.list(item) || is.null(item$datatype)) {
      cli_abort("spec[[{i}]] must be a list with 'datatype' element")
    }
  }

  cli_h1("Generating Batch Mock Data")
  cli_alert_info("Processing {length(spec)} file specifications...")

  generated_files <- character(0)

  for (i in seq_along(spec)) {
    item <- spec[[i]]

    # Determine output path
    output_path <- if (!is.null(item$output_file)) {
      file.path(base_dir, item$output_file)
    } else {
      NULL # Will create temporary file
    }

    # Extract options (everything except known parameters)
    known_params <- c("datatype", "output_file", "size", "n_records", "compress")
    options_args <- item[!names(item) %in% known_params]

    # Generate file
    result <- do.call(sn_generate_mockdata, c(
      list(
        datatype = item$datatype,
        output_file = output_path,
        size = item$size %||% "minimal",
        n_records = item$n_records,
        seed = seed,
        compress = item$compress %||% compress
      ),
      options_args
    ))

    generated_files <- c(generated_files, result)
  }

  # Modern batch completion message
  cli_h2("Batch Generation Complete")
  total_size <- sum(sapply(generated_files, function(f) file.size(f)))

  completion_info <- list(
    "Files Generated" = length(generated_files),
    "Total Size" = .format_file_size(total_size)
  )

  if (any(sapply(spec, function(x) is.null(x$output_file)))) {
    completion_info[["Location"]] <- "Mixed (temp files + specified directory)"
  } else {
    completion_info[["Location"]] <- base_dir
  }

  cli_dl(completion_info)

  return(invisible(generated_files))
}

#' Generate Mock Example Value with Temporary File
#'
#' Enhanced version of .get_example_value that can optionally create
#' temporary mock files for realistic examples in documentation and help.
#'
#' @param datatype Character. The datatype name.
#' @param param_name Character. The parameter name (for context-aware examples).
#' @param tool_name Character. Optional tool name for tool-specific datatypes.
#' @param input_output Character. Whether this is for "input" or "output".
#' @param create_temp_file Logical. Whether to create actual temporary mock files.
#' @param temp_dir Character. Directory for temporary files (default: tempdir()).
#' @param size Character. Size category for mock data generation.
#'
#' @return Character string with example value (path to temp file if create_temp_file=TRUE).
#' @family mock data generation
#' @concept mock data generation
#' @export
#'
#' @examples
#' \dontrun{
#' # Get regular example value
#' sn_get_example_value_with_mockdata("fasta")
#'
#' # Create temporary mock file and return its path
#' temp_fasta <- sn_get_example_value_with_mockdata("fasta",
#'   param_name = "reference", create_temp_file = TRUE
#' )
#'
#' # Use in tool demonstration
#' temp_fastq <- sn_get_example_value_with_mockdata("fastq",
#'   param_name = "input1", create_temp_file = TRUE, size = "small"
#' )
#' }
sn_get_example_value_with_mockdata <- function(datatype,
                                               param_name = NULL,
                                               tool_name = NULL,
                                               input_output = "input",
                                               create_temp_file = FALSE,
                                               temp_dir = NULL,
                                               size = "minimal") {
  if (!create_temp_file) {
    # Use the original function for non-file examples
    return(.get_example_value(datatype, param_name, tool_name, input_output))
  }

  # Only create temp files for file datatypes
  datatypes <- .load_datatypes(tool_name)
  is_file_type <- !is.null(datatypes$file_types[[datatype]])

  if (!is_file_type) {
    # For non-file types, use original function
    return(.get_example_value(datatype, param_name, tool_name, input_output))
  }

  # Set up temporary directory
  if (is.null(temp_dir)) {
    temp_dir <- file.path(tempdir(), "shennong_examples")
  }

  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  # Generate appropriate filename
  base_filename <- .generate_example_filename(datatype, param_name, input_output, compress = FALSE)
  temp_file <- file.path(temp_dir, base_filename)

  # Check if file already exists to avoid regeneration
  if (!file.exists(temp_file)) {
    tryCatch(
      {
        sn_generate_mockdata(
          datatype = datatype,
          output_file = temp_file,
          size = size,
          seed = 123 # Fixed seed for consistent examples
        )
      },
      error = function(e) {
        # Fallback to original example if mock generation fails
        warning("Failed to generate mock file for ", datatype, ": ", e$message)
        return(.get_example_value(datatype, param_name, tool_name, input_output))
      }
    )
  }

  if (file.exists(temp_file)) {
    return(paste0('"', temp_file, '"'))
  } else {
    # Fallback to original function
    return(.get_example_value(datatype, param_name, tool_name, input_output))
  }
}

#' Generate Compatible Mock Dataset for RNA-seq Analysis
#'
#' A convenience function to quickly generate a complete set of compatible
#' mock files commonly used in RNA-seq alignment and quantification workflows.
#'
#' @param outdir Character. Directory to save files. If NULL, temporary files are created.
#' @param include_paired_reads Logical. Whether to generate paired-end reads (default: TRUE).
#' @param genome_size Character. Size of the reference genome: "minimal", "small", "medium".
#' @param read_count Character. Number of reads to generate: "minimal", "small", "medium", "large".
#' @param compress Logical. Whether to compress output files (default: FALSE for realistic workflow).
#' @param seed Integer. Random seed for reproducible generation.
#'
#' @return Named list with paths to generated files: reference, reads_r1, reads_r2, annotation.
#' @family mock data generation
#' @concept mock data generation
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate temporary compatible dataset
#' dataset <- sn_generate_rnaseq_dataset()
#'
#' # Generate dataset in specific directory
#' dataset <- sn_generate_rnaseq_dataset(
#'   outdir = "mock_rnaseq_data/",
#'   genome_size = "small",
#'   read_count = "medium"
#' )
#'
#' # Use the files for alignment testing
#' print(dataset)
#' # $reference
#' # $reads_r1
#' # $reads_r2
#' # $annotation
#' }
sn_generate_rnaseq_dataset <- function(outdir = NULL,
                                       include_paired_reads = TRUE,
                                       genome_size = "small",
                                       read_count = "medium",
                                       compress = FALSE,
                                       seed = 123) {
  cli_h1("Generating RNA-seq Dataset")

  # Define output files
  if (is.null(outdir)) {
    # Use temporary files
    reference_file <- NULL
    annotation_file <- NULL
    reads_r1_file <- NULL
    reads_r2_file <- NULL
  } else {
    # Create output directory
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }

    # Define file extensions
    ref_ext <- ".fa"
    gtf_ext <- ".gtf"
    fq_ext <- ".fq.gz"

    reference_file <- file.path(outdir, paste0("reference", ref_ext))
    annotation_file <- file.path(outdir, paste0("annotation", gtf_ext))
    reads_r1_file <- file.path(outdir, paste0("reads_R1", fq_ext))
    reads_r2_file <- if (include_paired_reads) file.path(outdir, paste0("reads_R2", fq_ext)) else NULL
  }

  # Generate reference genome
  reference <- sn_generate_mockdata(
    datatype = "fasta",
    output_file = reference_file,
    size = genome_size,
    seed = seed,
    compress = FALSE
  )

  # Generate annotation
  annotation <- sn_generate_mockdata(
    datatype = "gtf",
    output_file = annotation_file,
    size = genome_size,
    seed = seed,
    compress = FALSE
  )

  # Generate reads
  reads_r1 <- sn_generate_mockdata(
    datatype = "fastq",
    output_file = reads_r1_file,
    size = read_count,
    seed = seed,
    compress = TRUE,
    options = list(read_type = "R1", adapters = TRUE)
  )

  result <- list(
    reference = reference,
    annotation = annotation,
    reads_r1 = reads_r1
  )

  if (include_paired_reads) {
    reads_r2 <- sn_generate_mockdata(
      datatype = "fastq",
      output_file = reads_r2_file,
      size = read_count,
      seed = seed,
      compress = TRUE,
      options = list(read_type = "R2", adapters = TRUE)
    )
    result$reads_r2 <- reads_r2
  }

  # Modern completion message
  cli_h2("✔ RNA-seq Dataset Generation Complete!")

  dataset_info <- list(
    "Reference" = reference,
    "Annotation" = annotation,
    "Reads R1" = reads_r1
  )

  if (include_paired_reads) {
    dataset_info[["Reads R2"]] <- reads_r2
  }

  dataset_info[["Realistic Features"]] <- "Error rates, adapters, non-aligned reads"

  cli_dl(dataset_info)

  return(invisible(result))
}

#' Clean Up Mock Example Files
#'
#' Remove temporary mock files created by sn_get_example_value_with_mockdata
#' and other mock data generation functions.
#'
#' @param temp_dir Character. Directory containing temporary mock files.
#'   If NULL, cleans all default temporary directories.
#' @param pattern Character. File pattern to match for deletion (default: all files).
#'
#' @return Logical. TRUE if cleanup was successful.
#' @family mock data generation
#' @concept mock data generation
#' @export
#'
#' @examples
#' \dontrun{
#' # Clean up all temporary mock files
#' sn_cleanup_mockdata_examples()
#'
#' # Clean specific directory
#' sn_cleanup_mockdata_examples("/path/to/temp/dir")
#'
#' # Clean only specific file types
#' sn_cleanup_mockdata_examples(pattern = "*.fastq*")
#' }
sn_cleanup_mockdata_examples <- function(temp_dir = NULL, pattern = "*") {
  cleaned_dirs <- character(0)

  if (is.null(temp_dir)) {
    # Clean both example and mockdata temp directories
    temp_dirs <- c(
      file.path(tempdir(), "shennong_examples"),
      file.path(tempdir(), "shennong_mockdata")
    )
  } else {
    temp_dirs <- temp_dir
  }

  for (dir_path in temp_dirs) {
    if (dir.exists(dir_path)) {
      if (pattern == "*") {
        # Remove entire directory
        unlink(dir_path, recursive = TRUE)
        cleaned_dirs <- c(cleaned_dirs, dir_path)
      } else {
        # Remove files matching pattern
        files_to_remove <- list.files(dir_path,
          pattern = glob2rx(pattern),
          full.names = TRUE, recursive = TRUE
        )
        if (length(files_to_remove) > 0) {
          unlink(files_to_remove)
          cleaned_dirs <- c(cleaned_dirs, dir_path)
        }
      }
    }
  }

  if (length(cleaned_dirs) > 0) {
    cli_alert_success("Cleaned up mock files in: {.path {paste(basename(cleaned_dirs), collapse = ', ')}}")
    return(TRUE)
  } else {
    cli_alert_info("No mock files to clean up")
    return(TRUE)
  }
}

# Internal helper functions ------------------------------------------------

#' Validate and Collect Options from ...
#' @keywords internal
.validate_and_collect_options <- function(datatype, ...) {
  options <- list(...)
  
  # Get valid options for this datatype
  valid_options <- .get_datatype_options(datatype)
  
  # Validate provided options
  if (length(options) > 0 && length(valid_options) > 0) {
    invalid_options <- setdiff(names(options), names(valid_options))
    if (length(invalid_options) > 0) {
      cli_warn("Unknown options for {.strong {datatype}}: {.val {paste(invalid_options, collapse = ', ')}}. Use {.fun sn_mockdata_options} to see valid options.")
    }
  }
  
  return(options)
}

#' Get Available Options for Datatype
#' @keywords internal
.get_datatype_options <- function(datatype) {
  switch(datatype,
    "fastq" = list(
      read_length = list(
        description = "Read length in bp or shortcut ('short'=50, 'medium'=100, 'long'=150, 'extra_long'=250, 'ultra_long'=300)",
        default = 150,
        choices = c(50, 75, 100, 150, 250, 300, "short", "medium", "long", "extra_long", "ultra_long")
      ),
      read_type = list(
        description = "Read type for paired-end sequencing (affects filename when auto-generating)",
        default = "single",
        choices = c("single", "R1", "R2")
      ),
      adapters = list(
        description = "Adapter contamination type",
        default = "illumina", 
        choices = c("none", "illumina", "truseq", "nextera", TRUE, FALSE)
      ),
      adapter_contamination_rate = list(
        description = "Fraction of reads with adapter sequences (0-1)",
        default = 0.35
      ),
      min_quality = list(
        description = "Minimum Phred quality score",
        default = 25
      ),
      max_quality = list(
        description = "Maximum Phred quality score", 
        default = 40
      ),
      error_rate = list(
        description = "Sequencing error rate (0-1)",
        default = 0.015
      ),
      non_aligned_rate = list(
        description = "Fraction of non-aligned reads (0-1)",
        default = 0.1
      )
    ),
    "fasta" = list(
      sequence_type = list(
        description = "Type of sequences to generate",
        default = "nucleotide",
        choices = c("nucleotide", "protein")
      ),
      min_length = list(
        description = "Minimum sequence length",
        default = 200
      ),
      max_length = list(
        description = "Maximum sequence length",
        default = 2000
      )
    ),
    "csv" = list(
      columns = list(
        description = "Column names for the CSV file",
        default = c("sample_id", "value1", "value2", "category")
      )
    ),
    "tsv" = list(
      columns = list(
        description = "Column names for the TSV file",
        default = c("gene_id", "log2FC", "pvalue", "padj")
      )
    ),
    "txt" = list(
      content_type = list(
        description = "Type of text content to generate",
        default = "log",
        choices = c("log", "generic")
      )
    ),
    "json" = list(
      content_type = list(
        description = "Type of JSON content to generate",
        default = "config",
        choices = c("config", "data")
      )
    ),
    "mtx" = list(
      n_genes = list(
        description = "Number of genes (rows)",
        default = 1000
      ),
      n_cells = list(
        description = "Number of cells (columns)",
        default = 500
      ),
      sparsity = list(
        description = "Fraction of non-zero entries",
        default = 0.1
      )
    ),
    "sam" = list(
      reference_name = list(
        description = "Reference chromosome name",
        default = "chr1"
      ),
      reference_length = list(
        description = "Reference chromosome length",
        default = 10000
      )
    ),
    # Default: no specific options
    list()
  )
}

#' Validate n_records Parameter
#' @keywords internal
.validate_n_records <- function(n_records) {
  if (!is.numeric(n_records) || length(n_records) != 1 || n_records < 1) {
    cli_abort("n_records must be a positive integer")
  }
  as.integer(n_records)
}

#' Create Temporary File Path with Smart Naming
#' @keywords internal
.create_temp_file_path <- function(datatype, options, compress) {
  temp_dir <- file.path(tempdir(), "shennong_mockdata")
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  
  # Determine compression for filename generation
  temp_compress <- compress %||% (datatype == "fastq")
  
  # Generate smart filename considering options
  filename <- .generate_smart_temp_filename(datatype, options, temp_compress)
  
  return(file.path(temp_dir, filename))
}

#' Generate Smart Temporary Filename with R1/R2 Support
#' @keywords internal
.generate_smart_temp_filename <- function(datatype, options, compress) {
  # Get extensions for this datatype
  datatypes <- .load_datatypes()
  extensions <- datatypes$file_types[[datatype]]$extensions
  
  if (is.null(extensions) || length(extensions) == 0) {
    ext <- glue(".{datatype}")
  } else {
    # Choose appropriate extension
    if (compress && any(grepl("\\.gz$", extensions))) {
      compressed_exts <- extensions[grepl("\\.gz$", extensions)]
      ext <- compressed_exts[1]
    } else {
      uncompressed_exts <- extensions[!grepl("\\.gz$", extensions)]
      if (length(uncompressed_exts) > 0) {
        ext <- uncompressed_exts[1]
      } else {
        ext <- extensions[1]
      }
    }
  }
  
  # Generate timestamp and random suffix
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  random_suffix <- sample(1000:9999, 1)
  
  # Base name with smart R1/R2 handling
  base_name <- switch(datatype,
    "fasta" = "mock_genome",
    "fastq" = .get_smart_fastq_basename(options),
    "sam" = "mock_alignment",
    "bam" = "mock_alignment", 
    "gtf" = "mock_annotation",
    "gff" = "mock_annotation",
    "bed" = "mock_regions",
    "vcf" = "mock_variants",
    "csv" = "mock_data",
    "tsv" = "mock_results",
    "txt" = "mock_log",
    "json" = "mock_config",
    "yaml" = "mock_config",
    "mtx" = "mock_matrix",
    glue("mock_{datatype}")
  )
  
  return(glue("{base_name}_{timestamp}_{random_suffix}{ext}"))
}

#' Get Smart FASTQ Base Name with R1/R2 Support
#' @keywords internal
.get_smart_fastq_basename <- function(options) {
  read_type <- options$read_type %||% "single"
  
  if (read_type == "R1") {
    return("mock_reads_R1")
  } else if (read_type == "R2") {
    return("mock_reads_R2")
  } else {
    return("mock_reads")
  }
}

#' Determine Final Compression Setting  
#' @keywords internal
.determine_compression <- function(output_file, datatype, compress) {
  if (is.null(compress)) {
    if (grepl("\\.gz$", output_file, ignore.case = TRUE)) {
      return(TRUE)
    } else if (datatype == "fastq") {
      return(TRUE) # Default to compressed for fastq
    } else {
      return(FALSE) # Other files default to uncompressed
    }
  }
  return(compress)
}

#' Ensure Output Directory Exists
#' @keywords internal
.ensure_output_directory <- function(output_file) {
  outdir <- dirname(output_file)
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
}

#' Create Biological Context If Needed
#' @keywords internal
.create_biological_context_if_needed <- function(datatype, seed) {
  if (datatype %in% c("fasta", "fastq", "gtf", "gff", "sam", "vcf", "bed")) {
    return(.get_shared_context(seed))
  }
  return(NULL)
}

#' Display Generation Summary with Modern Formatting
#' @keywords internal
.display_generation_summary <- function(output_file, datatype, n_records, compress, options, context) {
  if (!file.exists(output_file)) {
    cli_abort("Failed to generate mock file: {.file {output_file}}")
  }
  
  # Modern success message with detailed file info
  file_size <- .format_file_size(file.size(output_file))
  file_name <- basename(output_file)
  
  cli_alert_success("Generated {.strong {datatype}} file: {.file {file_name}}")
  
  # Create info table
  info_data <- list(
    "Records" = format(n_records, big.mark = ","),
    "Size" = file_size
  )
  
  if (compress || grepl("\\.gz$", output_file)) {
    info_data[["Compression"]] <- "Yes (.gz)"
  }
  
  # Add datatype-specific info
  .add_datatype_specific_info(info_data, datatype, options, context)
  
  # Display info table
  cli_dl(info_data)
}

#' Add Datatype-Specific Information to Summary
#' @keywords internal
.add_datatype_specific_info <- function(info_data, datatype, options, context) {
  if (!is.null(context)) {
    if (datatype == "fastq") {
      info_data[["Alignment Rate"]] <- "~85-95% (realistic)"
      info_data[["Error Rate"]] <- "~1-2%"
      
      adapter_type <- options$adapters %||% "illumina"
      if (is.logical(adapter_type)) adapter_type <- if (adapter_type) "illumina" else "none"
      
      if (adapter_type != "none") {
        adapter_name <- switch(adapter_type,
          "illumina" = "Illumina TruSeq",
          "truseq" = "Illumina TruSeq", 
          "nextera" = "Illumina Nextera",
          "Custom"
        )
        info_data[["Adapters"]] <- adapter_name
      }
      
      read_type <- options$read_type %||% "single"
      if (read_type %in% c("R1", "R2")) {
        info_data[["Read Type"]] <- read_type
      }
    }
    info_data[["Compatibility"]] <- "Yes (consistent coordinates)"
  }
  
  # Add read length info for FASTQ
  if (datatype == "fastq") {
    read_length <- options$read_length %||% 150
    if (is.character(read_length)) {
      actual_length <- switch(read_length,
        "short" = 50,
        "medium" = 100, 
        "long" = 150,
        "extra_long" = 250,
        "ultra_long" = 300,
        150
      )
      info_data[["Read Length"]] <- glue("{actual_length}bp ({read_length})")
    } else {
      info_data[["Read Length"]] <- glue("{read_length}bp")
    }
  }
}

#' Get Record Count Based on Size Category
#' @keywords internal
.get_size_records <- function(size) {
  switch(size,
    "minimal" = 5,
    "small" = 100,
    "medium" = 1000,
    "large" = 10000,
    cli_abort("Invalid size category. Use: minimal, small, medium, large")
  )
}

#' Get Shared Context for Compatible Files (Internal)
#' @keywords internal
.get_shared_context <- function(seed) {
  # Create a shared environment to store context across calls
  env_name <- glue("sn_shared_context_{seed}")

  if (!exists(env_name, envir = .GlobalEnv)) {
    # Create new context
    context <- .create_internal_context(seed)
    assign(env_name, context, envir = .GlobalEnv)
  }

  return(get(env_name, envir = .GlobalEnv))
}

#' Create Internal Genome Context
#' @keywords internal
.create_internal_context <- function(seed) {
  set.seed(seed)

  chromosomes <- glue("chr{1:3}")
  chr_lengths <- setNames(sample(5000:15000, length(chromosomes)), chromosomes)

  # Generate sequences
  sequences <- list()
  for (chr in chromosomes) {
    sequences[[chr]] <- .generate_random_sequence(chr_lengths[chr])
  }

  # Generate gene positions with higher density (3-5 genes per 10kb)
  genes <- .generate_gene_positions(chromosomes, chr_lengths, 4, seed)

  list(
    seed = seed,
    chromosomes = chromosomes,
    chr_lengths = chr_lengths,
    sequences = sequences,
    genes = genes
  )
}

#' Generate Random DNA Sequence
#' @keywords internal
.generate_random_sequence <- function(length) {
  alphabet <- c("A", "T", "C", "G")
  paste(sample(alphabet, length, replace = TRUE), collapse = "")
}

#' Generate Gene Positions for Genome Context
#' @keywords internal
.generate_gene_positions <- function(chromosomes, chr_lengths, gene_density, seed) {
  set.seed(seed)

  genes <- data.frame(
    gene_id = character(0),
    chr = character(0),
    start = integer(0),
    end = integer(0),
    strand = character(0),
    stringsAsFactors = FALSE
  )

  gene_counter <- 1

  for (chr in chromosomes) {
    chr_len <- chr_lengths[chr]
    # Calculate number of genes based on density (genes per 10kb)
    n_genes <- max(1, round(chr_len / 10000 * gene_density))

    for (i in seq_len(n_genes)) {
      gene_length <- sample(500:3000, 1)
      start_pos <- sample(1:(chr_len - gene_length), 1)
      end_pos <- start_pos + gene_length - 1
      strand <- sample(c("+", "-"), 1)

      genes <- rbind(genes, data.frame(
        gene_id = glue("gene_{sprintf('%04d', gene_counter)}"),
        chr = chr,
        start = start_pos,
        end = end_pos,
        strand = strand,
        stringsAsFactors = FALSE
      ))

      gene_counter <- gene_counter + 1
    }
  }

  # Sort by chromosome and position
  genes <- genes[order(genes$chr, genes$start), ]

  return(genes)
}

#' Format FASTA Sequence with Proper Line Breaks
#' @keywords internal
.format_fasta_sequence <- function(sequence, line_length = 60) {
  if (nchar(sequence) <= line_length) {
    return(sequence)
  }

  # Split sequence into chunks of specified length
  seq_length <- nchar(sequence)
  chunks <- character()

  for (i in seq(1, seq_length, by = line_length)) {
    end_pos <- min(i + line_length - 1, seq_length)
    chunks <- c(chunks, substr(sequence, i, end_pos))
  }

  return(chunks)
}

#' Write Lines with Optional Compression
#' @keywords internal
.write_lines <- function(lines, output_file, compress = FALSE) {
  if (compress && !grepl("\\.gz$", output_file)) {
    output_file <- glue("{output_file}.gz")
  }

  if (compress || grepl("\\.gz$", output_file)) {
    # Write compressed
    con <- gzfile(output_file, "wt")
    writeLines(lines, con)
    close(con)
  } else {
    # Write uncompressed
    writeLines(lines, output_file)
  }

  return(output_file)
}

#' Generate Mock FASTA File
#' @keywords internal
.generate_fasta <- function(output_file, n_records, options, genome_context = NULL, compress) {
  sequence_type <- options$sequence_type %||% "nucleotide"
  min_length <- options$min_length %||% 200
  max_length <- options$max_length %||% 2000

  sequences <- character(0)

  if (!is.null(genome_context) && sequence_type == "nucleotide") {
    # Use genome context to generate consistent sequences
    for (chr in names(genome_context$sequences)) {
      chr_seq <- genome_context$sequences[[chr]]
      header <- glue(">{chr} length={nchar(chr_seq)}")

      # Format sequence with proper line breaks
      formatted_seq <- .format_fasta_sequence(chr_seq)

      sequences <- c(sequences, header, formatted_seq)
    }
  } else {
    # Generate random sequences
    if (sequence_type == "protein") {
      alphabet <- strsplit("ACDEFGHIKLMNPQRSTVWY", "")[[1]]
    } else {
      alphabet <- strsplit("ATCG", "")[[1]]
    }

    for (i in seq_len(n_records)) {
      seq_length <- sample(min_length:max_length, 1)
      sequence <- paste(sample(alphabet, seq_length, replace = TRUE), collapse = "")

      # Create header
      if (sequence_type == "protein") {
        header <- glue(">protein_{sprintf('%03d', i)} length={seq_length}")
      } else {
        header <- glue(">chr{sample(1:5, 1)}_contig_{sprintf('%03d', i)} length={seq_length}")
      }

      # Format sequence with proper line breaks
      formatted_seq <- .format_fasta_sequence(sequence)

      sequences <- c(sequences, header, formatted_seq)
    }
  }

  # Write with optional compression
  output_file <- .write_lines(sequences, output_file, compress)
  return(output_file)
}

#' Generate Mock FASTQ File
#' @keywords internal
.generate_fastq <- function(output_file, n_records, options, genome_context = NULL, compress) {
  # Support common sequencing read lengths
  read_length <- options$read_length %||% 150 # Modern Illumina default
  if (is.character(read_length)) {
    read_length <- switch(read_length,
      "short" = 50,
      "medium" = 100,
      "long" = 150,
      "extra_long" = 250,
      "ultra_long" = 300,
      150 # default fallback
    )
  }

  # Improved quality scores (higher default to pass fastp filtering)
  min_quality <- options$min_quality %||% 25 # Higher default for realistic data
  max_quality <- options$max_quality %||% 40
  read_type <- options$read_type %||% "single" # "single", "R1", "R2"
  error_rate <- options$error_rate %||% 0.015 # 1.5% error rate
  non_aligned_rate <- options$non_aligned_rate %||% 0.1 # 10% non-aligned reads

  # Enhanced adapter control
  adapters <- options$adapters %||% "illumina" # "none", "illumina", "nextera", "truseq", or TRUE/FALSE for backward compatibility
  if (is.logical(adapters)) {
    adapters <- if (adapters) "illumina" else "none"
  }
  adapter_contamination_rate <- options$adapter_contamination_rate %||% 0.35 # 35% of reads have adapters (realistic for fresh data)

  # Define adapter sequences based on type
  if (adapters == "illumina" || adapters == "truseq") {
    adapter_r1 <- "AGATCGGAAGAGCACACGTCTGAACTCCAGTCA" # TruSeq Universal Adapter
    adapter_r2 <- "AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT" # TruSeq Universal Adapter
  } else if (adapters == "nextera") {
    adapter_r1 <- "CTGTCTCTTATACACATCT" # Nextera transposase adapter
    adapter_r2 <- "CTGTCTCTTATACACATCT" # Same for both reads
  } else {
    adapter_r1 <- ""
    adapter_r2 <- ""
  }

  lines <- character(0)

  # Calculate number of aligned vs non-aligned reads
  n_aligned <- round(n_records * (1 - non_aligned_rate))
  n_non_aligned <- n_records - n_aligned

  read_counter <- 1

  if (!is.null(genome_context) && n_aligned > 0) {
    # Generate aligned reads from genome context sequences
    all_sequences <- genome_context$sequences

    for (i in seq_len(n_aligned)) {
      # Pick random chromosome and position
      chr <- sample(names(all_sequences), 1)
      chr_seq <- all_sequences[[chr]]
      chr_len <- nchar(chr_seq)

      if (chr_len < read_length) {
        # Fallback to random sequence if chromosome too short
        sequence <- paste(sample(c("A", "T", "C", "G"), read_length, replace = TRUE), collapse = "")
        start_pos <- 1
      } else {
        # Extract read from chromosome sequence
        start_pos <- sample(1:(chr_len - read_length + 1), 1)
        sequence <- substr(chr_seq, start_pos, start_pos + read_length - 1)

        # For R2 reads, reverse complement
        if (read_type == "R2") {
          sequence <- .reverse_complement(sequence)
        }

        # Introduce sequencing errors
        if (error_rate > 0) {
          sequence <- .introduce_sequencing_errors(sequence, error_rate)
        }

        # Add adapter contamination for some reads (simulate read-through)
        if (adapters != "none" && nchar(adapter_r1) > 0 && runif(1) < adapter_contamination_rate) {
          adapter_seq <- if (read_type == "R2") adapter_r2 else adapter_r1

          # Simulate realistic read-through scenarios
          if (runif(1) < 0.7) {
            # Short insert read-through: longer adapter sequences (15-32bp)
            adapter_length <- sample(15:min(32, nchar(adapter_seq)), 1)
          } else {
            # Partial adapter contamination: shorter sequences (8-15bp)
            adapter_length <- sample(8:15, 1)
          }

          partial_adapter <- substr(adapter_seq, 1, adapter_length)

          # Replace end of sequence with adapter (simulate read-through)
          replace_start <- read_length - adapter_length + 1
          sequence <- glue("{substr(sequence, 1, replace_start - 1)}{partial_adapter}")
        }
      }

      # Generate quality scores with realistic distribution
      qualities <- .generate_realistic_qualities(read_length, min_quality, max_quality)
      # FIX: Correctly encode quality scores using Phred+33 encoding
      quality_string <- rawToChar(as.raw(qualities + 33), multiple = FALSE)

      # Read identifier with chromosome info
      read_id <- glue("@read_{sprintf('%06d', read_counter)}_{chr}:{start_pos}")
      if (read_type %in% c("R1", "R2")) {
        read_id <- glue("{read_id}/{substr(read_type, 2, 2)}")
      }

      # FASTQ format
      lines <- c(
        lines,
        read_id,
        sequence,
        "+",
        quality_string
      )

      read_counter <- read_counter + 1
    }
  }

  # Generate non-aligned reads (random sequences)
  if (n_non_aligned > 0) {
    alphabet <- c("A", "T", "C", "G")

    for (i in seq_len(n_non_aligned)) {
      # Generate completely random sequence
      sequence <- paste(sample(alphabet, read_length, replace = TRUE), collapse = "")

      # Add adapter contamination for some reads (higher rate for non-aligned)
      if (adapters != "none" && nchar(adapter_r1) > 0 && runif(1) < (adapter_contamination_rate * 1.5)) {
        adapter_seq <- if (read_type == "R2") adapter_r2 else adapter_r1

        # Non-aligned reads tend to have more severe adapter contamination
        if (runif(1) < 0.8) {
          # Heavy adapter contamination: very long sequences (20-45bp)
          adapter_length <- sample(20:min(45, nchar(adapter_seq)), 1)
        } else {
          # Moderate contamination (12-20bp)
          adapter_length <- sample(12:20, 1)
        }

        partial_adapter <- substr(adapter_seq, 1, adapter_length)

        # Replace end of sequence with adapter
        replace_start <- read_length - adapter_length + 1
        sequence <- glue("{substr(sequence, 1, replace_start - 1)}{partial_adapter}")
      }

      # Generate quality scores
      qualities <- .generate_realistic_qualities(read_length, min_quality, max_quality)
      # FIX: Correctly encode quality scores using Phred+33 encoding
      quality_string <- rawToChar(as.raw(qualities + 33), multiple = FALSE)

      # Read identifier for non-aligned read
      read_id <- glue("@read_{sprintf('%06d', read_counter)}_random")
      if (read_type %in% c("R1", "R2")) {
        read_id <- glue("{read_id}/{substr(read_type, 2, 2)}")
      }

      # FASTQ format
      lines <- c(
        lines,
        read_id,
        sequence,
        "+",
        quality_string
      )

      read_counter <- read_counter + 1
    }
  }

  # If no genome context, generate all random reads
  if (is.null(genome_context)) {
    alphabet <- c("A", "T", "C", "G")

    for (i in seq_len(n_records)) {
      # Sequence
      sequence <- paste(sample(alphabet, read_length, replace = TRUE), collapse = "")

      # Add adapters to some reads
      if (adapters != "none" && nchar(adapter_r1) > 0 && runif(1) < adapter_contamination_rate) {
        adapter_seq <- if (read_type == "R2") adapter_r2 else adapter_r1

        # Mixed adapter contamination patterns
        if (runif(1) < 0.6) {
          # Common read-through: medium-long adapter sequences (15-30bp)
          adapter_length <- sample(15:min(30, nchar(adapter_seq)), 1)
        } else {
          # Short adapter fragments (10-18bp)
          adapter_length <- sample(10:18, 1)
        }

        partial_adapter <- substr(adapter_seq, 1, adapter_length)

        replace_start <- read_length - adapter_length + 1
        sequence <- glue("{substr(sequence, 1, replace_start - 1)}{partial_adapter}")
      }

      # Quality scores
      qualities <- .generate_realistic_qualities(read_length, min_quality, max_quality)
      # FIX: Correctly encode quality scores using Phred+33 encoding
      quality_string <- rawToChar(as.raw(qualities + 33), multiple = FALSE)

      # FASTQ format
      lines <- c(
        lines,
        glue("@read_{sprintf('%06d', i)}"),
        sequence,
        "+",
        quality_string
      )
    }
  }

  # Write with optional compression
  output_file <- .write_lines(lines, output_file, compress)
  return(output_file)
}

#' Introduce Sequencing Errors to DNA Sequence
#' @keywords internal
.introduce_sequencing_errors <- function(sequence, error_rate) {
  if (error_rate <= 0) {
    return(sequence)
  }

  chars <- strsplit(sequence, "")[[1]]
  n_errors <- rbinom(1, length(chars), error_rate)

  if (n_errors > 0) {
    error_positions <- sample(seq_along(chars), n_errors)
    alphabet <- c("A", "T", "C", "G")

    for (pos in error_positions) {
      # Replace with different base
      current_base <- chars[pos]
      new_bases <- alphabet[alphabet != current_base]
      chars[pos] <- sample(new_bases, 1)
    }
  }

  return(paste(chars, collapse = ""))
}

#' Generate Realistic Quality Scores
#' @keywords internal
.generate_realistic_qualities <- function(read_length, min_quality, max_quality) {
  # Quality tends to decrease towards 3' end
  position_effect <- seq(0, -5, length.out = read_length)
  base_qualities <- rnorm(read_length, mean = (min_quality + max_quality) / 2, sd = 3)

  # Apply position effect
  qualities <- round(base_qualities + position_effect)

  # Clamp to valid range
  qualities <- pmax(min_quality, pmin(max_quality, qualities))

  return(qualities)
}

#' Generate Reverse Complement of DNA Sequence
#' @keywords internal
.reverse_complement <- function(sequence) {
  # Reverse the sequence
  rev_seq <- paste(rev(strsplit(sequence, "")[[1]]), collapse = "")

  # Complement
  complement_map <- c(
    "A" = "T", "T" = "A", "C" = "G", "G" = "C",
    "N" = "N", "a" = "t", "t" = "a", "c" = "g", "g" = "c"
  )

  chars <- strsplit(rev_seq, "")[[1]]
  complement_chars <- ifelse(chars %in% names(complement_map),
    complement_map[chars],
    chars
  )

  return(paste(complement_chars, collapse = ""))
}

#' Generate Mock SAM File
#' @keywords internal
.generate_sam <- function(output_file, n_records, options, genome_context = NULL, compress) {
  lines <- character(0)

  # SAM header
  lines <- c(lines, "@HD\tVN:1.0\tSO:coordinate")

  if (!is.null(genome_context)) {
    # Add reference sequences to header
    for (chr in genome_context$chromosomes) {
      chr_len <- genome_context$chr_lengths[chr]
      lines <- c(lines, glue("@SQ\tSN:{chr}\tLN:{chr_len}"))
    }

    chromosomes <- genome_context$chromosomes
    chr_lengths <- genome_context$chr_lengths
  } else {
    # Default reference
    reference_name <- options$reference_name %||% "chr1"
    reference_length <- options$reference_length %||% 10000
    lines <- c(lines, glue("@SQ\tSN:{reference_name}\tLN:{reference_length}"))

    chromosomes <- reference_name
    chr_lengths <- setNames(reference_length, reference_name)
  }

  # Alignment records
  for (i in seq_len(n_records)) {
    chr <- sample(chromosomes, 1)
    max_pos <- chr_lengths[chr] - 100
    pos <- sample(1:max_pos, 1)
    mapq <- sample(20:60, 1)

    lines <- c(
      lines,
      glue("read_{i}\t0\t{chr}\t{pos}\t{mapq}\t100M\t*\t0\t0\t{paste(sample(c('A', 'T', 'C', 'G'), 100, replace = TRUE), collapse = '')}\t{paste(rep('I', 100), collapse = '')}")
    )
  }

  # Write with optional compression
  output_file <- .write_lines(lines, output_file, compress)
  return(output_file)
}

#' Generate Mock GTF File
#' @keywords internal
.generate_gtf <- function(output_file, n_records, options, genome_context = NULL, compress) {
  lines <- character(0)

  if (!is.null(genome_context)) {
    # Use genome context for consistent annotations
    genes_info <- genome_context$genes

    # Limit to requested number of records
    if (nrow(genes_info) > n_records) {
      genes_info <- genes_info[1:n_records, ]
    }

    for (i in seq_len(nrow(genes_info))) {
      gene <- genes_info[i, ]

      attributes <- glue('gene_id "{gene$gene_id}"; transcript_id "{gene$gene_id}_tx1";')

      lines <- c(
        lines,
        glue("{gene$chr}\tmock_annotation\texon\t{gene$start}\t{gene$end}\t.\t{gene$strand}\t.\t{attributes}")
      )
    }
  } else {
    # Generate random GTF records
    chromosomes <- options$chromosomes %||% glue("chr{1:3}")

    gene_id <- 1

    for (i in seq_len(n_records)) {
      chr <- sample(chromosomes, 1)
      start <- sample(1000:50000, 1)
      end <- start + sample(500:5000, 1)
      strand <- sample(c("+", "-"), 1)

      attributes <- glue('gene_id "gene_{gene_id}"; transcript_id "tx_{gene_id}";')

      lines <- c(
        lines,
        glue("{chr}\ttest\texon\t{start}\t{end}\t.\t{strand}\t.\t{attributes}")
      )

      gene_id <- gene_id + 1
    }
  }

  # Write with optional compression
  output_file <- .write_lines(lines, output_file, compress)
  return(output_file)
}

#' Generate Mock GFF File
#' @keywords internal
.generate_gff <- function(output_file, n_records, options, genome_context = NULL, compress) {
  lines <- character(0)
  lines <- c(lines, "##gff-version 3")

  if (!is.null(genome_context)) {
    # Use genome context for consistent annotations
    genes_info <- genome_context$genes

    # Limit to requested number of records
    if (nrow(genes_info) > n_records) {
      genes_info <- genes_info[1:n_records, ]
    }

    for (i in seq_len(nrow(genes_info))) {
      gene <- genes_info[i, ]

      attributes <- glue("ID={gene$gene_id};Name={gene$gene_id}")

      lines <- c(
        lines,
        glue("{gene$chr}\tmock_annotation\tgene\t{gene$start}\t{gene$end}\t.\t{gene$strand}\t.\t{attributes}")
      )
    }
  } else {
    # Generate random GFF records
    chromosomes <- options$chromosomes %||% glue("chr{1:3}")

    for (i in seq_len(n_records)) {
      chr <- sample(chromosomes, 1)
      start <- sample(1000:50000, 1)
      end <- start + sample(500:5000, 1)
      strand <- sample(c("+", "-"), 1)

      attributes <- glue("ID=gene_{i};Name=gene_{i}")

      lines <- c(
        lines,
        glue("{chr}\ttest\tgene\t{start}\t{end}\t.\t{strand}\t.\t{attributes}")
      )
    }
  }

  # Write with optional compression
  output_file <- .write_lines(lines, output_file, compress)
  return(output_file)
}

#' Generate Mock BED File
#' @keywords internal
.generate_bed <- function(output_file, n_records, options, genome_context = NULL, compress) {
  lines <- character(0)

  if (!is.null(genome_context)) {
    # Use genome context for consistent regions
    chromosomes <- genome_context$chromosomes
    chr_lengths <- genome_context$chr_lengths

    for (i in seq_len(n_records)) {
      chr <- sample(chromosomes, 1)
      max_pos <- chr_lengths[chr] - 1000
      start <- sample(1:max_pos, 1)
      end <- start + sample(100:1000, 1)

      lines <- c(
        lines,
        glue("{chr}\t{start}\t{end}\tregion_{i}")
      )
    }
  } else {
    # Generate random BED records
    chromosomes <- options$chromosomes %||% glue("chr{1:3}")

    for (i in seq_len(n_records)) {
      chr <- sample(chromosomes, 1)
      start <- sample(1000:50000, 1)
      end <- start + sample(100:1000, 1)

      lines <- c(
        lines,
        glue("{chr}\t{start}\t{end}\tregion_{i}")
      )
    }
  }

  # Write with optional compression
  output_file <- .write_lines(lines, output_file, compress)
  return(output_file)
}

#' Generate Mock VCF File
#' @keywords internal
.generate_vcf <- function(output_file, n_records, options, genome_context = NULL, compress) {
  lines <- character(0)

  # VCF header
  lines <- c(
    lines,
    "##fileformat=VCFv4.2",
    "##source=ShennongTools_MockDataGenerator"
  )

  if (!is.null(genome_context)) {
    # Add chromosome info to header
    for (chr in genome_context$chromosomes) {
      chr_len <- genome_context$chr_lengths[chr]
      lines <- c(lines, glue("##contig=<ID={chr},length={chr_len}>"))
    }
  }

  lines <- c(lines, "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO")

  # Variant records
  if (!is.null(genome_context)) {
    chromosomes <- genome_context$chromosomes
    chr_lengths <- genome_context$chr_lengths

    for (i in seq_len(n_records)) {
      chr <- sample(chromosomes, 1)
      max_pos <- chr_lengths[chr] - 1
      pos <- sample(1:max_pos, 1)
      ref <- sample(c("A", "T", "C", "G"), 1)
      alt <- sample(setdiff(c("A", "T", "C", "G"), ref), 1)
      qual <- sample(20:99, 1)

      lines <- c(
        lines,
        glue("{chr}\t{pos}\tvar_{i}\t{ref}\t{alt}\t{qual}\tPASS\t.")
      )
    }
  } else {
    # Generate random variants
    chromosomes <- options$chromosomes %||% glue("chr{1:3}")

    for (i in seq_len(n_records)) {
      chr <- sample(chromosomes, 1)
      pos <- sample(1000:50000, 1)
      ref <- sample(c("A", "T", "C", "G"), 1)
      alt <- sample(setdiff(c("A", "T", "C", "G"), ref), 1)
      qual <- sample(20:99, 1)

      lines <- c(
        lines,
        glue("{chr}\t{pos}\tvar_{i}\t{ref}\t{alt}\t{qual}\tPASS\t.")
      )
    }
  }

  # Write with optional compression
  output_file <- .write_lines(lines, output_file, compress)
  return(output_file)
}

#' Generate Mock CSV File
#' @keywords internal
.generate_csv <- function(output_file, n_records, options, compress) {
  columns <- options$columns %||% c("sample_id", "value1", "value2", "category")

  data_list <- list()
  data_list[[columns[1]]] <- glue("sample_{seq_len(n_records)}")

  for (col in columns[-1]) {
    if (grepl("value|score|count", col, ignore.case = TRUE)) {
      data_list[[col]] <- round(runif(n_records, 0, 100), 2)
    } else if (grepl("category|type|group", col, ignore.case = TRUE)) {
      data_list[[col]] <- sample(c("A", "B", "C"), n_records, replace = TRUE)
    } else {
      data_list[[col]] <- glue("{col}_{seq_len(n_records)}")
    }
  }

  df <- data.frame(data_list, stringsAsFactors = FALSE)

  # Handle compression for CSV
  if (compress && !grepl("\\.gz$", output_file)) {
    output_file <- glue("{output_file}.gz")
  }

  if (compress || grepl("\\.gz$", output_file)) {
    con <- gzfile(output_file, "wt")
    write.csv(df, con, row.names = FALSE)
    close(con)
  } else {
    write.csv(df, output_file, row.names = FALSE)
  }

  return(output_file)
}

#' Generate Mock TSV File
#' @keywords internal
.generate_tsv <- function(output_file, n_records, options, compress) {
  columns <- options$columns %||% c("gene_id", "log2FC", "pvalue", "padj")

  data_list <- list()
  data_list[[columns[1]]] <- glue("gene_{seq_len(n_records)}")

  for (col in columns[-1]) {
    if (grepl("pvalue|padj|fdr", col, ignore.case = TRUE)) {
      data_list[[col]] <- runif(n_records, 0, 1)
    } else if (grepl("fc|fold|log", col, ignore.case = TRUE)) {
      data_list[[col]] <- rnorm(n_records, 0, 2)
    } else {
      data_list[[col]] <- round(runif(n_records, 0, 1000), 2)
    }
  }

  df <- data.frame(data_list, stringsAsFactors = FALSE)

  # Handle compression for TSV
  if (compress && !grepl("\\.gz$", output_file)) {
    output_file <- glue("{output_file}.gz")
  }

  if (compress || grepl("\\.gz$", output_file)) {
    con <- gzfile(output_file, "wt")
    write.table(df, con, sep = "\t", row.names = FALSE, quote = FALSE)
    close(con)
  } else {
    write.table(df, output_file, sep = "\t", row.names = FALSE, quote = FALSE)
  }

  return(output_file)
}

#' Generate Mock TXT File
#' @keywords internal
.generate_txt <- function(output_file, n_records, options, compress) {
  content_type <- options$content_type %||% "log"

  if (content_type == "log") {
    lines <- character(0)
    for (i in seq_len(n_records)) {
      timestamp <- format(Sys.time() + i, "%Y-%m-%d %H:%M:%S")
      level <- sample(c("INFO", "WARNING", "ERROR"), 1, prob = c(0.7, 0.2, 0.1))
      message <- glue("Process step {i} completed with {sample(c('success', 'warnings'), 1)}")
      lines <- c(lines, glue("{timestamp} {level} {message}"))
    }
  } else {
    lines <- glue("Line {seq_len(n_records)} - Mock text content for testing")
  }

  # Write with optional compression
  output_file <- .write_lines(lines, output_file, compress)
  return(output_file)
}

#' Generate Mock JSON File
#' @keywords internal
.generate_json <- function(output_file, n_records, options, compress) {
  content_type <- options$content_type %||% "config"

  if (content_type == "config") {
    config <- list(
      version = "1.0",
      tool_name = "mock_tool",
      parameters = list(
        threads = 4,
        memory = "8G",
        quality_threshold = 20
      ),
      input_files = glue("input_{seq_len(min(n_records, 5))}.fastq"),
      outdirectory = "results/"
    )
  } else {
    config <- list(
      data = lapply(seq_len(n_records), function(i) {
        list(
          id = glue("item_{i}"),
          value = runif(1, 0, 100),
          category = sample(c("A", "B", "C"), 1)
        )
      })
    )
  }

  json_content <- jsonlite::toJSON(config, pretty = TRUE, auto_unbox = TRUE)

  # Write with optional compression
  output_file <- .write_lines(json_content, output_file, compress)
  return(output_file)
}

#' Generate Mock YAML File
#' @keywords internal
.generate_yaml <- function(output_file, n_records, options, compress) {
  config <- list(
    tool_name = "mock_tool",
    version = "1.0.0",
    description = "Mock tool configuration",
    parameters = list()
  )

  for (i in seq_len(n_records)) {
    param_name <- glue("param_{i}")
    param_type <- sample(c("string", "integer", "float"), 1)
    config$parameters[[param_name]] <- list(
      type = param_type,
      default = switch(param_type,
        "string" = glue("value_{i}"),
        "integer" = sample(1:100, 1),
        "float" = round(runif(1, 0, 10), 2)
      ),
      description = glue("Description for parameter {i}")
    )
  }

  yaml_content <- as.yaml(config)

  # Write with optional compression
  output_file <- .write_lines(yaml_content, output_file, compress)
  return(output_file)
}

#' Generate Mock MTX (Matrix Market) File
#' @keywords internal
.generate_mtx <- function(output_file, n_records, options, compress) {
  n_genes <- options$n_genes %||% min(n_records, 1000)
  n_cells <- options$n_cells %||% min(n_records, 500)
  sparsity <- options$sparsity %||% 0.1 # Fraction of non-zero entries

  lines <- character(0)

  # MTX header
  lines <- c(
    lines,
    "%%MatrixMarket matrix coordinate integer general",
    glue("{n_genes} {n_cells} {round(n_genes * n_cells * sparsity)}")
  )

  # Generate sparse matrix entries
  n_entries <- round(n_genes * n_cells * sparsity)

  for (i in seq_len(n_entries)) {
    gene_idx <- sample(1:n_genes, 1)
    cell_idx <- sample(1:n_cells, 1)
    count <- sample(1:50, 1)

    lines <- c(lines, glue("{gene_idx} {cell_idx} {count}"))
  }

  # Write with optional compression
  output_file <- .write_lines(lines, output_file, compress)
  return(output_file)
}
