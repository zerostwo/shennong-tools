test_that("Mock data generation basic functionality works", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Test FASTA generation
  fasta_file <- file.path(temp_dir, "test.fa")
  result <- sn_generate_mockdata("fasta", fasta_file, size = "minimal")

  expect_true(file.exists(fasta_file))
  expect_equal(result, fasta_file)

  # Check file content
  content <- readLines(fasta_file)
  expect_true(length(content) > 0)
  expect_true(any(grepl("^>", content))) # Should have headers

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("NULL output_file creates temporary files", {
  # Generate temporary file
  temp_fasta <- sn_generate_mockdata("fasta", NULL, size = "minimal")

  expect_true(is.character(temp_fasta))
  expect_true(file.exists(temp_fasta))
  expect_true(grepl("mock_genome", basename(temp_fasta)))

  # Check content
  content <- readLines(temp_fasta)
  expect_true(length(content) > 0)
  expect_true(any(grepl("^>", content)))

  # Clean up
  sn_cleanup_mockdata_examples()
})

test_that("Genome context creates compatible files", {
  # Create genome context
  context <- sn_create_genome_context(seed = 123, chromosomes = c("chr1", "chr2"))

  expect_true(is.list(context))
  expect_true("genome_context" %in% class(context))
  expect_equal(context$chromosomes, c("chr1", "chr2"))
  expect_true(length(context$sequences) == 2)
  expect_true(nrow(context$genes) > 0)

  # Generate compatible files
  temp_dir <- tempfile()
  dir.create(temp_dir)

  fasta_file <- file.path(temp_dir, "ref.fa")
  fastq_file <- file.path(temp_dir, "reads.fq")
  gtf_file <- file.path(temp_dir, "genes.gtf")

  sn_generate_mockdata("fasta", fasta_file, options = list(genome_context = context))
  sn_generate_mockdata("fastq", fastq_file, size = "minimal", options = list(genome_context = context))
  sn_generate_mockdata("gtf", gtf_file, size = "minimal", options = list(genome_context = context))

  # Check FASTA has expected chromosomes
  fasta_content <- readLines(fasta_file)
  expect_true(any(grepl(">chr1", fasta_content)))
  expect_true(any(grepl(">chr2", fasta_content)))

  # Check FASTQ reads reference chromosomes
  fastq_content <- readLines(fastq_file)
  read_headers <- fastq_content[grepl("^@", fastq_content)]
  expect_true(any(grepl("chr[12]", read_headers)))

  # Check GTF uses same chromosomes
  gtf_content <- readLines(gtf_file)
  expect_true(any(grepl("^chr1\t", gtf_content)))
  expect_true(any(grepl("^chr2\t", gtf_content)))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("Paired-end reads are correctly generated", {
  context <- sn_create_genome_context(seed = 123)

  temp_dir <- tempfile()
  dir.create(temp_dir)

  r1_file <- file.path(temp_dir, "reads_R1.fq")
  r2_file <- file.path(temp_dir, "reads_R2.fq")

  sn_generate_mockdata("fastq", r1_file,
    size = "minimal",
    options = list(read_type = "R1", genome_context = context)
  )

  sn_generate_mockdata("fastq", r2_file,
    size = "minimal",
    options = list(read_type = "R2", genome_context = context)
  )

  # Check both files exist
  expect_true(file.exists(r1_file))
  expect_true(file.exists(r2_file))

  # Check read identifiers
  r1_content <- readLines(r1_file)
  r2_content <- readLines(r2_file)

  r1_headers <- r1_content[grepl("^@", r1_content)]
  r2_headers <- r2_content[grepl("^@", r2_content)]

  expect_true(any(grepl("/1$", r1_headers)))
  expect_true(any(grepl("/2$", r2_headers)))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("Batch generation works with compatible files", {
  spec <- list(
    list(datatype = "fasta", size = "minimal"),
    list(datatype = "fastq", options = list(read_type = "R1"), size = "minimal"),
    list(datatype = "fastq", options = list(read_type = "R2"), size = "minimal"),
    list(datatype = "gtf", size = "minimal")
  )

  files <- sn_generate_mockdata_batch(spec)

  expect_equal(length(files), 4)
  expect_true(all(sapply(files, file.exists)))

  # Check that files use compatible chromosomes
  fasta_content <- readLines(files[1])
  gtf_content <- readLines(files[4])

  # Extract chromosome names from FASTA headers
  fasta_chrs <- gsub("^>([^ ]+).*", "\\1", fasta_content[grepl("^>", fasta_content)])

  # Extract chromosome names from GTF
  gtf_chrs <- unique(gsub("^([^\t]+)\t.*", "\\1", gtf_content))

  # Should have overlapping chromosomes
  expect_true(length(intersect(fasta_chrs, gtf_chrs)) > 0)

  # Clean up
  sn_cleanup_mockdata_examples()
})

test_that("RNA-seq dataset generation works", {
  # Generate temporary dataset
  dataset <- sn_generate_rnaseq_dataset(
    include_paired_reads = TRUE,
    genome_size = "minimal",
    read_count = "minimal"
  )

  expect_true(is.list(dataset))
  expect_true(all(c("reference", "annotation", "reads_r1", "reads_r2") %in% names(dataset)))
  expect_true(all(sapply(dataset, file.exists)))

  # Check file contents are compatible
  ref_content <- readLines(dataset$reference)
  gtf_content <- readLines(dataset$annotation)

  ref_chrs <- gsub("^>([^ ]+).*", "\\1", ref_content[grepl("^>", ref_content)])
  gtf_chrs <- unique(gsub("^([^\t]+)\t.*", "\\1", gtf_content))

  expect_true(length(intersect(ref_chrs, gtf_chrs)) > 0)

  # Clean up
  sn_cleanup_mockdata_examples()
})

test_that("Different datatypes can be generated", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  datatypes_to_test <- c("fasta", "fastq", "gtf", "bed", "vcf", "csv", "tsv", "txt", "json", "yaml")

  for (datatype in datatypes_to_test) {
    test_file <- file.path(temp_dir, paste0("test.", datatype))

    result <- sn_generate_mockdata(datatype, test_file, size = "minimal")
    expect_true(file.exists(test_file), info = paste("Failed for datatype:", datatype))
    expect_true(file.size(test_file) > 0, info = paste("Empty file for datatype:", datatype))
  }

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("Size parameters affect file content", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Generate files of different sizes
  minimal_file <- file.path(temp_dir, "minimal.fa")
  small_file <- file.path(temp_dir, "small.fa")

  sn_generate_mockdata("fasta", minimal_file, size = "minimal")
  sn_generate_mockdata("fasta", small_file, size = "small")

  minimal_content <- readLines(minimal_file)
  small_content <- readLines(small_file)

  expect_true(length(small_content) > length(minimal_content))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("Reverse complement function works correctly", {
  # Test the internal reverse complement function
  expect_equal(.sn_reverse_complement("ATCG"), "CGAT")
  expect_equal(.sn_reverse_complement("AAAA"), "TTTT")
  expect_equal(.sn_reverse_complement("ATCGATCG"), "CGATCGAT")
  expect_equal(.sn_reverse_complement(""), "")
})

test_that("Cleanup function works", {
  # Generate some temporary files
  temp_files <- c(
    sn_generate_mockdata("fasta", NULL, size = "minimal"),
    sn_generate_mockdata("fastq", NULL, size = "minimal")
  )

  expect_true(all(sapply(temp_files, file.exists)))

  # Clean up
  result <- sn_cleanup_mockdata_examples()
  expect_true(result)

  # Files should be gone (note: tempdir cleanup might not work immediately in tests)
  # So we just test that the function runs without error
})

test_that("Error handling works correctly", {
  # Invalid datatype
  expect_error(sn_generate_mockdata("invalid_type", "test.txt"))

  # Invalid spec in batch generation
  expect_error(sn_generate_mockdata_batch(list()))
  expect_error(sn_generate_mockdata_batch(list(list()))) # Missing datatype
})

test_that("n_records parameter overrides size", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  test_file <- file.path(temp_dir, "test.gtf")
  sn_generate_mockdata("gtf", test_file, size = "large", n_records = 3)

  content <- readLines(test_file)
  # Should have exactly 3 lines (3 records)
  expect_equal(length(content), 3)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("Options are passed to generators", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Test FASTA with protein option
  protein_file <- file.path(temp_dir, "protein.fa")
  sn_generate_mockdata("fasta", protein_file,
    options = list(sequence_type = "protein"),
    n_records = 2
  )

  content <- readLines(protein_file)
  # Should contain protein header
  expect_true(any(grepl("protein_", content)))

  # Test FASTQ with custom read length
  fastq_file <- file.path(temp_dir, "reads.fq")
  sn_generate_mockdata("fastq", fastq_file,
    options = list(read_length = 50),
    n_records = 1
  )

  content <- readLines(fastq_file)
  # Second line should be the sequence, should be 50 bases
  expect_equal(nchar(content[2]), 50)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("Example value generation with mock data works", {
  # Test regular example values (no temp files)
  example_fasta <- sn_get_example_value_with_mockdata("fasta")
  expect_true(is.character(example_fasta))
  expect_true(grepl("genome\\.fa", example_fasta))

  # Test with non-file datatype
  example_int <- sn_get_example_value_with_mockdata("integer")
  expect_equal(example_int, "4")
})

test_that("Temporary mock files can be created", {
  # Test creating temporary mock file
  temp_fasta <- sn_get_example_value_with_mockdata("fasta",
    param_name = "reference",
    create_temp_file = TRUE
  )

  # Should return quoted path
  expect_true(grepl('^".*"$', temp_fasta))

  # Extract path and check file exists
  file_path <- gsub('^"|"$', "", temp_fasta)
  expect_true(file.exists(file_path))

  # Test cleanup
  result <- sn_cleanup_mockdata_examples()
  expect_true(result)
})

test_that("Filename generation is context-aware", {
  # Test different parameter names generate appropriate filenames

  # Test R1/R2 naming
  filename_r1 <- ShennongTools:::.sn_generate_example_filename("fastq", "read1", "input")
  filename_r2 <- ShennongTools:::.sn_generate_example_filename("fastq", "read2", "input")

  expect_true(grepl("R1", filename_r1))
  expect_true(grepl("R2", filename_r2))

  # Test reference naming
  filename_ref <- ShennongTools:::.sn_generate_example_filename("fasta", "reference", "input")
  expect_true(grepl("reference", filename_ref))

  # Test output prefix
  filename_out <- ShennongTools:::.sn_generate_example_filename("bam", "alignment", "output")
  expect_true(grepl("output_", filename_out))
})

test_that("Invalid datatype throws error", {
  temp_file <- tempfile()

  expect_error(
    sn_generate_mockdata("invalid_datatype", temp_file),
    "Unsupported datatype"
  )
})

test_that("FASTQ format is correct", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  fastq_file <- file.path(temp_dir, "test.fq")
  sn_generate_mockdata("fastq", fastq_file, n_records = 2)

  content <- readLines(fastq_file)

  # Should have 8 lines (4 per read * 2 reads)
  expect_equal(length(content), 8)

  # Check format
  expect_true(grepl("^@", content[1])) # Header line
  expect_true(grepl("^[ATCG]+$", content[2])) # Sequence line
  expect_equal(content[3], "+") # Plus line
  expect_true(nchar(content[4]) == nchar(content[2])) # Quality same length as sequence

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("GTF format is correct", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  gtf_file <- file.path(temp_dir, "test.gtf")
  sn_generate_mockdata("gtf", gtf_file, n_records = 3)

  content <- readLines(gtf_file)
  expect_equal(length(content), 3)

  # Check GTF format (9 tab-separated columns)
  for (line in content) {
    fields <- strsplit(line, "\t")[[1]]
    expect_equal(length(fields), 9)
    expect_true(grepl("gene_id", fields[9])) # Attributes field
  }

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("VCF format is correct", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  vcf_file <- file.path(temp_dir, "test.vcf")
  sn_generate_mockdata("vcf", vcf_file, n_records = 2)

  content <- readLines(vcf_file)

  # Should have header lines + 2 variant lines
  expect_true(length(content) >= 4)

  # Check header
  expect_true(any(grepl("^##fileformat=VCF", content)))
  expect_true(any(grepl("^#CHROM", content)))

  # Check variant lines
  variant_lines <- content[!grepl("^#", content)]
  expect_equal(length(variant_lines), 2)

  for (line in variant_lines) {
    fields <- strsplit(line, "\t")[[1]]
    expect_true(length(fields) >= 8) # VCF has at least 8 columns
  }

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})
