# YAML specification for tool descriptions

This schema defines how to describe bioinformatics tools and their commands in a structured, machine-readable YAML format. It enables consistent parsing, validation, and execution (via shell or Python).

## Top-level fields

```yaml
tool_name: string          # Unique tool identifier (e.g., samtools)
description: string        # Short summary
citation: string           # DOI

environment:               # Conda environment specification
  channels: [channel1, ...]
  dependencies:
    - pkg=version
    - pip:
        - pip_pkg
```

## Command definition

Each command represents a callable unit (e.g., view, index).

```yaml
commands:
  <command_name>:
    description: string
    binary: string                      # REQUIRED: Executable name (e.g., samtools, hisat2-build)
    help_flag: string                   # REQUIRED: Help flag for the command (e.g., --help, -h)

    inputs:
      <name>:
        datatype: string | [string, ...]   # e.g., bam, fasta
        required: true | false
        description: string

    outputs:
      <name>:
        datatype: string | [string, ...]
        required: true | false
        description: string

    params:
      <name>:
        datatype: string  # string, integer, boolean, numeric
        default: value
        description: string

    shell: |              # Shell script template (Jinja2 syntax)
      command --opt {{ param }} > {{ output }}

    python: |             # Python script template (mutually exclusive with shell)
      import tool
      run(input="{{ input }}", out="{{ output }}")
```

## Required Fields

### binary
- **Required**: Yes
- **Purpose**: Specifies the exact executable name that will be called
- **Examples**: `samtools`, `hisat2-build`, `featureCounts`, `python`
- **Usage**: Used by the system to locate and execute the correct binary

### help_flag  
- **Required**: Yes
- **Purpose**: Specifies the help flag for the command to display usage information
- **Examples**: `--help`, `-h`, `help`, or empty string `""` if no help available
- **Usage**: Used by the system to provide help information to users

## Datatype System

The package uses a unified datatype system for validation and example generation:

### Global Datatypes
All datatypes are defined in `inst/config/datatypes.yaml` with the following structure:

```yaml
file_types:
  <datatype_name>:
    description: string
    extensions: [".ext1", ".ext2"]
    example_value: "path/to/example"

value_types:
  <datatype_name>:
    description: string
    example_value: "example_value"
```

### Tool-specific Datatypes
Individual tools can extend the global datatypes by creating `datatypes.yaml` in their tool directory:

```yaml
# inst/tools/<tool_name>/datatypes.yaml
file_types:
  custom_format:
    description: Tool-specific format
    extensions: [".custom"]
    example_value: "path/to/custom.file"
```

Tool-specific datatypes take precedence over global ones when there are naming conflicts.

## Templating (Use `jinjar`)

Supports:

- {{ var }} for substitution

- {% if condition %}...{% endif %} for conditional logic

## Best Practices

- Use lowercase for tool_name and command keys
- **Always include `binary` and `help_flag` fields for every command**
- Prefer explicit required: true for inputs/outputs
- Use either shell or python, not both
- Define tool-specific datatypes in tool directories when needed
- Use the global datatype registry for common file formats
- Include example_value in custom datatypes for better documentation
- Set `help_flag: ""` if the command doesn't support help options
