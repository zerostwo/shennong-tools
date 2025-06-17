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

### Critical Jinjar Rules

**1. Optional Parameter Handling**
- Optional inputs/outputs are automatically set to `NULL` when not provided by the user
- Empty strings (`""`) are considered **truthy** in jinjar, so use proper NULL handling
- Template system automatically handles this conversion for `required: false` parameters

**2. YAML Syntax Requirements**
- Use YAML folded scalar syntax (`>`) for multi-line shell commands
- **Never use backslash line continuation (`\`)** - jinjar preserves backslashes literally
- For multi-line commands, use folded scalar format and separate arguments logically

**3. Conditional Logic Patterns**
```yaml
# ✅ Correct - simple conditional for optional parameters
shell: >
  {{ binary }} -i {{ input1 }}
  {% if input2 %} -I {{ input2 }}{% endif %}
  -o {{ output1 }}
  {% if output2 %} -O {{ output2 }}{% endif %}

# ✅ Correct - handling paired vs single-end reads
shell: >
  {{ binary }} -x {{ index }}
  {% if read2 %}-1 {{ read1 }} -2 {{ read2 }}{% else %}-U {{ read1 }}{% endif %}
  | samtools view -Sbh -o {{ bam }}
```

**4. Common Pitfalls to Avoid**
```yaml
# ❌ Incorrect - backslash continuation
shell: |
  command --option1 {{ param1 }} \
    --option2 {{ param2 }} \
    {{ input }} {{ output }}

# ❌ Incorrect - empty string checking (not needed)
shell: >
  {{ binary }}
  {% if param and param != "" %} --param {{ param }}{% endif %}

# ✅ Correct - simple existence check is sufficient
shell: >
  {{ binary }}
  {% if param %} --param {{ param }}{% endif %}
```

**5. Binary Parameter**
- All templates automatically receive a `binary` parameter containing the command's binary value
- Always use `{{ binary }}` at the start of shell templates instead of hardcoding the executable name

**6. Template Debugging**
- Use `dry_run = TRUE` in `sn_run()` to test template rendering without execution
- Check rendered commands for correct parameter inclusion/exclusion
- Verify optional parameters are properly omitted when not provided

## Parameter Standardization

### Threading Parameters
- **Always use `threads`** as the parameter name for threading/parallelization options
- Map to tool-specific flags in the shell template (e.g., `--num_workers`, `-@`, `-p`)
- Example:
```yaml
params:
  threads:
    datatype: integer
    default: 4
    description: Number of threads to use

shell: >
  {{ binary }} --num_workers {{ threads }} {{ input }} {{ output }}
```

### Extra Parameters
- **Always include `extras` parameter** unless all possible parameters are explicitly defined
- This allows users to pass additional tool-specific arguments not covered in the YAML
- Use empty string as default value
- Example:
```yaml
params:
  extras:
    datatype: string
    default: ""
    description: "Additional arguments to pass to the tool command."

shell: >
  {{ binary }} {{ input }} {{ output }} {{ extras }}
```

## Best Practices

- Use lowercase for tool_name and command keys
- **Always include `binary` and `help_flag` fields for every command**
- Prefer explicit required: true for inputs/outputs
- Use either shell or python, not both
- Define tool-specific datatypes in tool directories when needed
- Use the global datatype registry for common file formats
- Include example_value in custom datatypes for better documentation
- Set `help_flag: ""` if the command doesn't support help options
