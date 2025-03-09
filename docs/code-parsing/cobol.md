# COBOL Code Parsing

The COBOL parser provides comprehensive analysis of COBOL source code, including divisions, sections, paragraphs, and data definitions.

## Features

- Division structure analysis
- Section and paragraph parsing
- Data hierarchy extraction
- Picture clause parsing
- File handling analysis
- Procedure division analysis

## COBOL Structure Elements

### Divisions
COBOL programs are organized into four main divisions:

1. IDENTIFICATION DIVISION
2. ENVIRONMENT DIVISION
3. DATA DIVISION
4. PROCEDURE DIVISION

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. DATAPROC.
```

Parsed as:
```json
{
    "type": "division",
    "name": "IDENTIFICATION",
    "elements": [
        {
            "type": "program_id",
            "name": "DATAPROC"
        }
    ]
}
```

### Data Items
```cobol
01 CUSTOMER-RECORD.
    05 CUSTOMER-ID       PIC 9(5).
    05 CUSTOMER-NAME     PIC X(30).
    05 CUSTOMER-ADDRESS.
        10 STREET       PIC X(20).
        10 CITY        PIC X(15).
        10 STATE       PIC X(2).
```

Parsed as:
```json
{
    "type": "data_item",
    "name": "CUSTOMER-RECORD",
    "level": 1,
    "items": [
        {
            "name": "CUSTOMER-ID",
            "level": 5,
            "picture": "9(5)"
        },
        {
            "name": "CUSTOMER-NAME",
            "level": 5,
            "picture": "X(30)"
        },
        {
            "name": "CUSTOMER-ADDRESS",
            "level": 5,
            "items": [
                {
                    "name": "STREET",
                    "level": 10,
                    "picture": "X(20)"
                }
            ]
        }
    ]
}
```

## Parsing Features

### Program Identification
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. DATAPROC.
AUTHOR. JOHN DOE.
DATE-WRITTEN. 2025-03-09.
```

### Environment Configuration
```cobol
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. X86-64.
OBJECT-COMPUTER. X86-64.
```

### File Control
```cobol
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMER.DAT"
    ORGANIZATION IS INDEXED
    ACCESS MODE IS DYNAMIC
    RECORD KEY IS CUSTOMER-ID.
```

### Data Division
```cobol
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
01 WS-COUNTER    PIC 9(4) VALUE ZEROS.
```

### Procedure Division
```cobol
PROCEDURE DIVISION.
MAIN-LOGIC SECTION.
    PERFORM PROCESS-DATA
    STOP RUN.

PROCESS-DATA.
    DISPLAY "Processing...".
```

## Usage Example

```python
from jsonparser import CodeParser

# Initialize parser
parser = CodeParser()

# COBOL code to parse
cobol_code = '''
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATAPROC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
           05 WS-ID    PIC 9(5).
           05 WS-NAME  PIC X(30).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
           DISPLAY WS-RECORD
           STOP RUN.
'''

# Define schema
schema = {
    "type": "code",
    "properties": {
        "elements": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "name": {"type": "string"},
                    "type": {"type": "string"},
                    "content": {"type": "string"}
                }
            }
        }
    }
}

# Parse code
result = parser.parse(cobol_code, schema)
```

## Implementation Details

The COBOL parser uses specialized analysis techniques:

1. **Format Handling**: 
   - Fixed format (columns 1-72)
   - Free format support
   - Continuation handling

2. **Division Analysis**:
   - Division boundary detection
   - Section identification
   - Paragraph parsing

3. **Data Analysis**:
   - Level number hierarchy
   - Picture clause parsing
   - Group item handling

4. **Procedure Analysis**:
   - Section and paragraph structure
   - Perform handling
   - Statement analysis

For more complex COBOL analysis needs, consider using specialized COBOL parsers or extending the current implementation.
