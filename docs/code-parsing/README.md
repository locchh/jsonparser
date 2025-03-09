# Code Parsing Documentation

The Code Parsing feature allows you to convert source code from various programming languages into structured JSON format while preserving code semantics and structure.

## Supported Languages

1. [Python](python.md)
2. [Java](java.md)
3. [COBOL](cobol.md)

## Common Features

Each language parser provides:
- AST or token-based analysis
- Symbol extraction
- Type information
- Documentation parsing
- Structure analysis

## Basic Usage

```python
from jsonparser import CodeParser

parser = CodeParser()
result = parser.parse(code, schema)
```

## Language-Specific Features

### Python
- AST-based parsing
- Type hint extraction
- Docstring parsing
- Decorator handling

### Java
- Class hierarchy analysis
- JavaDoc parsing
- Annotation processing
- Package structure

### COBOL
- Division structure
- Data hierarchy
- Procedure analysis
- Picture clause parsing

## Code Element Structure

All parsed code elements follow this basic structure:

```json
{
    "type": "code",
    "language": "python|java|cobol",
    "elements": [
        {
            "name": "element_name",
            "type": "class|function|method|division|etc",
            "documentation": "element_documentation",
            "location": {
                "start_line": 1,
                "end_line": 10
            },
            "parameters": [...],
            "return_type": "type_if_applicable",
            "parent": "parent_element_if_applicable"
        }
    ]
}
```

## Schema Definition

When parsing code, you need to provide a schema that defines the expected JSON structure. Here's a basic example:

```python
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
                    "documentation": {"type": "string"},
                    "location": {
                        "type": "object",
                        "properties": {
                            "start_line": {"type": "integer"},
                            "end_line": {"type": "integer"}
                        }
                    }
                }
            }
        }
    }
}
```

## Extension

To add support for a new programming language:

1. Create a new analyzer class:
```python
class NewLanguageAnalyzer:
    def analyze_code(self, code: str) -> List[CodeElement]:
        # Implement language-specific analysis
        pass
```

2. Add language to CodeLanguage enum:
```python
class CodeLanguage(Enum):
    NEW_LANGUAGE = "new_language"
```

3. Update CodeParser to handle the new language:
```python
def parse_code(self, code: str, schema: Dict[str, Any]) -> Dict[str, Any]:
    if language == CodeLanguage.NEW_LANGUAGE:
        elements = self.new_language_analyzer.analyze_code(code)
```

For detailed documentation on each supported language, click the respective links above.
