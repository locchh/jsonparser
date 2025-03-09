# API Reference

This section provides detailed API documentation for all public classes and methods in the LLM JSON Parser library.

## Core Classes

### JSONParser

Base class for all parsing operations.

```python
class JSONParser:
    def __init__(self, api_key: Optional[str] = None):
        """Initialize JSON parser.
        
        Args:
            api_key: Optional OpenAI API key. If not provided, will use environment variable.
        """
        pass

    def parse(self, text: str, schema: Dict[str, Any]) -> Dict[str, Any]:
        """Parse text into JSON according to schema.
        
        Args:
            text: Text to parse
            schema: JSON schema defining the expected structure
            
        Returns:
            Parsed JSON matching the schema
            
        Raises:
            ValueError: If parsing fails or result doesn't match schema
        """
        pass
```

### ComplexityParser

Enhanced parser for handling complex nested structures.

```python
class ComplexityParser(JSONParser):
    def __init__(self, api_key: Optional[str] = None, complexity_threshold: float = 0.6):
        """Initialize complexity parser.
        
        Args:
            api_key: Optional OpenAI API key
            complexity_threshold: Threshold for splitting complex schemas (0.0 to 1.0)
        """
        pass

    def parse(self, text: str, schema: Dict[str, Any]) -> Dict[str, Any]:
        """Parse text using complexity-descent methodology.
        
        Args:
            text: Text to parse
            schema: JSON schema defining the expected structure
            
        Returns:
            Parsed JSON matching the schema
        """
        pass
```

### CodeParser

Specialized parser for programming languages.

```python
class CodeParser(JSONParser):
    def __init__(self, api_key: Optional[str] = None):
        """Initialize code parser.
        
        Args:
            api_key: Optional OpenAI API key
        """
        pass

    def parse_code(self, code: str, schema: Dict[str, Any]) -> Dict[str, Any]:
        """Parse code into JSON according to schema.
        
        Args:
            code: Source code to parse
            schema: JSON schema defining the expected structure
            
        Returns:
            Parsed JSON representing the code structure
        """
        pass
```

## Language Analyzers

### JavaAnalyzer

Analyzes Java code structure.

```python
class JavaAnalyzer:
    def analyze_code(self, code: str) -> List[CodeElement]:
        """Analyze Java code and extract its elements.
        
        Args:
            code: Java source code
            
        Returns:
            List of CodeElement objects representing the code structure
        """
        pass
```

### CobolAnalyzer

Analyzes COBOL code structure.

```python
class CobolAnalyzer:
    def analyze_code(self, code: str) -> List[CodeElement]:
        """Analyze COBOL code and extract its elements.
        
        Args:
            code: COBOL source code
            
        Returns:
            List of CodeElement objects representing the code structure
        """
        pass
```

## Data Classes

### CodeElement

Represents a code element with its metadata.

```python
@dataclass
class CodeElement:
    name: str                    # Element name
    element_type: str           # Type (class, function, etc.)
    content: str                # Raw content
    start_line: int            # Starting line number
    end_line: int              # Ending line number
    docstring: Optional[str]    # Documentation string
    decorators: List[str]      # List of decorators/annotations
    params: List[Dict[str, str]]  # Parameters
    return_type: Optional[str]  # Return type if applicable
    parent: Optional[str]       # Parent element name
```

## Utility Functions

### Schema Optimization

```python
def optimize_schema(schema: Dict[str, Any]) -> Dict[str, Any]:
    """Optimize schema for better parsing performance.
    
    Args:
        schema: Original JSON schema
        
    Returns:
        Optimized schema with additional constraints and hints
    """
    pass
```

### Language Detection

```python
def detect_language(code: str) -> CodeLanguage:
    """Detect programming language from code content.
    
    Args:
        code: Source code
        
    Returns:
        Detected CodeLanguage enum value
    """
    pass
```

For detailed examples and usage patterns, see the [Examples](../examples/README.md) section.
