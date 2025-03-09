# Java Code Parsing

The Java parser provides comprehensive analysis of Java source code, including class structures, methods, fields, and annotations.

## Features

- Package and import analysis
- Class hierarchy parsing
- Method signature extraction
- JavaDoc documentation
- Annotation processing
- Access modifier handling
- Generic type support

## Java Element Types

### Classes
```java
@Service
public class DataProcessor {
    // Class content
}
```

Parsed as:
```json
{
    "type": "class",
    "name": "DataProcessor",
    "annotations": ["Service"],
    "modifiers": {
        "visibility": "public",
        "is_final": false
    }
}
```

### Methods
```java
/**
 * Process data with options.
 * @param data Input data
 * @param options Processing options
 * @return Processed result
 */
@Override
public Map<String, Object> processData(List<String> data, 
                                     Map<String, String> options) {
    // Method content
}
```

Parsed as:
```json
{
    "type": "method",
    "name": "processData",
    "documentation": "Process data with options.",
    "annotations": ["Override"],
    "modifiers": {
        "visibility": "public",
        "is_static": false
    },
    "parameters": [
        {
            "name": "data",
            "type": "List<String>"
        },
        {
            "name": "options",
            "type": "Map<String, String>"
        }
    ],
    "return_type": "Map<String, Object>"
}
```

## Parsing Features

### Package Detection
```java
package com.example.processor;
```

### Import Analysis
```java
import java.util.List;
import static java.util.Collections.emptyList;
```

### JavaDoc Parsing
```java
/**
 * Class description.
 * @author John Doe
 * @version 1.0
 */
```

### Annotation Processing
```java
@Service
@Deprecated
public class MyClass {
}
```

### Generic Types
```java
public class Container<T extends Comparable<T>> {
}
```

## Usage Example

```python
from jsonparser import CodeParser

# Initialize parser
parser = CodeParser()

# Java code to parse
java_code = '''
package com.example;

import java.util.List;

/**
 * Data processor class.
 */
@Service
public class DataProcessor {
    private final String name;
    
    public String process(List<String> data) {
        return "Processed: " + data.size();
    }
}
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
                    "documentation": {"type": "string"},
                    "annotations": {
                        "type": "array",
                        "items": {"type": "string"}
                    }
                }
            }
        }
    }
}

# Parse code
result = parser.parse(java_code, schema)
```

## Implementation Details

The Java parser uses a combination of regular expressions and pattern matching to analyze Java code structure. Key components include:

1. **JavaAnalyzer**: Main class for Java code analysis
2. **Pattern Matching**: Regular expressions for Java syntax
3. **Symbol Resolution**: Tracks scope and visibility
4. **Type Analysis**: Handles generic types and type bounds

For more complex analysis needs, consider using a full Java parser like JavaParser or ANTLR.
