# Complexity Parser

The Complexity Parser is a specialized component designed to handle complex, deeply nested JSON schemas by breaking them down into manageable chunks and optimizing the parsing process.

## Overview

When dealing with complex JSON structures, direct parsing can be challenging and error-prone. The Complexity Parser addresses this by:

1. Analyzing schema complexity
2. Breaking down complex schemas into simpler sub-schemas
3. Parsing each sub-schema independently
4. Recombining results into the final structure

## How It Works

### 1. Complexity Analysis

```python
from jsonparser import ComplexityParser

parser = ComplexityParser(complexity_threshold=0.6)
```

The parser evaluates schema complexity based on:
- Nesting depth
- Number of properties
- Property type complexity
- Array item complexity
- Reference dependencies

### 2. Schema Decomposition

For complex schemas, the parser:

1. Identifies independent sub-schemas
2. Creates parsing boundaries
3. Maintains reference relationships

Example:
```python
complex_schema = {
    "type": "object",
    "properties": {
        "user": {
            "type": "object",
            "properties": {
                "profile": {
                    "type": "object",
                    "properties": {
                        "details": { ... },
                        "preferences": { ... }
                    }
                },
                "history": {
                    "type": "array",
                    "items": { ... }
                }
            }
        }
    }
}

# Decomposed into:
sub_schemas = [
    {"user.profile.details": {...}},
    {"user.profile.preferences": {...}},
    {"user.history": {...}}
]
```

### 3. Optimized Parsing

```python
# Parse complex nested structure
result = parser.parse(text, complex_schema)
```

The parser:
1. Processes each sub-schema independently
2. Uses parallel processing when possible
3. Maintains context between sub-schemas
4. Validates intermediate results

## Advanced Features

### 1. Schema Optimization

```python
parser.optimize_schema(schema)
```

- Flattens unnecessary nesting
- Combines similar patterns
- Identifies reusable components

### 2. Reference Handling

```python
{
    "definitions": {
        "address": {
            "type": "object",
            "properties": {
                "street": {"type": "string"},
                "city": {"type": "string"}
            }
        }
    },
    "properties": {
        "shipping": {"$ref": "#/definitions/address"},
        "billing": {"$ref": "#/definitions/address"}
    }
}
```

The parser:
- Resolves references before decomposition
- Maintains reference integrity
- Optimizes repeated structures

### 3. Context Preservation

```python
parser = ComplexityParser(
    preserve_context=True,
    context_depth=2
)
```

Maintains parsing context across:
- Parent-child relationships
- Sibling properties
- Reference chains

## Best Practices

1. **Schema Design**
   ```python
   # Prefer
   {
       "type": "object",
       "properties": {
           "user": {"$ref": "#/definitions/user"},
           "orders": {"$ref": "#/definitions/orders"}
       }
   }
   
   # Instead of
   {
       "type": "object",
       "properties": {
           "user": {
               "type": "object",
               "properties": { ... }
           },
           "orders": {
               "type": "object",
               "properties": { ... }
           }
       }
   }
   ```

2. **Complexity Threshold**
   ```python
   # Adjust based on your needs
   parser = ComplexityParser(
       complexity_threshold=0.7,  # Higher = more aggressive splitting
       min_chunk_size=5          # Minimum properties per chunk
   )
   ```

3. **Performance Optimization**
   ```python
   # Enable caching for repeated patterns
   parser = ComplexityParser(
       enable_cache=True,
       cache_size=1000
   )
   ```

## Error Handling

```python
try:
    result = parser.parse(text, schema)
except SchemaComplexityError as e:
    print(f"Schema too complex: {e.complexity_score}")
    print(f"Suggested split points: {e.split_suggestions}")
except ValidationError as e:
    print(f"Validation failed: {e.validation_errors}")
```

## Integration with Other Components

The Complexity Parser works seamlessly with:
- Schema Optimizer
- Code Parser
- Base JSON Parser

Example:
```python
from jsonparser import ComplexityParser, SchemaOptimizer

optimizer = SchemaOptimizer()
parser = ComplexityParser()

# Optimize and parse
optimized_schema = optimizer.optimize(complex_schema)
result = parser.parse(text, optimized_schema)
```
