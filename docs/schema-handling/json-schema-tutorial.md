# JSON Schema Tutorial

This tutorial provides a comprehensive guide to working with JSON schemas in the LLM JSON Parser library.

## Introduction to JSON Schema

JSON Schema is a vocabulary that allows you to annotate and validate JSON documents. In our library, it's used to:
1. Define expected data structures
2. Validate parsing results
3. Guide the LLM in generating structured output

## Basic Schema Structure

### Simple Types

```json
{
    "type": "string"
}
```

```json
{
    "type": "number"
}
```

```json
{
    "type": "boolean"
}
```

### Objects

```json
{
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "integer"},
        "email": {"type": "string", "format": "email"}
    },
    "required": ["name", "email"]
}
```

### Arrays

```json
{
    "type": "array",
    "items": {
        "type": "string"
    },
    "minItems": 1,
    "maxItems": 10
}
```

## Advanced Features

### 1. Combining Schemas

```json
{
    "anyOf": [
        {"type": "string"},
        {"type": "number"}
    ]
}
```

```json
{
    "allOf": [
        {"type": "object", "properties": {"name": {"type": "string"}}},
        {"type": "object", "properties": {"age": {"type": "integer"}}}
    ]
}
```

### 2. References

```json
{
    "definitions": {
        "address": {
            "type": "object",
            "properties": {
                "street": {"type": "string"},
                "city": {"type": "string"},
                "country": {"type": "string"}
            }
        }
    },
    "type": "object",
    "properties": {
        "shipping": {"$ref": "#/definitions/address"},
        "billing": {"$ref": "#/definitions/address"}
    }
}
```

### 3. Pattern Properties

```json
{
    "type": "object",
    "patternProperties": {
        "^S_": {"type": "string"},
        "^I_": {"type": "integer"}
    }
}
```

## Using Schemas with the Parser

### 1. Basic Parsing

```python
from jsonparser import JSONParser

schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "integer"}
    }
}

parser = JSONParser()
result = parser.parse("Create a person named John who is 30 years old", schema)
# Result: {"name": "John", "age": 30}
```

### 2. Nested Structures

```python
schema = {
    "type": "object",
    "properties": {
        "user": {
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "addresses": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "type": {"type": "string"},
                            "street": {"type": "string"},
                            "city": {"type": "string"}
                        }
                    }
                }
            }
        }
    }
}

text = """Create a user John with two addresses:
1. Home: 123 Main St, Boston
2. Work: 456 Park Ave, New York"""

result = parser.parse(text, schema)
```

### 3. Code Parsing Schemas

```python
code_schema = {
    "type": "code",
    "properties": {
        "elements": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "name": {"type": "string"},
                    "type": {"type": "string"},
                    "params": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "name": {"type": "string"},
                                "type": {"type": "string"}
                            }
                        }
                    }
                }
            }
        }
    }
}
```

## Best Practices

### 1. Schema Design

```json
{
    "type": "object",
    "properties": {
        "id": {
            "type": "string",
            "pattern": "^[A-Z]{2}[0-9]{6}$",
            "description": "User ID in format: XX999999"
        },
        "email": {
            "type": "string",
            "format": "email",
            "description": "User's email address"
        }
    }
}
```

### 2. Validation Options

```python
parser = JSONParser(
    strict_validation=True,
    additional_properties=False
)
```

### 3. Error Messages

```json
{
    "type": "object",
    "properties": {
        "age": {
            "type": "integer",
            "minimum": 0,
            "maximum": 120,
            "errorMessage": {
                "minimum": "Age cannot be negative",
                "maximum": "Age cannot exceed 120 years"
            }
        }
    }
}
```

## Common Patterns

### 1. Enums

```json
{
    "type": "string",
    "enum": ["draft", "published", "archived"]
}
```

### 2. Conditional Fields

```json
{
    "type": "object",
    "properties": {
        "type": {"type": "string", "enum": ["personal", "business"]},
        "company": {"type": "string"}
    },
    "required": ["type"],
    "if": {
        "properties": {"type": {"const": "business"}}
    },
    "then": {
        "required": ["company"]
    }
}
```

### 3. Complex Validation

```json
{
    "type": "object",
    "properties": {
        "start_date": {"type": "string", "format": "date"},
        "end_date": {"type": "string", "format": "date"}
    },
    "required": ["start_date", "end_date"],
    "dependencies": {
        "end_date": {
            "properties": {
                "end_date": {
                    "formatMinimum": {"$data": "1/start_date"}
                }
            }
        }
    }
}
```

For more examples and advanced usage, refer to the [Examples](../examples/README.md) section.
