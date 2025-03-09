# LLM JSON Parser

A powerful tool that leverages Large Language Models (LLMs) to parse text and programming languages into JSON while adhering to specified JSON schemas.

## Features

- Parse natural language text into structured JSON
- Convert code snippets into JSON representations
- Validate output against JSON schemas
- Support for custom schema definitions
- Easy-to-use Python API

## Installation

```bash
pip install -r requirements.txt
```

## Quick Start

```python
from jsonparser import JSONParser

# Initialize parser
parser = JSONParser()

# Parse text with a schema
text = "Create a user named John Doe who is 30 years old"
schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "integer"}
    }
}

result = parser.parse(text, schema)
print(result)
# Output: {"name": "John Doe", "age": 30}
```

## License

See [LICENSE](LICENSE) for details.
