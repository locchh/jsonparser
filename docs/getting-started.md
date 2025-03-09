# Getting Started with LLM JSON Parser

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/jsonparser.git
cd jsonparser
```

2. Install dependencies:
```bash
pip install -r requirements.txt
```

3. Set up your environment:
```bash
cp .env.example .env
# Edit .env with your OpenAI API key
```

## Basic Usage

### Simple Text to JSON

```python
from jsonparser import JSONParser

# Initialize parser
parser = JSONParser()

# Define schema
schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "integer"}
    }
}

# Parse text
text = "Create a user named John Doe who is 30 years old"
result = parser.parse(text, schema)
print(result)
# Output: {"name": "John Doe", "age": 30}
```

### Code Parsing

```python
from jsonparser import CodeParser

# Initialize parser
parser = CodeParser()

# Parse Python code
code = '''
def greet(name: str) -> str:
    return f"Hello, {name}!"
'''

schema = {
    "type": "code",
    "properties": {
        "elements": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "name": {"type": "string"},
                    "parameters": {"type": "array"},
                    "return_type": {"type": "string"}
                }
            }
        }
    }
}

result = parser.parse(code, schema)
```

## Configuration

### Environment Variables

- `OPENAI_API_KEY`: Your OpenAI API key (required)
- `MODEL_NAME`: OpenAI model to use (default: "gpt-4")
- `TEMPERATURE`: Model temperature (default: 0.1)

### Parser Configuration

#### JSONParser

```python
parser = JSONParser(
    api_key="your-api-key",  # Optional, will use env var if not provided
)
```

#### ComplexityParser

```python
from jsonparser import ComplexityParser

parser = ComplexityParser(
    api_key="your-api-key",
    complexity_threshold=0.6  # Threshold for splitting complex schemas
)
```

#### CodeParser

```python
from jsonparser import CodeParser

parser = CodeParser(
    api_key="your-api-key"
)
```

## Next Steps

- Check out the [Examples](examples/README.md) section for more usage scenarios
- Learn about [Schema Handling](schema-handling/README.md)
- Explore the [API Reference](api-reference/README.md)
