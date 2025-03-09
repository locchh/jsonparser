from jsonparser import ComplexityParser, JSONParser
import json

def compare_parsers():
    # Initialize both parsers
    standard_parser = JSONParser()
    complexity_parser = ComplexityParser(complexity_threshold=0.5)
    
    # Complex nested schema for a software project
    schema = {
        "type": "object",
        "properties": {
            "project": {
                "type": "object",
                "properties": {
                    "name": {"type": "string"},
                    "version": {"type": "string"},
                    "dependencies": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "name": {"type": "string"},
                                "version": {"type": "string"},
                                "requirements": {
                                    "type": "array",
                                    "items": {
                                        "type": "object",
                                        "properties": {
                                            "feature": {"type": "string"},
                                            "minimum_version": {"type": "string"},
                                            "optional": {"type": "boolean"}
                                        },
                                        "required": ["feature", "minimum_version"]
                                    }
                                }
                            },
                            "required": ["name", "version"]
                        }
                    },
                    "configuration": {
                        "type": "object",
                        "properties": {
                            "environment": {"type": "string"},
                            "settings": {
                                "type": "object",
                                "additionalProperties": {
                                    "type": "object",
                                    "properties": {
                                        "value": {"type": "string"},
                                        "encrypted": {"type": "boolean"}
                                    }
                                }
                            }
                        }
                    }
                },
                "required": ["name", "version"]
            }
        }
    }

    # Complex text to parse
    text = """
    Create a project called "AI Parser" version 2.0.0 with the following dependencies:
    1. OpenAI SDK version 1.0.0, which requires:
       - Python 3.8 or higher (not optional)
       - API key configuration (optional)
    2. JSONSchema version 4.17.0, which requires:
       - Python 3.7 or higher (not optional)
       - Draft 2020-12 support (optional)
    
    The project should be configured for a production environment with these settings:
    - API_KEY: "sk-..." (encrypted)
    - DEBUG_MODE: "false" (not encrypted)
    """

    print("Testing standard parser...")
    try:
        standard_result = standard_parser.parse(text, schema)
        print("\nStandard Parser Result:")
        print(json.dumps(standard_result, indent=2))
    except Exception as e:
        print(f"Standard parser error: {e}")

    print("\nTesting complexity-descent parser...")
    try:
        complexity_result = complexity_parser.parse(text, schema)
        print("\nComplexity Parser Result:")
        print(json.dumps(complexity_result, indent=2))
    except Exception as e:
        print(f"Complexity parser error: {e}")

if __name__ == "__main__":
    compare_parsers()
