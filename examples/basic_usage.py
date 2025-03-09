import os
from dotenv import load_dotenv
from jsonparser import JSONParser

# Load API key from environment
load_dotenv()

def main():
    # Initialize parser
    parser = JSONParser()
    
    # Example 1: Parse a simple user description
    text = "Create a user named John Doe who is 30 years old and works as a software engineer"
    schema = {
        "type": "object",
        "properties": {
            "name": {"type": "string"},
            "age": {"type": "integer"},
            "occupation": {"type": "string"}
        },
        "required": ["name", "age", "occupation"]
    }
    
    try:
        result = parser.parse(text, schema)
        print("Example 1 - User Information:")
        print(result)
    except Exception as e:
        print(f"Error: {e}")
        
    # Example 2: Parse a code snippet into JSON
    code_text = """
    def greet(name: str, age: int) -> str:
        return f"Hello {name}, you are {age} years old!"
    """
    
    code_schema = {
        "type": "object",
        "properties": {
            "function_name": {"type": "string"},
            "parameters": {
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "name": {"type": "string"},
                        "type": {"type": "string"}
                    }
                }
            },
            "return_type": {"type": "string"}
        }
    }
    
    try:
        result = parser.parse(code_text, code_schema)
        print("\nExample 2 - Code Analysis:")
        print(result)
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    main()
