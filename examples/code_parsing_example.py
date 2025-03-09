from jsonparser import CodeParser
import json

def demonstrate_code_parsing():
    # Initialize parser
    parser = CodeParser()
    
    # Example Python code to parse
    code = '''
class DataProcessor:
    """A class for processing data with various methods."""
    
    @staticmethod
    def validate_input(data: dict) -> bool:
        """Validate input data format.
        
        Args:
            data: Input dictionary to validate
            
        Returns:
            bool: True if valid, False otherwise
        """
        return isinstance(data, dict)
    
    def process_data(self, data: dict, options: dict = None) -> dict:
        """Process the input data according to specified options.
        
        Args:
            data: Input data to process
            options: Processing options
            
        Returns:
            Processed data dictionary
        """
        if not self.validate_input(data):
            raise ValueError("Invalid input data")
        
        result = data.copy()
        # Process data here
        return result

def transform_data(input_data: list[dict]) -> dict:
    """Transform a list of dictionaries into a single dictionary.
    
    Args:
        input_data: List of dictionaries to transform
        
    Returns:
        Transformed data as a single dictionary
    """
    result = {}
    for item in input_data:
        result.update(item)
    return result
'''

    # Schema for code parsing
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
                        "decorators": {
                            "type": "array",
                            "items": {"type": "string"}
                        },
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
                        "return_type": {"type": "string"},
                        "parent_class": {"type": "string"},
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

    print("Parsing Python code...")
    try:
        result = parser.parse(code, schema)
        print("\nParsing Result:")
        print(json.dumps(result, indent=2))
        
        print("\nCode Structure Analysis:")
        for element in result["elements"]:
            print(f"\n{element['type'].upper()}: {element['name']}")
            if element.get("documentation"):
                print(f"Documentation: {element['documentation'].split()[0]}...")
            if element.get("parameters"):
                params = [f"{p['name']}: {p.get('type', 'any')}" for p in element["parameters"]]
                print(f"Parameters: {', '.join(params)}")
            if element.get("return_type"):
                print(f"Returns: {element['return_type']}")
            if element.get("decorators"):
                print(f"Decorators: {', '.join(element['decorators'])}")
            
    except Exception as e:
        print(f"Parsing error: {e}")

if __name__ == "__main__":
    demonstrate_code_parsing()
