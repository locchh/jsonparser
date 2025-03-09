from jsonparser import ComplexityParser
import json
from datetime import datetime

def demonstrate_advanced_parsing():
    parser = ComplexityParser(complexity_threshold=0.5)
    
    # Complex schema with various patterns
    schema = {
        "type": "object",
        "properties": {
            "organization": {
                "type": "object",
                "properties": {
                    "name": {"type": "string"},
                    "founded": {
                        "type": "string",
                        "description": "Organization founding date"
                    },
                    "headquarters": {
                        "type": "object",
                        "properties": {
                            "address": {"type": "string"},
                            "location": {
                                "type": "object",
                                "properties": {
                                    "latitude": {"type": "number"},
                                    "longitude": {"type": "number"},
                                    "altitude": {"type": "number"}
                                }
                            }
                        }
                    },
                    "contacts": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "name": {"type": "string"},
                                "email": {
                                    "type": "string",
                                    "description": "Contact email address"
                                },
                                "phone": {"type": "string"}
                            }
                        }
                    },
                    "software": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "name": {"type": "string"},
                                "version": {
                                    "type": "object",
                                    "properties": {
                                        "major": {"type": "integer"},
                                        "minor": {"type": "integer"},
                                        "patch": {"type": "integer"},
                                        "build": {"type": "string"}
                                    }
                                },
                                "release_date": {
                                    "type": "string",
                                    "description": "Software release date"
                                },
                                "dependencies": {
                                    "type": "array",
                                    "items": {
                                        "type": "object",
                                        "properties": {
                                            "name": {"type": "string"},
                                            "version_constraint": {"type": "string"}
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    # Complex text with various patterns to parse
    text = """
    The organization Codeium AI was founded on January 15, 2022. Their headquarters 
    is located at 123 Innovation Drive, Silicon Valley, CA 94025, with coordinates 
    37.4419° N, 122.1430° W at sea level.

    The organization has two main contacts:
    1. Sarah Johnson (sarah.johnson@codeium.ai, +1-555-0123)
    2. Michael Chen (michael.chen@codeium.ai, +1-555-0124)

    They maintain several software products:
    1. CodeAssist Pro v2.1.5-beta (released March 1, 2024)
       Dependencies:
       - TensorFlow >= 2.0.0
       - PyTorch >= 1.9.0
    
    2. AIFlow Framework v1.3.0-stable (released February 15, 2024)
       Dependencies:
       - NumPy >= 1.21.0
       - SciPy >= 1.7.0
    """

    print("Parsing complex organizational data...")
    try:
        result = parser.parse(text, schema)
        print("\nParsing Result:")
        print(json.dumps(result, indent=2))
        
        # Demonstrate pattern recognition
        print("\nRecognized Patterns:")
        for path, patterns in parser.schema_optimizer.recognized_patterns.items():
            print(f"\nPath: {path}")
            for pattern in patterns:
                print(f"- Type: {pattern.pattern_type}")
                print(f"  Properties: {', '.join(pattern.properties)}")
        
        # Show optimization hints
        print("\nOptimization Hints:")
        for path, hints in parser.schema_optimizer.optimization_hints.items():
            print(f"\nPath: {path}")
            for hint in hints:
                print(f"- {hint}")
                
    except Exception as e:
        print(f"Parsing error: {e}")

if __name__ == "__main__":
    demonstrate_advanced_parsing()
