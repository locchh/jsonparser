from jsonparser import CodeParser
import json

def demonstrate_java_parsing():
    # Initialize parser
    parser = CodeParser()
    
    # Example Java code to parse
    java_code = '''
package com.example.processor;

import java.util.List;
import java.util.Map;

/**
 * A data processor for handling various types of data transformations.
 * @author John Doe
 * @version 1.0
 */
@Service
public class DataProcessor {
    private final Logger logger;
    
    /**
     * Processes input data according to specified rules.
     * @param data The input data to process
     * @param options Processing options
     * @return Processed data as a map
     * @throws IllegalArgumentException if data is invalid
     */
    @Override
    public Map<String, Object> processData(List<String> data, 
                                         Map<String, String> options) 
        throws IllegalArgumentException {
        
        if (data == null || data.isEmpty()) {
            throw new IllegalArgumentException("Data cannot be null or empty");
        }
        
        return transformData(data, options);
    }
    
    private Map<String, Object> transformData(List<String> data,
                                            Map<String, String> options) {
        // Implementation
        return new HashMap<>();
    }
}
'''

    # Schema for Java code parsing
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
                        },
                        "modifiers": {
                            "type": "object",
                            "properties": {
                                "visibility": {"type": "string"},
                                "is_static": {"type": "boolean"},
                                "is_final": {"type": "boolean"}
                            }
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
                        "throws": {
                            "type": "array",
                            "items": {"type": "string"}
                        },
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

    print("Parsing Java code...")
    try:
        result = parser.parse(java_code, schema)
        print("\nParsing Result:")
        print(json.dumps(result, indent=2))
        
        print("\nCode Structure Analysis:")
        for element in result["elements"]:
            print(f"\n{element['type'].upper()}: {element['name']}")
            if element.get("documentation"):
                print(f"Documentation: {element['documentation'].split()[0]}...")
            if element.get("annotations"):
                print(f"Annotations: {', '.join(element['annotations'])}")
            if element.get("parameters"):
                params = [f"{p['name']}: {p['type']}" for p in element["parameters"]]
                print(f"Parameters: {', '.join(params)}")
            if element.get("return_type"):
                print(f"Returns: {element['return_type']}")
            if element.get("throws"):
                print(f"Throws: {', '.join(element['throws'])}")
            
    except Exception as e:
        print(f"Parsing error: {e}")

if __name__ == "__main__":
    demonstrate_java_parsing()
