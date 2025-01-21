Here’s an example of a **universal JSON schema** for representing **Data Types** (both fundamental and complex) across **COBOL**, **C++**, **Java**, and **Python**:  

---

### **Universal JSON Schema for Data Types**  

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "Universal Data Types Schema",
  "type": "object",
  "properties": {
    "language": {
      "type": "string",
      "enum": ["COBOL", "C++", "Java", "Python"],
      "description": "The programming language for which the data type is defined."
    },
    "dataTypes": {
      "type": "array",
      "description": "List of data types supported by the language.",
      "items": {
        "type": "object",
        "properties": {
          "name": {
            "type": "string",
            "description": "The name of the data type."
          },
          "category": {
            "type": "string",
            "enum": ["primitive", "complex"],
            "description": "Whether the data type is primitive or complex."
          },
          "description": {
            "type": "string",
            "description": "A brief description of the data type."
          },
          "size": {
            "type": ["integer", "null"],
            "description": "The size of the data type in bytes (if applicable)."
          },
          "examples": {
            "type": "array",
            "description": "Examples of the data type usage.",
            "items": {
              "type": "string"
            }
          },
          "relatedKeywords": {
            "type": "array",
            "description": "Keywords associated with the data type in the given language.",
            "items": {
              "type": "string"
            }
          }
        },
        "required": ["name", "category"]
      }
    }
  },
  "required": ["language", "dataTypes"]
}
```  

---

### **Example Instances for Each Language**  

#### **1. COBOL**  
```json
{
  "language": "COBOL",
  "dataTypes": [
    {
      "name": "PIC 9",
      "category": "primitive",
      "description": "Numeric data type for digits.",
      "size": null,
      "examples": ["01 VAR1 PIC 9(5)."],
      "relatedKeywords": ["PIC", "9", "Numeric"]
    },
    {
      "name": "PIC X",
      "category": "primitive",
      "description": "Alphanumeric data type for characters.",
      "size": null,
      "examples": ["01 VAR2 PIC X(10)."],
      "relatedKeywords": ["PIC", "X", "Alphanumeric"]
    }
  ]
}
```  

#### **2. C++**  
```json
{
  "language": "C++",
  "dataTypes": [
    {
      "name": "int",
      "category": "primitive",
      "description": "Integer data type.",
      "size": 4,
      "examples": ["int x = 42;"],
      "relatedKeywords": ["int", "signed", "numeric"]
    },
    {
      "name": "std::vector",
      "category": "complex",
      "description": "Dynamic array-like data structure.",
      "size": null,
      "examples": ["std::vector<int> vec = {1, 2, 3};"],
      "relatedKeywords": ["vector", "std", "container"]
    }
  ]
}
```  

#### **3. Java**  
```json
{
  "language": "Java",
  "dataTypes": [
    {
      "name": "int",
      "category": "primitive",
      "description": "32-bit signed integer.",
      "size": 4,
      "examples": ["int num = 100;"],
      "relatedKeywords": ["int", "numeric", "primitive"]
    },
    {
      "name": "ArrayList",
      "category": "complex",
      "description": "Resizable array implementation.",
      "size": null,
      "examples": ["ArrayList<String> list = new ArrayList<>();"],
      "relatedKeywords": ["ArrayList", "Collection", "List"]
    }
  ]
}
```  

#### **4. Python**  
```json
{
  "language": "Python",
  "dataTypes": [
    {
      "name": "int",
      "category": "primitive",
      "description": "Integer type (arbitrary precision).",
      "size": null,
      "examples": ["x = 123"],
      "relatedKeywords": ["int", "integer", "numeric"]
    },
    {
      "name": "list",
      "category": "complex",
      "description": "Ordered, mutable collection of elements.",
      "size": null,
      "examples": ["my_list = [1, 2, 3]"],
      "relatedKeywords": ["list", "array", "collection"]
    }
  ]
}
```  

---

### **How This Schema Works**  

- **Extensible**: New languages and data types can be added easily.  
- **Reusable Components**: Shared attributes like `name`, `size`, and `examples` ensure consistency.  
- **Validation Ready**: Conforms to the JSON Schema Draft 2020-12 standard for interoperability.  

Let me know if you need more details or specific extensions!
