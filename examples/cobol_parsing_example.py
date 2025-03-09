from jsonparser import CodeParser
import json

def demonstrate_cobol_parsing():
    # Initialize parser
    parser = CodeParser()
    
    # Example COBOL code to parse
    cobol_code = '''
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATAPROCESSOR.
       AUTHOR. JOHN DOE.
       DATE-WRITTEN. 2025-03-09.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. X86-64.
       OBJECT-COMPUTER. X86-64.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMER.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CUSTOMER-ID.
           
       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID       PIC 9(5).
           05 CUSTOMER-NAME     PIC X(30).
           05 CUSTOMER-ADDRESS.
               10 STREET        PIC X(20).
               10 CITY         PIC X(15).
               10 STATE        PIC X(2).
               10 ZIP-CODE     PIC 9(5).
           05 CUSTOMER-BALANCE  PIC 9(7)V99.
           
       WORKING-STORAGE SECTION.
       01 WS-CURRENT-DATE.
           05 WS-YEAR          PIC 9(4).
           05 WS-MONTH         PIC 9(2).
           05 WS-DAY           PIC 9(2).
           
       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
           
       PROCESS-CUSTOMER.
           OPEN INPUT CUSTOMER-FILE
           PERFORM UNTIL EOF
               READ CUSTOMER-FILE
                   AT END SET EOF TO TRUE
                   NOT AT END PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE
           STOP RUN.
           
       PROCESS-RECORD.
           IF CUSTOMER-BALANCE > 1000
               PERFORM GENERATE-REPORT
           END-IF.
           
       GENERATE-REPORT.
           DISPLAY CUSTOMER-NAME
           DISPLAY CUSTOMER-BALANCE.
    '''

    # Schema for COBOL code parsing
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
                        "content": {"type": "string"},
                        "location": {
                            "type": "object",
                            "properties": {
                                "start_line": {"type": "integer"},
                                "end_line": {"type": "integer"}
                            }
                        },
                        "parent": {"type": "string"},
                        "data_items": {
                            "type": "array",
                            "items": {
                                "type": "object",
                                "properties": {
                                    "level": {"type": "integer"},
                                    "name": {"type": "string"},
                                    "picture": {"type": "string"},
                                    "parent": {"type": "string"}
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    print("Parsing COBOL code...")
    try:
        result = parser.parse(cobol_code, schema)
        print("\nParsing Result:")
        print(json.dumps(result, indent=2))
        
        print("\nCode Structure Analysis:")
        for element in result["elements"]:
            print(f"\n{element['type'].upper()}: {element['name']}")
            
            if element['type'] == 'division':
                print(f"Content Summary: {element['content'].split()[0]}...")
                if element.get('data_items'):
                    print("\nData Items:")
                    for item in element['data_items']:
                        indent = "  " * (item['level'] // 5)
                        print(f"{indent}{item['level']} {item['name']}", end='')
                        if item.get('picture'):
                            print(f" PIC {item['picture']}")
                        else:
                            print()
            
            elif element['type'] == 'section':
                print(f"Parent Division: {element.get('parent', 'None')}")
                print(f"Content Summary: {element['content'].split()[0]}...")
            
            elif element['type'] == 'paragraph':
                print(f"Parent Section: {element.get('parent', 'None')}")
                print(f"Content: {element['content'].strip()}")
            
    except Exception as e:
        print(f"Parsing error: {e}")

if __name__ == "__main__":
    demonstrate_cobol_parsing()
