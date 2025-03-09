from typing import Dict, Any, List, Optional, Union, Set
import ast
import re
from dataclasses import dataclass
from enum import Enum
import tokenize
from io import StringIO
import json
from .parser import JSONParser
from .languages.java_analyzer import JavaAnalyzer
from .languages.cobol_analyzer import CobolAnalyzer

class CodeLanguage(Enum):
    """Supported programming languages."""
    PYTHON = "python"
    JAVASCRIPT = "javascript"
    TYPESCRIPT = "typescript"
    JAVA = "java"
    COBOL = "cobol"

@dataclass
class CodeElement:
    """Represents a code element with its metadata."""
    name: str
    element_type: str
    content: str
    start_line: int
    end_line: int
    docstring: Optional[str] = None
    decorators: List[str] = None
    params: List[Dict[str, str]] = None
    return_type: Optional[str] = None
    parent: Optional[str] = None

class CodeAnalyzer:
    """Analyzes code structure and extracts relevant information."""

    def __init__(self):
        self._language_patterns = {
            CodeLanguage.PYTHON: {
                'class': r'class\s+(\w+)(?:\([^)]*\))?:',
                'function': r'def\s+(\w+)\s*\([^)]*\)\s*(?:->\s*[^:]+)?:',
                'import': r'(?:from\s+[\w.]+\s+)?import\s+[\w.]+(?:\s+as\s+\w+)?',
                'decorator': r'@[\w.]+(?:\([^)]*\))?'
            },
            CodeLanguage.JAVASCRIPT: {
                'class': r'class\s+(\w+)(?:\s+extends\s+\w+)?{',
                'function': r'(?:function\s+(\w+)|const\s+(\w+)\s*=\s*(?:async\s*)?function)',
                'import': r'import\s+.*\s+from\s+[\'"].*[\'"]',
                'decorator': r'@[\w.]+(?:\([^)]*\))?'
            }
        }

    def detect_language(self, code: str) -> CodeLanguage:
        """Detect the programming language from code content."""
        # Python detection
        if re.search(r'def\s+\w+\s*\(.*\):', code):
            return CodeLanguage.PYTHON
            
        # Java detection
        if re.search(r'public\s+class|class\s+\w+\s*{', code):
            return CodeLanguage.JAVA
            
        # COBOL detection
        if re.search(r'IDENTIFICATION\s+DIVISION|PROCEDURE\s+DIVISION', code, re.IGNORECASE):
            return CodeLanguage.COBOL
            
        # JavaScript detection
        if re.search(r'function\s+\w+|const\s+\w+\s*=\s*function', code):
            return CodeLanguage.JAVASCRIPT
            
        return CodeLanguage.PYTHON  # Default to Python

    def analyze_python_code(self, code: str) -> List[CodeElement]:
        """Analyze Python code using AST."""
        elements = []
        try:
            tree = ast.parse(code)
            for node in ast.walk(tree):
                if isinstance(node, ast.ClassDef):
                    elements.append(CodeElement(
                        name=node.name,
                        element_type='class',
                        content=self._get_node_source(code, node),
                        start_line=node.lineno,
                        end_line=node.end_lineno,
                        docstring=ast.get_docstring(node),
                        decorators=[d.id for d in node.decorator_list if isinstance(d, ast.Name)],
                        params=[{'name': base.id} for base in node.bases if isinstance(base, ast.Name)]
                    ))
                elif isinstance(node, ast.FunctionDef):
                    elements.append(CodeElement(
                        name=node.name,
                        element_type='function',
                        content=self._get_node_source(code, node),
                        start_line=node.lineno,
                        end_line=node.end_lineno,
                        docstring=ast.get_docstring(node),
                        decorators=[d.id for d in node.decorator_list if isinstance(d, ast.Name)],
                        params=self._extract_function_params(node),
                        return_type=self._get_return_annotation(node),
                        parent=self._get_parent_class(node)
                    ))
        except SyntaxError:
            pass  # Handle invalid Python code
        return elements

    def _get_node_source(self, code: str, node: ast.AST) -> str:
        """Extract source code for a node."""
        lines = code.split('\n')
        return '\n'.join(lines[node.lineno-1:node.end_lineno])

    def _extract_function_params(self, node: ast.FunctionDef) -> List[Dict[str, str]]:
        """Extract function parameters with type annotations."""
        params = []
        for arg in node.args.args:
            param = {'name': arg.arg}
            if arg.annotation:
                param['type'] = self._annotation_to_string(arg.annotation)
            params.append(param)
        return params

    def _annotation_to_string(self, annotation: ast.AST) -> str:
        """Convert type annotation AST to string."""
        if isinstance(annotation, ast.Name):
            return annotation.id
        elif isinstance(annotation, ast.Subscript):
            return f"{annotation.value.id}[{self._annotation_to_string(annotation.slice)}]"
        return str(annotation)

    def _get_return_annotation(self, node: ast.FunctionDef) -> Optional[str]:
        """Get function return type annotation."""
        if node.returns:
            return self._annotation_to_string(node.returns)
        return None

    def _get_parent_class(self, node: ast.FunctionDef) -> Optional[str]:
        """Get the parent class name for a method."""
        parent = getattr(node, 'parent', None)
        if isinstance(parent, ast.ClassDef):
            return parent.name
        return None

class CodeParser(JSONParser):
    """Specialized parser for converting code into JSON."""

    def __init__(self, api_key: Optional[str] = None):
        super().__init__(api_key)
        self.analyzer = CodeAnalyzer()
        self.java_analyzer = JavaAnalyzer()
        self.cobol_analyzer = CobolAnalyzer()

    def parse_code(self, code: str, schema: Dict[str, Any]) -> Dict[str, Any]:
        """Parse code into JSON according to schema."""
        language = self.analyzer.detect_language(code)
        
        if language == CodeLanguage.PYTHON:
            elements = self.analyzer.analyze_python_code(code)
        elif language == CodeLanguage.JAVA:
            elements = self.java_analyzer.analyze_code(code)
        elif language == CodeLanguage.COBOL:
            elements = self.cobol_analyzer.analyze_code(code)
        else:
            raise ValueError(f"Unsupported language: {language}")

        # Convert code elements to JSON based on schema
        result = self._elements_to_json(elements, schema, language)
        return result

    def _elements_to_json(self, elements: List[CodeElement], schema: Dict[str, Any], 
                         language: CodeLanguage) -> Dict[str, Any]:
        """Convert code elements to JSON according to schema."""
        result = {
            "type": "code",
            "language": language.value,
            "elements": []
        }

        for element in elements:
            json_element = {
                "name": element.name,
                "type": element.element_type,
                "location": {
                    "start_line": element.start_line,
                    "end_line": element.end_line
                }
            }

            if element.docstring:
                json_element["documentation"] = element.docstring

            if element.decorators:
                json_element["decorators"] = element.decorators

            if element.params:
                json_element["parameters"] = element.params

            if element.return_type:
                json_element["return_type"] = element.return_type

            if element.parent:
                json_element["parent"] = element.parent

            result["elements"].append(json_element)

        return result

    def parse(self, text: str, schema: Dict[str, Any]) -> Dict[str, Any]:
        """Override parent parse method to handle code parsing."""
        if schema.get("type") == "code":
            return self.parse_code(text, schema)
        return super().parse(text, schema)
