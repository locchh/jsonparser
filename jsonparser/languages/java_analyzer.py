from typing import List, Dict, Any, Optional
import re
from dataclasses import dataclass
from ..code_parser import CodeElement, CodeLanguage

@dataclass
class JavaModifier:
    """Represents Java modifiers (public, private, static, etc.)."""
    visibility: str
    is_static: bool
    is_final: bool
    is_abstract: bool

class JavaAnalyzer:
    """Analyzes Java code structure."""

    # Regex patterns for Java code elements
    PATTERNS = {
        'package': r'package\s+([\w.]+);',
        'import': r'import\s+(static\s+)?([\w.]+\*?);',
        'class': r'(?P<modifiers>(?:public|private|protected|static|final|abstract)\s+)*class\s+(?P<name>\w+)(?:\s+extends\s+(?P<extends>\w+))?(?:\s+implements\s+(?P<implements>[\w,\s]+))?',
        'method': r'(?P<modifiers>(?:public|private|protected|static|final|abstract)\s+)*(?P<return>[\w<>[\],\s]+)\s+(?P<name>\w+)\s*\((?P<params>.*?)\)',
        'field': r'(?P<modifiers>(?:public|private|protected|static|final)\s+)*(?P<type>[\w<>[\],\s]+)\s+(?P<name>\w+)(?:\s*=\s*(?P<value>[^;]+))?;',
        'annotation': r'@(\w+)(?:\((?:[^)]+)\))?'
    }

    def analyze_code(self, code: str) -> List[CodeElement]:
        """Analyze Java code and extract its elements."""
        elements = []
        lines = code.split('\n')
        
        # Extract package
        package_match = re.search(self.PATTERNS['package'], code)
        if package_match:
            elements.append(CodeElement(
                name=package_match.group(1),
                element_type='package',
                content=package_match.group(0),
                start_line=self._find_line_number(lines, package_match.group(0)),
                end_line=self._find_line_number(lines, package_match.group(0)),
                docstring=None
            ))

        # Extract imports
        for match in re.finditer(self.PATTERNS['import'], code):
            elements.append(CodeElement(
                name=match.group(2),
                element_type='import',
                content=match.group(0),
                start_line=self._find_line_number(lines, match.group(0)),
                end_line=self._find_line_number(lines, match.group(0)),
                docstring=None
            ))

        # Extract classes
        for match in re.finditer(self.PATTERNS['class'], code):
            class_info = match.groupdict()
            javadoc = self._extract_javadoc(lines, self._find_line_number(lines, match.group(0)))
            
            elements.append(CodeElement(
                name=class_info['name'],
                element_type='class',
                content=match.group(0),
                start_line=self._find_line_number(lines, match.group(0)),
                end_line=self._find_class_end(lines, self._find_line_number(lines, match.group(0))),
                docstring=javadoc,
                decorators=self._extract_annotations(lines, self._find_line_number(lines, match.group(0))),
                params=[{'name': 'extends', 'type': class_info['extends']} if class_info['extends'] else None,
                       {'name': 'implements', 'type': class_info['implements']} if class_info['implements'] else None]
            ))

        # Extract methods
        for match in re.finditer(self.PATTERNS['method'], code):
            method_info = match.groupdict()
            javadoc = self._extract_javadoc(lines, self._find_line_number(lines, match.group(0)))
            
            elements.append(CodeElement(
                name=method_info['name'],
                element_type='method',
                content=match.group(0),
                start_line=self._find_line_number(lines, match.group(0)),
                end_line=self._find_method_end(lines, self._find_line_number(lines, match.group(0))),
                docstring=javadoc,
                decorators=self._extract_annotations(lines, self._find_line_number(lines, match.group(0))),
                params=self._parse_method_parameters(method_info['params']),
                return_type=method_info['return'].strip()
            ))

        return elements

    def _find_line_number(self, lines: List[str], content: str) -> int:
        """Find the line number for a given content."""
        for i, line in enumerate(lines, 1):
            if content.strip() in line:
                return i
        return 1

    def _extract_javadoc(self, lines: List[str], line_number: int) -> Optional[str]:
        """Extract Javadoc comment above the given line."""
        javadoc = []
        current_line = line_number - 1
        
        while current_line > 0:
            line = lines[current_line - 1].strip()
            if line.endswith('*/'):
                while current_line > 0:
                    line = lines[current_line - 1].strip()
                    if line.startswith('/**'):
                        break
                    javadoc.insert(0, line.lstrip('* '))
                    current_line -= 1
                break
            current_line -= 1
            
        return '\n'.join(javadoc) if javadoc else None

    def _extract_annotations(self, lines: List[str], line_number: int) -> List[str]:
        """Extract annotations above the given line."""
        annotations = []
        current_line = line_number - 1
        
        while current_line > 0:
            line = lines[current_line - 1].strip()
            if not line.startswith('@'):
                break
            annotation_match = re.match(self.PATTERNS['annotation'], line)
            if annotation_match:
                annotations.insert(0, annotation_match.group(1))
            current_line -= 1
            
        return annotations

    def _parse_method_parameters(self, params_str: str) -> List[Dict[str, str]]:
        """Parse Java method parameters."""
        if not params_str.strip():
            return []
            
        params = []
        for param in params_str.split(','):
            param = param.strip()
            if param:
                param_parts = param.split()
                params.append({
                    'name': param_parts[-1],
                    'type': ' '.join(param_parts[:-1])
                })
        return params

    def _find_method_end(self, lines: List[str], start_line: int) -> int:
        """Find the end line of a method."""
        brace_count = 0
        current_line = start_line - 1
        
        while current_line < len(lines):
            line = lines[current_line]
            brace_count += line.count('{') - line.count('}')
            if brace_count == 0 and '{' in line:
                return current_line + 1
            current_line += 1
        return start_line

    def _find_class_end(self, lines: List[str], start_line: int) -> int:
        """Find the end line of a class."""
        brace_count = 0
        current_line = start_line - 1
        
        while current_line < len(lines):
            line = lines[current_line]
            brace_count += line.count('{') - line.count('}')
            if brace_count == 0 and '}' in line:
                return current_line + 1
            current_line += 1
        return start_line
