from typing import List, Dict, Any, Optional
import re
from dataclasses import dataclass
from ..code_parser import CodeElement, CodeLanguage

@dataclass
class CobolDivision:
    """Represents a COBOL division."""
    name: str
    sections: List[str]
    start_line: int
    end_line: int

class CobolAnalyzer:
    """Analyzes COBOL code structure."""

    # COBOL divisions and their common sections
    DIVISIONS = {
        'IDENTIFICATION': ['PROGRAM-ID', 'AUTHOR', 'DATE-WRITTEN', 'DATE-COMPILED'],
        'ENVIRONMENT': ['CONFIGURATION', 'INPUT-OUTPUT'],
        'DATA': ['WORKING-STORAGE', 'LINKAGE', 'FILE'],
        'PROCEDURE': []  # Procedure division can have custom section names
    }

    def analyze_code(self, code: str) -> List[CodeElement]:
        """Analyze COBOL code and extract its elements."""
        elements = []
        lines = code.split('\n')
        
        # Normalize COBOL code (handle different formats)
        normalized_lines = self._normalize_code(lines)
        
        # Extract program information
        program_id = self._extract_program_id(normalized_lines)
        if program_id:
            elements.append(CodeElement(
                name=program_id,
                element_type='program',
                content='\n'.join(normalized_lines),
                start_line=1,
                end_line=len(normalized_lines),
                docstring=self._extract_program_documentation(normalized_lines)
            ))

        # Process each division
        current_division = None
        current_section = None
        division_content = []
        
        for i, line in enumerate(normalized_lines, 1):
            # Check for division headers
            division_match = re.match(r'^\s*(\w+)\s+DIVISION\.', line)
            if division_match:
                if current_division:
                    # Add previous division to elements
                    elements.append(self._create_division_element(
                        current_division, division_content, current_section
                    ))
                current_division = division_match.group(1)
                division_content = [line]
                continue

            # Check for section headers
            section_match = re.match(r'^\s*(\w+(?:-\w+)*)\s+SECTION\.', line)
            if section_match:
                if current_section:
                    # Add previous section to elements
                    elements.append(self._create_section_element(
                        current_division, current_section, division_content
                    ))
                current_section = section_match.group(1)
                division_content = [line]
                continue

            # Check for paragraphs in PROCEDURE DIVISION
            if current_division == 'PROCEDURE':
                paragraph_match = re.match(r'^\s*(\w+(?:-\w+)*)\.\s*$', line)
                if paragraph_match:
                    elements.append(CodeElement(
                        name=paragraph_match.group(1),
                        element_type='paragraph',
                        content=line,
                        start_line=i,
                        end_line=i,
                        parent=current_section
                    ))

            if line.strip():
                division_content.append(line)

        # Add last division/section
        if current_division:
            elements.append(self._create_division_element(
                current_division, division_content, current_section
            ))

        return elements

    def _normalize_code(self, lines: List[str]) -> List[str]:
        """Normalize COBOL code format."""
        normalized = []
        for line in lines:
            # Remove line numbers if present (columns 1-6)
            if len(line) > 6:
                line = line[6:]
            
            # Remove comment indicators (column 7)
            if line and line[0] == '*':
                continue
                
            # Handle continuation lines
            if line and line[0] == '-':
                if normalized:
                    normalized[-1] = normalized[-1] + line[1:].strip()
                    continue
                    
            normalized.append(line.strip())
        
        return normalized

    def _extract_program_id(self, lines: List[str]) -> Optional[str]:
        """Extract PROGRAM-ID from IDENTIFICATION DIVISION."""
        for line in lines:
            match = re.search(r'PROGRAM-ID\.\s+(\w+)', line)
            if match:
                return match.group(1)
        return None

    def _extract_program_documentation(self, lines: List[str]) -> Optional[str]:
        """Extract program documentation from IDENTIFICATION DIVISION."""
        doc_lines = []
        in_id_division = False
        
        for line in lines:
            if 'IDENTIFICATION DIVISION' in line:
                in_id_division = True
                continue
            if in_id_division and 'DIVISION' in line:
                break
            if in_id_division and line.strip() and not line.strip().endswith('.'):
                doc_lines.append(line.strip())
                
        return '\n'.join(doc_lines) if doc_lines else None

    def _create_division_element(self, name: str, content: List[str], 
                               current_section: Optional[str]) -> CodeElement:
        """Create a CodeElement for a COBOL division."""
        return CodeElement(
            name=name,
            element_type='division',
            content='\n'.join(content),
            start_line=1,  # Will be calculated properly in actual implementation
            end_line=len(content),
            docstring=None,
            params=[{'name': 'section', 'value': current_section}] if current_section else None
        )

    def _create_section_element(self, division: str, name: str, 
                              content: List[str]) -> CodeElement:
        """Create a CodeElement for a COBOL section."""
        return CodeElement(
            name=name,
            element_type='section',
            content='\n'.join(content),
            start_line=1,  # Will be calculated properly in actual implementation
            end_line=len(content),
            docstring=None,
            parent=division
        )

    def analyze_data_items(self, code: str) -> List[Dict[str, Any]]:
        """Analyze COBOL data items in the DATA DIVISION."""
        data_items = []
        in_data_division = False
        level_stack = []

        for line in code.split('\n'):
            if 'DATA DIVISION' in line:
                in_data_division = True
                continue
            if in_data_division and 'DIVISION' in line:
                break
            if not in_data_division or not line.strip():
                continue

            # Parse data item
            match = re.match(r'^\s*(\d+)\s+(\w+(?:-\w+)*)\s+(.*)$', line)
            if match:
                level, name, description = match.groups()
                level = int(level)

                # Handle level hierarchy
                while level_stack and level_stack[-1]['level'] >= level:
                    level_stack.pop()

                item = {
                    'level': level,
                    'name': name,
                    'description': description.strip(),
                    'parent': level_stack[-1]['name'] if level_stack else None
                }

                # Add picture clause if present
                pic_match = re.search(r'PIC(?:TURE)?\s+IS\s+(\S+)', description)
                if pic_match:
                    item['picture'] = pic_match.group(1)

                data_items.append(item)
                level_stack.append(item)

        return data_items
