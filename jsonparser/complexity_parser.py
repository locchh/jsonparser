from typing import Any, Dict, List, Optional, Union, Tuple
import json
from jsonschema import validate
import openai
from dataclasses import dataclass
from .parser import JSONParser
from .schema_optimizer import SchemaOptimizer, SchemaPattern

@dataclass
class SchemaNode:
    """Represents a node in the schema complexity tree."""
    schema: Dict[str, Any]
    path: List[str]
    complexity_score: float
    dependencies: List[str] = None
    parsing_strategy: Optional[str] = None

class ComplexityParser(JSONParser):
    """Enhanced JSON parser using complexity-descent methodology with pattern recognition."""

    def __init__(self, api_key: Optional[str] = None, complexity_threshold: float = 0.6):
        """Initialize the complexity-based parser.
        
        Args:
            api_key: Optional OpenAI API key
            complexity_threshold: Threshold to determine when to split parsing (0.0 to 1.0)
        """
        super().__init__(api_key)
        self.complexity_threshold = complexity_threshold
        self.schema_optimizer = SchemaOptimizer()
        self.parsing_cache = {}

    def _optimize_schema(self, schema: Dict[str, Any]) -> Tuple[Dict[str, Any], List[Tuple[str, str]]]:
        """Optimize schema and get parsing strategies."""
        # Analyze and optimize the schema
        self.schema_optimizer.analyze_schema(schema)
        optimized_schema = self.schema_optimizer.optimize_schema(schema)
        strategies = self.schema_optimizer.get_parsing_strategy(schema)
        return optimized_schema, strategies

    def _calculate_complexity(self, schema: Dict[str, Any], depth: int = 0) -> float:
        """Calculate the complexity score of a schema node.
        
        The complexity score is based on:
        - Number of properties
        - Depth of nesting
        - Type of properties (objects and arrays increase complexity)
        - Presence of constraints (required fields, patterns, etc.)
        """
        base_score = 0.0
        
        if not isinstance(schema, dict):
            return 0.0

        # Add complexity for each property
        properties = schema.get("properties", {})
        base_score += len(properties) * 0.1

        # Add complexity for nested structures
        base_score += depth * 0.15

        # Add complexity for arrays
        if schema.get("type") == "array":
            base_score += 0.3
            if "items" in schema:
                base_score += self._calculate_complexity(schema["items"], depth + 1)

        # Add complexity for required fields
        required = schema.get("required", [])
        base_score += len(required) * 0.05

        # Add complexity for additional constraints
        for constraint in ["pattern", "enum", "minimum", "maximum", "minLength", "maxLength"]:
            if constraint in schema:
                base_score += 0.1

        # Recursive calculation for nested objects
        for prop_schema in properties.values():
            if isinstance(prop_schema, dict):
                base_score += self._calculate_complexity(prop_schema, depth + 1)

        return min(base_score, 1.0)

    def _decompose_schema(self, schema: Dict[str, Any], path: List[str] = None) -> List[SchemaNode]:
        """Decompose a complex schema into simpler sub-schemas."""
        if path is None:
            path = []

        nodes: List[SchemaNode] = []
        complexity = self._calculate_complexity(schema)

        # If schema is simple enough, keep it as is
        if complexity < self.complexity_threshold:
            return [SchemaNode(schema=schema, path=path.copy(), complexity_score=complexity)]

        # Decompose complex objects
        if schema.get("type") == "object" and "properties" in schema:
            for prop_name, prop_schema in schema["properties"].items():
                new_path = path + [prop_name]
                prop_complexity = self._calculate_complexity(prop_schema)
                
                if prop_complexity >= self.complexity_threshold:
                    nodes.extend(self._decompose_schema(prop_schema, new_path))
                else:
                    nodes.append(SchemaNode(
                        schema=prop_schema,
                        path=new_path,
                        complexity_score=prop_complexity
                    ))

        # Decompose arrays with complex items
        elif schema.get("type") == "array" and "items" in schema:
            items_schema = schema["items"]
            items_complexity = self._calculate_complexity(items_schema)
            
            if items_complexity >= self.complexity_threshold:
                nodes.extend(self._decompose_schema(items_schema, path + ["items"]))
            else:
                nodes.append(SchemaNode(
                    schema=items_schema,
                    path=path + ["items"],
                    complexity_score=items_complexity
                ))

        return nodes

    def _create_subschema_prompt(self, text: str, node: SchemaNode, context: Dict[str, Any]) -> str:
        """Create a prompt for parsing a specific part of the schema."""
        path_str = " -> ".join(node.path) if node.path else "root"
        context_str = json.dumps(context, indent=2) if context else "No context available"
        
        # Add parsing strategy to prompt if available
        strategy_hint = f"\nParsing Strategy: {node.parsing_strategy}" if node.parsing_strategy else ""
        
        return f"""Parse the following text to extract information for the specific JSON path: {path_str}
        
Text to parse: {text}

Target Schema for this component:
{json.dumps(node.schema, indent=2)}

Current Context:
{context_str}{strategy_hint}

Generate only the JSON that matches this schema component. Do not include any other text or explanation."""

    def _merge_results(self, nodes: List[SchemaNode], results: Dict[str, Any]) -> Dict[str, Any]:
        """Merge parsed results back into a complete JSON structure."""
        final_result = {}
        
        for node in nodes:
            current = final_result
            # Navigate to the correct position in the result structure
            for i, path_part in enumerate(node.path[:-1]):
                if path_part not in current:
                    current[path_part] = {} if i < len(node.path) - 2 else None
                current = current[path_part]
            
            if node.path:  # If we have a path, set the value
                current[node.path[-1]] = results.get(tuple(node.path))
        
        return final_result

    def parse(self, text: str, schema: Dict[str, Any]) -> Dict[str, Any]:
        """Parse text into JSON using the complexity-descent methodology with pattern recognition.
        
        Args:
            text: The text to parse into JSON
            schema: JSON schema that defines the structure of the output
            
        Returns:
            A dictionary containing the parsed JSON that matches the schema
        """
        # Optimize schema and get parsing strategies
        optimized_schema, strategies = self._optimize_schema(schema)
        
        # Create strategy lookup
        strategy_lookup = dict(strategies)
        
        # Decompose the schema into manageable parts
        nodes = self._decompose_schema(optimized_schema)
        results = {}
        context = {}

        # Sort nodes by complexity score (ascending) and path length
        nodes.sort(key=lambda x: (x.complexity_score, len(x.path)))

        # Assign parsing strategies to nodes
        for node in nodes:
            path_str = ".".join(node.path) if node.path else ""
            node.parsing_strategy = strategy_lookup.get(path_str)

        # Process each node in order
        for node in nodes:
            # Check cache first
            cache_key = (text, json.dumps(node.schema), tuple(node.path))
            if cache_key in self.parsing_cache:
                results[tuple(node.path)] = self.parsing_cache[cache_key]
                continue

            prompt = self._create_subschema_prompt(text, node, context)
            
            response = openai.chat.completions.create(
                model="gpt-4",
                messages=[
                    {
                        "role": "system",
                        "content": "You are a precise JSON parser focusing on a specific component of a larger schema."
                        + (" Use the provided parsing strategy when available." if node.parsing_strategy else "")
                    },
                    {"role": "user", "content": prompt}
                ],
                temperature=0.1,
            )
            
            try:
                partial_result = json.loads(response.choices[0].message.content)
                validate(instance=partial_result, schema=node.schema)
                
                # Cache the result
                self.parsing_cache[cache_key] = partial_result
                results[tuple(node.path)] = partial_result
                
                # Update context for subsequent parses
                context[tuple(node.path)] = partial_result
            except (json.JSONDecodeError, Exception) as e:
                raise ValueError(f"Failed to parse component at path {' -> '.join(node.path)}: {str(e)}")

        # Merge all results into final structure
        final_result = self._merge_results(nodes, results)
        
        # Validate the complete result against the original schema
        validate(instance=final_result, schema=schema)
        return final_result
