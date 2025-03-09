from typing import Dict, Any, List, Set, Tuple
import json
from dataclasses import dataclass
from collections import defaultdict

@dataclass
class SchemaPattern:
    """Represents a recognized pattern in schema structures."""
    pattern_type: str
    properties: Set[str]
    constraints: Dict[str, Any]

class SchemaOptimizer:
    """Analyzes and optimizes JSON schemas for better parsing performance."""

    COMMON_PATTERNS = {
        "person": {"name", "age", "email", "address"},
        "timestamp": {"date", "time", "timezone"},
        "geo_location": {"latitude", "longitude", "altitude"},
        "version_info": {"major", "minor", "patch", "build"},
        "contact": {"email", "phone", "address"},
    }

    def __init__(self):
        self.recognized_patterns: Dict[str, List[SchemaPattern]] = defaultdict(list)
        self.optimization_hints: Dict[str, List[str]] = defaultdict(list)

    def analyze_schema(self, schema: Dict[str, Any], path: str = "") -> Dict[str, Any]:
        """Analyze schema structure and suggest optimizations."""
        if not isinstance(schema, dict):
            return schema

        # Detect common patterns
        if schema.get("type") == "object" and "properties" in schema:
            properties = set(schema["properties"].keys())
            for pattern_name, pattern_props in self.COMMON_PATTERNS.items():
                if pattern_props.issubset(properties):
                    self.recognized_patterns[path].append(
                        SchemaPattern(pattern_name, properties, schema.get("required", []))
                    )

        # Analyze and optimize nested structures
        if "properties" in schema:
            for prop_name, prop_schema in schema["properties"].items():
                new_path = f"{path}.{prop_name}" if path else prop_name
                self.analyze_schema(prop_schema, new_path)
                self._generate_optimization_hints(prop_schema, new_path)

        return schema

    def _generate_optimization_hints(self, schema: Dict[str, Any], path: str) -> None:
        """Generate optimization hints for schema components."""
        if not isinstance(schema, dict):
            return

        # Check for array optimizations
        if schema.get("type") == "array":
            if "maxItems" not in schema:
                self.optimization_hints[path].append(
                    "Consider adding 'maxItems' constraint for better parsing control"
                )
            if "uniqueItems" not in schema:
                self.optimization_hints[path].append(
                    "Consider if items should be unique using 'uniqueItems'"
                )

        # Check for string optimizations
        if schema.get("type") == "string":
            if "pattern" not in schema and "format" not in schema:
                self.optimization_hints[path].append(
                    "Consider adding 'pattern' or 'format' constraint for validation"
                )

        # Check for number optimizations
        if schema.get("type") in ("number", "integer"):
            if "minimum" not in schema and "maximum" not in schema:
                self.optimization_hints[path].append(
                    "Consider adding range constraints (minimum/maximum)"
                )

    def get_parsing_strategy(self, schema: Dict[str, Any]) -> List[Tuple[str, str]]:
        """Generate optimal parsing strategy based on schema analysis."""
        strategies = []
        
        # Add strategies based on recognized patterns
        for path, patterns in self.recognized_patterns.items():
            for pattern in patterns:
                if pattern.pattern_type == "person":
                    strategies.append((path, "Use name parsing techniques for person fields"))
                elif pattern.pattern_type == "timestamp":
                    strategies.append((path, "Apply datetime parsing with timezone handling"))
                elif pattern.pattern_type == "geo_location":
                    strategies.append((path, "Use coordinate parsing with validation"))
                elif pattern.pattern_type == "version_info":
                    strategies.append((path, "Apply semantic versioning parsing"))
                elif pattern.pattern_type == "contact":
                    strategies.append((path, "Use contact information extraction techniques"))

        return strategies

    def optimize_schema(self, schema: Dict[str, Any]) -> Dict[str, Any]:
        """Optimize schema for better parsing performance."""
        optimized = schema.copy()
        
        def _optimize_node(node: Dict[str, Any]) -> Dict[str, Any]:
            if not isinstance(node, dict):
                return node

            # Optimize string fields
            if node.get("type") == "string" and "format" not in node:
                if any(pattern in str(node.get("description", "")).lower() 
                      for pattern in ["email", "mail"]):
                    node["format"] = "email"
                elif any(pattern in str(node.get("description", "")).lower() 
                        for pattern in ["date", "time"]):
                    node["format"] = "date-time"

            # Optimize number fields
            if node.get("type") in ("number", "integer"):
                if "minimum" not in node and "maximum" not in node:
                    # Add reasonable defaults based on type
                    if node["type"] == "integer":
                        node["minimum"] = 0
                    else:
                        node["minimum"] = 0.0

            # Optimize arrays
            if node.get("type") == "array" and "maxItems" not in node:
                node["maxItems"] = 1000  # Reasonable default limit

            # Recursively optimize properties
            if "properties" in node:
                node["properties"] = {
                    k: _optimize_node(v) for k, v in node["properties"].items()
                }

            return node

        return _optimize_node(optimized)
