# Core Components

This section provides detailed documentation for each core component of the LLM JSON Parser.

## Component Overview

1. [JSON Parser](json-parser.md)
2. [Complexity Parser](complexity-parser.md)
3. [Code Parser](code-parser.md)
4. [Schema Optimizer](schema-optimizer.md)

## Architecture Diagram

```
                           ┌─────────────────┐
                           │     Client      │
                           └────────┬────────┘
                                    │
                           ┌────────┴────────┐
                           │   JSONParser    │
                           └────────┬────────┘
                                    │
                    ┌───────────────┴───────────────┐
                    │                               │
           ┌────────┴────────┐            ┌─────────┴─────────┐
           │ ComplexityParser│            │    CodeParser     │
           └────────┬────────┘            └─────────┬─────────┘
                    │                               │
           ┌────────┴────────┐            ┌─────────┴─────────┐
           │ SchemaOptimizer │            │LanguageAnalyzers  │
           └─────────────────┘            └─────────┬─────────┘
                                                    │
                                          ┌─────────┴─────────┐
                                          │  Python │  Java   │
                                          │  COBOL  │  ...    │
                                          └───────────────────┘
```

## Component Interactions

### JSON Parser
- Base class for all parsing operations
- Handles communication with OpenAI API
- Provides basic schema validation

### Complexity Parser
- Extends JSON Parser
- Implements complexity-descent methodology
- Uses Schema Optimizer for better parsing

### Code Parser
- Specialized parser for programming languages
- Uses language-specific analyzers
- Converts code structures to JSON

### Schema Optimizer
- Analyzes and optimizes JSON schemas
- Provides parsing strategies
- Improves parsing accuracy

## Extension Points

The library is designed to be extensible in several ways:

1. **New Language Support**
   - Add new language analyzers in `languages/`
   - Implement language-specific parsing logic
   - Register with CodeParser

2. **Custom Optimizations**
   - Extend SchemaOptimizer
   - Add new optimization strategies
   - Implement custom pattern recognition

3. **Parser Extensions**
   - Inherit from base parsers
   - Override core methods
   - Add specialized functionality

For detailed documentation on each component, click the links in the Component Overview section.
