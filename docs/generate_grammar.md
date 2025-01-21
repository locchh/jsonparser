# Generating Code from ANTLR Grammar and Grammarinator

### Using ANTLR to Generate Lexer, Parser, Listener, and Visitor

ANTLR can generate essential components like lexer, parser, listener, and visitor classes from a `.g4` grammar file. Use the following command:  

```bash
antlr4 -Dlanguage=Python3 -visitor path/to/file.g4
```  

- `-Dlanguage=Python3`: Specifies Python as the target language.  
- `-visitor`: Enables the generation of visitor classes.  
- `path/to/file.g4`: Path to your ANTLR grammar file.  

---

### Using Grammarinator to Generate Code

Grammarinator is a powerful tool for generating test cases or code samples based on your grammar. Follow these steps:  

#### 1. Preprocess the Grammar

Prepare the generator by processing the grammar file:  

```bash
grammarinator-process path/to/file.g4 -o path/to/file/python/generator
```  

- `path/to/file.g4`: Path to the grammar file.  
- `-o`: Specifies the output directory for the generator files.  

---

#### 2. Generate Code or Test Cases

After preprocessing, generate the code or test cases using the following command:  

```bash
grammarinator-generate <GeneratorPythonFile>.<GeneratorClass> \
    -r <NameOfRule> \
    -d <DepthNumber> \
    -o output/test/test_%d.txt \
    -n <NumberOfFile> \
    --sys-path path/to/file/python/generator
```  

- `<GeneratorPythonFile>`: Name of the Python generator file (exclude `.py`).  
- `<GeneratorClass>`: Name of the generator class in the Python file.  
- `-r <NameOfRule>`: Specifies the starting grammar rule.  
- `-d <DepthNumber>`: Determines the depth of generated code (controls complexity).  
- `-o output/test/test_%d.txt`: Output file path and naming pattern (e.g., `test_1.txt`, `test_2.txt`).  
- `-n <NumberOfFile>`: Number of output files to generate.  
- `--sys-path path/to/file/python/generator`: System path to the generator files.  

---

## References

[ANTLR Grammars Repository](https://github.com/antlr/grammars-v4)  

[Grammarinator GitHub Repository](https://github.com/renatahodovan/grammarinator)  
