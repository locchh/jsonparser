import json
from typing import Any, Dict, Optional
import openai
from jsonschema import validate
from pydantic import BaseModel

class JSONParser:
    """A class that uses LLMs to parse text into JSON according to a schema."""
    
    def __init__(self, api_key: Optional[str] = None):
        """Initialize the JSON parser.
        
        Args:
            api_key: Optional OpenAI API key. If not provided, will look for OPENAI_API_KEY in environment.
        """
        if api_key:
            openai.api_key = api_key
            
    def _create_prompt(self, text: str, schema: Dict[str, Any]) -> str:
        """Create a prompt for the LLM that includes the text and schema."""
        return f"""Parse the following text into JSON that matches the provided JSON schema.
        
Text to parse: {text}

JSON Schema:
{json.dumps(schema, indent=2)}

Generate only valid JSON that matches this schema. Do not include any other text or explanation.
"""

    def parse(self, text: str, schema: Dict[str, Any]) -> Dict[str, Any]:
        """Parse text into JSON according to the provided schema.
        
        Args:
            text: The text to parse into JSON
            schema: JSON schema that defines the structure of the output
            
        Returns:
            A dictionary containing the parsed JSON that matches the schema
            
        Raises:
            ValueError: If the generated JSON doesn't match the schema
            openai.OpenAIError: If there's an error with the OpenAI API
        """
        prompt = self._create_prompt(text, schema)
        
        response = openai.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are a precise JSON parser that only outputs valid JSON."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.1,  # Low temperature for more consistent outputs
        )
        
        try:
            result = json.loads(response.choices[0].message.content)
            # Validate the result against the schema
            validate(instance=result, schema=schema)
            return result
        except json.JSONDecodeError:
            raise ValueError("Failed to generate valid JSON")
        except Exception as e:
            raise ValueError(f"Generated JSON does not match schema: {str(e)}")
