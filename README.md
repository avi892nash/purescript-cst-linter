# PureScript Linter

A configurable linting tool for PureScript codebases that helps maintain code quality and consistency.

## Installation

```bash
npm install --save-dev purescript-linter
# or
spago install purescript-linter
```

## Configuration

Create a `.pslintrc.json` file in your project root:

```json
{
  "files": ["src/**/*.purs"],
  "ignore": [],
  "rules": {
    "mandatory-signature": "error",
    "no-array-jsx": null,
    "no-class-constraint": null,
    "dom-syntax-safety": { 
      "exprApp": []
    }
  }
}
```

## Available Rules

### mandatory-signature

Enforces that all top-level declarations have type signatures.

- **Options**: 
  - `"error"`: Treats violations as errors
  - `"warn"`: Treats violations as warnings
  - `null`: Disables the rule

### no-array-jsx

Prevents accidental use of array syntax in JSX contexts.

- **Options**: 
  - `"error"`: Treats violations as errors
  - `"warn"`: Treats violations as warnings
  - `null`: Disables the rule

### no-class-constraint

Discourages the use of class constraints in type signatures when not necessary.

- **Options**: 
  - `"error"`: Treats violations as errors
  - `"warn"`: Treats violations as warnings
  - `null`: Disables the rule

### dom-syntax-safety

Enforces safe DOM manipulation patterns.

- **Options**: 
  - `"error"`: Treats violations as errors
  - `"warn"`: Treats violations as warnings
  - A configuration object with:
    - `exprApp`: Array of expression application configurations:
      ```
      {
        "name": "functionName",
        "syntax": ["allowed", "syntax", "patterns"],
        "strict": true
      }
      ```
  - `null`: Disables the rule

## Usage

```bash
# Run the linter on your codebase
pslint

# Check specific files
pslint src/Main.purs src/Types.purs

# Show help
pslint --help
```

## Configuration Schema

The linter configuration uses the following PureScript types:

```purescript
type PSLintConfig = 
  { files :: Array String
  , ignore :: Array String
  , rules :: 
    { "mandatory-signature" :: Maybe (RulesConfig {})
    , "no-array-jsx" :: Maybe (RulesConfig { exprApp :: Maybe (Array String)})
    , "no-class-constraint" :: Maybe (RulesConfig {})
    , "dom-syntax-safety" :: Maybe (RulesConfig DomSyntaxConfig)
    } 
  }

data RulesConfig a = Error | Warn | Config a

type DomSyntaxConfig = 
  { exprApp :: Array 
    { name :: String
    , syntax :: Array Syntax
    , strict :: Maybe Boolean 
    } 
  }
```

## Extending the Linter

You can extend the linter with custom rules by implementing the `LinterRule` type class.

See the documentation for more details on creating custom rules for your specific codebase needs.

## Contributing

Contributions are welcome! Please see our [contribution guidelines](CONTRIBUTING.md) for more information.

## License

This project is licensed under the MIT License - see the LICENSE file for details.