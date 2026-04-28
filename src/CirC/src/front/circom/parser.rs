//! Parsing and recursively loading Circom.

use fxhash::FxHashMap as HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use circom_pest_ast::{generate_ast, File};
use typed_arena::Arena;

/// Loads and parses Circom files
pub struct CircomLoad {
    sources: Arena<String>,
}

impl CircomLoad {
    /// Create a new Circom loader
    pub fn new() -> Self {
        Self {
            sources: Arena::new(),
        }
    }

    /// Load and parse a Circom file and its dependencies
    pub fn load<P: AsRef<Path>>(&self, p: &P) -> HashMap<PathBuf, File<'_>> {
        let mut result = HashMap::default();
        let mut to_process = vec![PathBuf::from(p.as_ref())];
        let mut processed = Vec::new();

        while let Some(path) = to_process.pop() {
            if processed.contains(&path) {
                continue;
            }

            // Parse the file
            match self.parse(&path) {
                Ok(ast) => {
                    // Find includes and add them to the processing queue
                    let includes = self.includes(&ast, &path);
                    for include in includes {
                        if !processed.contains(&include) {
                            to_process.push(include);
                        }
                    }

                    // Add the parsed file to the result
                    result.insert(path.clone(), ast);
                    processed.push(path);
                }
                Err(e) => {
                    panic!(
                        "Failed to parse Circom file {}: {}",
                        path.display(),
                        e
                    );
                }
            }
        }

        result
    }

    /// Parse a single Circom file
    fn parse<P: AsRef<Path>>(&self, p: &P) -> Result<File<'_>, String> {
        let path = p.as_ref();
        let content = match fs::read_to_string(path) {
            Ok(content) => content,
            Err(e) => return Err(format!("Error reading {}: {}", path.display(), e)),
        };

        let content = self.sources.alloc(content);

        match generate_ast(content) {
            Ok(ast) => Ok(ast),
            Err(e) => Err(format!("Parse error in {}: {}", path.display(), e)),
        }
    }

    /// Extract include directives from a parsed file and resolve their paths
    fn includes<P: AsRef<Path>>(&self, ast: &File, p: &P) -> Vec<PathBuf> {
        let mut result = Vec::new();
        let parent = p.as_ref();

        for decl in &ast.declarations {
            if let circom_pest_ast::SymbolDeclaration::Include(include) = decl {
                // Handle the include path: strip quotes and resolve relative to parent
                let path_str = include.path.value.trim_matches('"');
                let include_path = if let Some(parent_dir) = parent.parent() {
                    parent_dir.join(path_str)
                } else {
                    PathBuf::from(path_str)
                };
                
                result.push(include_path);
            }
        }

        result
    }
}
