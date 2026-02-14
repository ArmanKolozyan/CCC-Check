//! The Circom front-end

// Global debug flag to control AST printing
// static mut PRINT_FULL_AST: bool = false;

pub mod parser;
pub mod term;
pub mod cvisit;
use crate::front::FrontEnd;
use std::path::PathBuf;
use crate::ir::term::{Computations, Term};
use crate::front::circom::cvisit::cstmtwalker::{CircomStatementWalker, CompileTimeValue};
use crate::circify::Circify;
use fxhash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::cell::RefCell;

use term::*;

use circom_pest_ast as ast;

use std::collections::HashMap as StdHashMap;

thread_local! {
    static LAST_SIGNAL_TAGS: RefCell<StdHashMap<String, Vec<(String, Option<rug::Integer>)>>> = RefCell::new(StdHashMap::new());
}

/// Get the signal tags from the last `CircomFE::gen()` call.
pub fn get_last_signal_tags() -> StdHashMap<String, Vec<(String, Option<rug::Integer>)>> {
    LAST_SIGNAL_TAGS.with(|tags| tags.borrow().clone())
}

/// Input to the Circom frontend
pub struct Inputs {
    /// The Circom source file
    pub file: PathBuf,
}

/// Circom frontend
pub struct CircomFE;

impl FrontEnd for CircomFE {
    type Inputs = Inputs;

    fn gen(i: Inputs) -> Computations {
        // Load and parse the Circom file
        let loader = parser::CircomLoad::new();
        let asts = loader.load(&i.file);

        let print_ast = std::env::var("CIRC_PRINT_CIRCOM_AST").is_ok();
        if print_ast {
            // Print the ASTs
            println!("Parsed Circom ASTs:");
            for (path, ast) in &asts {
                println!("File: {}", path.display());
                println!("  Pragma: {:?}", ast.pragma);
                println!("  Declarations: {} items", ast.declarations.len());

                // Print summary of each declaration
                for (i, decl) in ast.declarations.iter().enumerate() {
                    match decl {
                        circom_pest_ast::SymbolDeclaration::Include(inc) => {
                            println!("    {}: Include \"{}\"", i, inc.path.value);
                        }
                        circom_pest_ast::SymbolDeclaration::Template(template) => {
                            println!(
                                "    {}: Template \"{}\" with {} params and {} statements",
                                i,
                                template.id.value,
                                template.params.len(),
                                template.statements.len()
                            );
                        }
                        circom_pest_ast::SymbolDeclaration::Function(func) => {
                            println!(
                                "    {}: Function \"{}\" with {} params and {} statements",
                                i,
                                func.id.value,
                                func.params.len(),
                                func.statements.len()
                            );
                        }
                        circom_pest_ast::SymbolDeclaration::MainComponent(main) => {
                            println!(
                                "    {}: Main component with {} public signals",
                                i,
                                main.public_signals.len()
                            );
                        }
                    }
                }

                // Print full AST if the global flag is set
                //  unsafe {
                //     println!("\nFull AST for {}:", path.display());
                //     println!("{:#?}", ast);
                //  }

                println!(); // Empty line between files
            }
        }
        
        // Create a code generator
        let mut circom_gen = CircomGen::new(asts);
        
        // Process ASTs and generate IR
        circom_gen.visit_files();
        
        // Find and process the main component
        circom_gen.process_main_component();

        // Store signal tags in thread-local for later retrieval
        let tags: StdHashMap<String, Vec<(String, Option<rug::Integer>)>> = circom_gen.signal_tags.borrow()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        LAST_SIGNAL_TAGS.with(|t| *t.borrow_mut() = tags);

        // Return the computations
        circom_gen.into_computations()
    }
}

/// The Circom code generator
pub struct CircomGen<'ast> {
    /// The ASTs for each loaded file
    asts: HashMap<PathBuf, ast::File<'ast>>,
    /// The circify context
    circ: RefCell<Circify<Circom>>,
    /// Map of template definitions by path and name
    templates: HashMap<PathBuf, HashMap<String, ast::TemplateDefinition<'ast>>>,
    /// Map of function definitions by path and name
    functions: HashMap<PathBuf, HashMap<String, ast::FunctionDefinition<'ast>>>,
    /// Current file path stack
    file_stack: RefCell<Vec<PathBuf>>,
    /// Import map for resolving includes
    import_map: HashMap<PathBuf, HashMap<String, PathBuf>>,
    /// Constraints collected during processing
    constraints: RefCell<Vec<Term>>,
    /// Output signal names collected during processing
    output_signals: RefCell<Vec<String>>,
    /// Two-level stack for template/function variables
    template_vars: RefCell<Vec<HashMap<String, T>>>,
    /// Signal tags collected during processing: IR variable name -> list of (tag_name, optional_value)
    signal_tags: RefCell<HashMap<String, Vec<(String, Option<rug::Integer>)>>>,
}

impl<'ast> CircomGen<'ast> {
    /// Create a new Circom code generator
    pub fn new(asts: HashMap<PathBuf, ast::File<'ast>>) -> Self {
        CircomGen {
            asts,
            circ: RefCell::new(Circify::new(Circom::new())),
            templates: HashMap::default(),
            functions: HashMap::default(),
            file_stack: RefCell::new(Vec::new()),
            import_map: HashMap::default(),
            constraints: RefCell::new(Vec::new()),
            output_signals: RefCell::new(Vec::new()),
            template_vars: RefCell::new(Vec::new()),
            signal_tags: RefCell::new(HashMap::default()),
        }
    }
    
    /// Enter a template or function scope
    fn enter_template(&self) {
        self.template_vars.borrow_mut().push(HashMap::default());
    }
    
    /// Exit a template or function scope
    fn exit_template(&self) {
        if !self.template_vars.borrow().is_empty() {
            self.template_vars.borrow_mut().pop();
        }
    }
    
    /// Visit all files and build maps of templates and functions
    fn visit_files(&mut self) {
        // First pass: collect includes and build import map
        let files = self.process_includes();
        
        // Second pass: collect template and function definitions
        for path in &files {
            self.file_stack.borrow_mut().push(path.clone());
            if let Some(ast) = self.asts.get(path) {
                // Process declarations
                for decl in &ast.declarations {
                    match decl {
                        circom_pest_ast::SymbolDeclaration::Template(template) => {
                            self.templates
                                .entry(path.clone())
                                .or_insert_with(HashMap::default)
                                .insert(template.id.value.clone(), template.clone());
                        },
                        circom_pest_ast::SymbolDeclaration::Function(function) => {
                            self.functions
                                .entry(path.clone())
                                .or_insert_with(HashMap::default)
                                .insert(function.id.value.clone(), function.clone());
                        },
                        _ => {}
                    }
                }
            }
            self.file_stack.borrow_mut().pop();
        }
    }
    
    /// Process includes and build import map
    fn process_includes(&mut self) -> Vec<PathBuf> {
        // Build dependency graph and return toposorted files
        // Similar to ZGen::visit_imports
        let mut files = Vec::new();
        for (path, ast) in &self.asts {
            self.file_stack.borrow_mut().push(path.clone());
            
            let mut includes = HashMap::default();
            for decl in &ast.declarations {
                if let circom_pest_ast::SymbolDeclaration::Include(inc) = decl {
                    let include_path = self.resolve_include_path(&inc.path.value);
                    includes.insert(inc.path.value.clone(), include_path);
                }
            }
            
            self.import_map.insert(path.clone(), includes);
            files.push(path.clone());
            
            self.file_stack.borrow_mut().pop();
        }
        
        files
    }
    
    /// Resolve an include path relative to the current file
    fn resolve_include_path(&self, include_path: &str) -> PathBuf {
        // Remove quotes from path
        let path_str = include_path.trim_matches('"');

        // Get current directory
        let current_file = self.file_stack.borrow().last()
            .unwrap_or_else(|| {
                panic!(
                    "File stack is empty when resolving include path\n\
                     \n\
                     Include path: '{}'\n\
                     \n\
                     This is an internal compiler error - the file stack should always\n\
                     contain at least the current file being processed.",
                    include_path
                )
            })
            .clone();
        let parent_dir = current_file.parent()
            .unwrap_or_else(|| {
                panic!(
                    "Current file has no parent directory\n\
                     \n\
                     Current file: {:?}\n\
                     Include path: '{}'\n\
                     \n\
                     This may occur if the file is at the filesystem root or\n\
                     if the path is malformed.",
                    current_file, include_path
                )
            });

        // Join with parent directory
        parent_dir.join(path_str)
    }
    
    /// Evaluate a compile-time constant expression
    /// Used for extracting main component parameter values
    fn eval_const_expr(expr: &ast::Expression) -> Option<i64> {
        match expr {
            ast::Expression::Number(num) => {
                match num {
                    ast::Number::Decimal(dec) => {
                        dec.span.as_str().trim().parse::<i64>().ok()
                    }
                    ast::Number::Hex(hex) => {
                        let hex_str = hex.span.as_str().trim();
                        let hex_str = hex_str.strip_prefix("0x")
                            .or_else(|| hex_str.strip_prefix("0X"))
                            .unwrap_or(hex_str);
                        i64::from_str_radix(hex_str, 16).ok()
                    }
                }
            }
            ast::Expression::Binary(bin) => {
                let left = Self::eval_const_expr(&bin.left)?;
                let right = Self::eval_const_expr(&bin.right)?;
                match bin.op {
                    ast::OpBinary::AddOp => Some(left.wrapping_add(right)),
                    ast::OpBinary::SubOp => Some(left.wrapping_sub(right)),
                    ast::OpBinary::MulOp => Some(left.wrapping_mul(right)),
                    ast::OpBinary::DivOp => {
                        if right != 0 {
                            Some(left / right)
                        } else {
                            None
                        }
                    }
                    ast::OpBinary::IDivOp => {
                        if right != 0 {
                            Some(left / right)
                        } else {
                            None
                        }
                    }
                    ast::OpBinary::ModOp => {
                        if right != 0 {
                            Some(left % right)
                        } else {
                            None
                        }
                    }
                    ast::OpBinary::PowOp => {
                        if right >= 0 && right <= 32 {
                            Some(left.wrapping_pow(right as u32))
                        } else {
                            None
                        }
                    }
                    ast::OpBinary::LeftShiftOp => {
                        if right >= 0 && right < 64 {
                            Some(left << right)
                        } else {
                            None
                        }
                    }
                    ast::OpBinary::RightShiftOp => {
                        if right >= 0 && right < 64 {
                            Some(left >> right)
                        } else {
                            None
                        }
                    }
                    _ => None, // Comparison and logical ops not valid for template params
                }
            }
            ast::Expression::Unary(un) => {
                let inner = Self::eval_const_expr(&un.expression)?;
                match &un.op {
                    ast::OpUnary::Neg(_) => Some(-inner),
                    ast::OpUnary::Not(_) => Some(!inner),
                    _ => None,
                }
            }
            _ => None, // Other expressions (identifiers, calls, arrays, etc.) need context
        }
    }

    /// Process the main component
    fn process_main_component(&mut self) {
        for (path, ast) in &self.asts {
            self.file_stack.borrow_mut().push(path.clone());
            
            for decl in &ast.declarations {
                if let circom_pest_ast::SymbolDeclaration::MainComponent(main) = decl {
                    self.process_component(main);
                    break;
                }
            }
            
            self.file_stack.borrow_mut().pop();
        }
    }
    
    /// Process a component instance
    fn process_component(&self, main: &circom_pest_ast::MainComponent) {
        // Get template name
        let template_name = &main.component_instantiation.id.value;

        // Find template definition
        if let Some((template_path, template)) = self.find_template(template_name) {
            // Collect public signal names
            let public_signal_names: HashSet<String> = main.public_signals
                .iter()
                .map(|sig| sig.value.clone())
                .collect();

            // Process public signals - register them first
            for signal in &main.public_signals {
                // Register public input in computations
                self.register_public_input(&signal.value);
            }

            // Process template instantiation
            self.file_stack.borrow_mut().push(template_path.clone());

            // Create a statement walker with public signals
            let mut walker = CircomStatementWalker::new(self, public_signal_names);

            // Enter template scope
            self.enter_template();

            // Extract and set template parameter values from component instantiation
            // Handle both scalar and array parameters
            let param_values: Vec<CompileTimeValue> = main.component_instantiation.args.iter()
                .filter_map(|expr| {
                    match expr {
                        // Array literal
                        ast::Expression::Array(arr) => {
                            let elements: Option<Vec<i64>> = arr.elements.iter()
                                .map(|e| Self::eval_const_expr(e))
                                .collect();
                            elements.map(|vec| CompileTimeValue::Array1D(
                                vec.into_iter().map(rug::Integer::from).collect()
                            ))
                        }
                        // Scalar expression
                        _ => Self::eval_const_expr(expr).map(CompileTimeValue::scalar)
                    }
                })
                .collect();

            // Instantiate the main component using instantiate_component for consistent processing
            walker.instantiate_component("main", template_name, &param_values, template);

            // Collect constraints
            self.constraints.borrow_mut().extend(walker.get_constraints().to_vec());

            // Collect output signals
            self.output_signals.borrow_mut().extend(walker.get_output_signals().iter().cloned());

            // Collect signal tags
            self.signal_tags.borrow_mut().extend(
                walker.get_signal_tags().iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
            );

            // Exit template scope
            self.exit_template();
            
            self.file_stack.borrow_mut().pop();
        }
    }
    
    /// Find a template definition by name
    fn find_template(
        &self,
        name: &str,
    ) -> Option<(&PathBuf, &circom_pest_ast::TemplateDefinition<'_>)> {
        for (path, templates) in &self.templates {
            if let Some(template) = templates.get(name) {
                return Some((path, template));
            }
        }
        None
    }

    /// Find a function definition by name
    pub fn find_function(
        &self,
        name: &str,
    ) -> Option<(&PathBuf, &circom_pest_ast::FunctionDefinition<'_>)> {
        for (path, functions) in &self.functions {
            if let Some(function) = functions.get(name) {
                return Some((path, function));
            }
        }
        None
    }

    /// Register a public input signal
    fn register_public_input(&self, name: &str) {
        // Signal declarations in the template will also declare these,
        // so we ignore Rebind errors but panic on unexpected errors.
        if let Err(e) = self.circ
            .borrow_mut()
            .declare_input(
                name.to_string(),
                &Ty::Field,
                None,
                None,
                false
            )
        {
            let is_rebind = format!("{}", e).contains("already declared");
            if !is_rebind {
                panic!("failed to declare public input '{}': {}", name, e);
            }
        }
    }

    /// Assert a constraint to the computation
    pub fn assert_constraint(&self, constraint: Term) {
        self.circ.borrow_mut().assert(constraint);
    }
    
    /// Convert to computations
    fn into_computations(self) -> Computations {
        let mut comp = Computations::default();

        // Add the constraints
        let circ = self.circ.into_inner();
        let cs = circ.cir_ctx().cs.borrow().clone();

        // Create a single main computation
        comp.comps.insert("main".to_string(), cs);

        comp
    }
}
