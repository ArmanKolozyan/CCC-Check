mod trans;
mod utils;
mod datan_ast;

use crate::target::datan::datan_ast::*;

use std::collections::HashSet;

/// Represents if an expression is defined in the precompiles or in the outputs (constraints).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExprType {
    /// Expression defined in the precompiles.
    Precompile,
    /// Expression defined in the constraints.
    Constraint,
}

/// Represents the Datalog Analyzer (datan) structure.
pub struct Datan {
    public_inputs: Vec<PublicVariable>,
    private_inputs: Vec<PrivateVariable>,
    identifiers: Vec<Identifier>,
    exprs: Vec<Expr>,
    field_constants: HashSet<String>,
    boolean_constants: HashSet<String>,
    bv_constants: HashSet<String>,
    binary_exprs: Vec<BinaryExpr>,
    assigns: Vec<Assign>,
    asserts: Vec<Assert>,
    nots: Vec<Not>,
    transforms: Vec<Transform>,
    ites: Vec<Ite>,
    concatxs: Vec<ConcatX>,
    boolmjs: Vec<BoolMaj>,
    exprs_counter: usize,
    comps_counter: usize,
    assert_counter: usize,
}

impl Datan {
    /// Creates a new, empty Datan instance.
    pub fn new() -> Self {
        Datan {
            public_inputs: Vec::new(),
            private_inputs: Vec::new(),
            identifiers: Vec::new(),
            exprs: Vec::new(),
            field_constants: HashSet::new(),
            boolean_constants: HashSet::new(),
            bv_constants: HashSet::new(),
            binary_exprs: Vec::new(),
            assigns: Vec::new(),
            asserts: Vec::new(),
            nots: Vec::new(),
            transforms: Vec::new(),
            ites: Vec::new(),
            concatxs: Vec::new(),
            boolmjs: Vec::new(),
            exprs_counter: 0,
            comps_counter: 0,
            assert_counter: 0,
        }
    }

    /// Adds a public input to the Datan instance.
    pub fn add_public_input(&mut self, x: String) {
        self.public_inputs.push(PublicVariable::new(x.clone()));
        self.identifiers.push(Identifier::new(x));
    }

    /// Adds a private input to the Datan instance.
    pub fn add_private_input(&mut self, x: String) {
        self.private_inputs.push(PrivateVariable::new(x.clone()));
        self.identifiers.push(Identifier::new(x));
    }
    
   /// Adds a field constant to the Datan instance.
    pub fn add_field_constant(&mut self, value: String) {
        // if the value is not already a field constant, add it
        self.field_constants.insert(value);
    }

    /// Adds a bv constant to the Datan instance.
    pub fn add_bv_constant(&mut self, value: String) {
        self.bv_constants.insert(value);
    }

    /// Adds a boolean constant to the Datan instance.
    pub fn add_boolean_constant(&mut self, value: String) {
        self.boolean_constants.insert(value);
    }

    /// Adds a binary expression to the Datan instance.
    pub fn add_binary_expr(&mut self, expr_type: ExprType, lexpr: String, rexpr: String, op: String) -> String {
        let (id, expr_type) = match expr_type {
            ExprType::Precompile => {
                self.comps_counter += 1;
                (format!("comp{}", self.comps_counter), ExprType::Precompile)
            }
            ExprType::Constraint => {
                self.exprs_counter += 1;
                (format!("e{}", self.exprs_counter), ExprType::Constraint)
            }
        };
        self.binary_exprs.push(BinaryExpr::new(id.clone(), lexpr, rexpr, op, expr_type));
        self.exprs.push(Expr::new(id.clone()));
        id
    }

    /// Adds an assign to the Datan instance.
    pub fn add_assign(&mut self, x: String, y: String) {
        self.assigns.push(Assign::new(x, y));
    }

    /// Adds an assert to the Datan instance.
    pub fn add_assert(&mut self, expr: String) {
        let assert_id = format!("c{}", self.assert_counter);
        self.assert_counter += 1;
        self.add_assign(assert_id.clone(), expr);
        self.asserts.push(Assert::new(assert_id));
    }

    /// Adds a not to the Datan instance.
    pub fn add_not(&mut self, expr_type: ExprType, expr: String) -> String{
        let id = match expr_type {  
            ExprType::Precompile => {
                self.comps_counter += 1;
                format!("comp{}", self.comps_counter)
            }
            ExprType::Constraint => {
                self.exprs_counter += 1;
                format!("e{}", self.exprs_counter)
            }
        };
        self.nots.push(Not::new(id.clone(), expr, expr_type));
        id
    }

    /// Adds a transform to the Datan instance.
    pub fn add_transform(&mut self, expr_type: ExprType, expr: String, op: String) -> String {
        let id = match expr_type {  
            ExprType::Precompile => {
                self.comps_counter += 1;
                format!("comp{}", self.comps_counter)
            }
            ExprType::Constraint => {
                self.exprs_counter += 1;
                format!("e{}", self.exprs_counter)
            }
        };
        self.transforms.push(Transform::new(id.clone(), expr, op, expr_type));
        id
    }

    /// Adds an ite to the Datan instance.
    pub fn add_ite(&mut self, expr_type: ExprType, condition: String, then_expr: String, else_expr: String) -> String {
        let id = match expr_type {
            ExprType::Precompile => {
                self.comps_counter += 1;
                format!("comp{}", self.comps_counter)
            }
            ExprType::Constraint => {
                self.exprs_counter += 1;
                format!("e{}", self.exprs_counter)
            }
        };
        self.ites.push(Ite::new(id.clone(), condition, then_expr, else_expr, expr_type));
        id
    }

    /// Adds a concatx to the Datan instance.
    pub fn add_concatx(&mut self, expr_type: ExprType, exprs: Vec<String>) -> String {
        let id = match expr_type {
            ExprType::Precompile => {
                self.comps_counter += 1;
                format!("comp{}", self.comps_counter)
            }
            ExprType::Constraint => {
                self.exprs_counter += 1;
                format!("e{}", self.exprs_counter)
            }
        };
        self.concatxs.push(ConcatX::new(id.clone(), exprs, expr_type));
        id
    }

    /// Adds a boolmaj to the Datan instance.
    pub fn add_boolmaj(&mut self, expr_type: ExprType, expr1: String, expr2: String, expr3: String) -> String {
        let id = match expr_type {
            ExprType::Precompile => {
                self.comps_counter += 1;
                format!("comp{}", self.comps_counter)
            }
            ExprType::Constraint => {
                self.exprs_counter += 1;
                format!("e{}", self.exprs_counter)
            }
        };
        self.boolmjs.push(BoolMaj::new(id.clone(), expr1, expr2, expr3, expr_type));
        id
    }   
}

pub use trans::to_datan;
pub use utils::serialize_datan;
