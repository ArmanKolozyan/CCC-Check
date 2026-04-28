//! Type inference for Circom expressions

use super::{CircomStatementWalker, CircomType};
use super::super::cvmut::CircomVisitorMut;
use super::walkfns::*;
use circom_pest_ast as ast;

/// Type inference visitor for Circom expressions
pub struct CircomExpressionTyper<'ast, 'walk, 'ctx> {
    /// The statement walker context
    pub walker: &'walk mut CircomStatementWalker<'ast, 'ctx>,
    /// The inferred type of the current expression
    pub ty: Option<CircomType>,
}

impl<'ast, 'walk, 'ctx> CircomExpressionTyper<'ast, 'walk, 'ctx> {
    /// Create a new expression typer
    pub fn new(walker: &'walk mut CircomStatementWalker<'ast, 'ctx>) -> Self {
        Self {
            walker,
            ty: None,
        }
    }

    /// Take the inferred type, leaving None in its place
    pub fn take_type(&mut self) -> Option<CircomType> {
        self.ty.take()
    }
}

impl<'ast, 'walk, 'ctx> CircomVisitorMut<'ast> for CircomExpressionTyper<'ast, 'walk, 'ctx> {
    fn visit_ternary_or_expression(&mut self, expr: &mut ast::TernaryOrExpression<'ast>) -> Result<(), String> {
        // Visit ternary or regular expression
        match expr {
            ast::TernaryOrExpression::Expression(e) => self.visit_expression(e),
            ast::TernaryOrExpression::Ternary(ternary) => {
                // Visit all parts and use the consequence type
                self.visit_expression(&mut ternary.condition.clone())?;
                self.visit_expression(&mut ternary.consequence.clone())?;
                // Type is the type of the consequence (then branch)
                Ok(())
            }
        }
    }

    fn visit_expression(&mut self, expr: &mut ast::Expression<'ast>) -> Result<(), String> {
        // Visit the expression and infer its type
        match expr {
            ast::Expression::Binary(bin_expr) => self.visit_binary_expression(bin_expr),
            ast::Expression::Unary(un_expr) => self.visit_unary_expression(un_expr),
            ast::Expression::Identifier(id) => self.visit_identifier_expression(id),
            ast::Expression::Number(_) => {
                // Numbers are always field elements in Circom
                self.ty = Some(CircomType::Field);
                Ok(())
            },
            ast::Expression::Array(array) => self.visit_array_expression(array),
            ast::Expression::Postfix(postfix) => self.visit_postfix_expression(postfix),
        }
    }

    fn visit_binary_expression(&mut self, bin_expr: &mut ast::BinaryExpression<'ast>) -> Result<(), String> {
        // Type check binary expressions
        // Most binary operations in Circom result in Field type
        walk_binary_expression(self, bin_expr)?;

        // Binary operations on fields produce fields
        // Binary operations on arrays are not supported
        self.ty = Some(CircomType::Field);
        Ok(())
    }

    fn visit_unary_expression(&mut self, un_expr: &mut ast::UnaryExpression<'ast>) -> Result<(), String> {
        // Type check unary expressions
        walk_unary_expression(self, un_expr)?;

        // Unary operations preserve the type or produce Field
        // For now, assume Field (covers -, !, ~)
        self.ty = Some(CircomType::Field);
        Ok(())
    }

    fn visit_identifier_expression(&mut self, id: &mut ast::Identifier<'ast>) -> Result<(), String> {
        // Lookup identifier type in the walker's variable map
        if let Some(var_type) = self.walker.vars.get(&id.value) {
            self.ty = Some(var_type.clone());
        } else {
            // Unknown identifier - default to Field
            self.ty = Some(CircomType::Field);
        }
        Ok(())
    }

    fn visit_array_expression(&mut self, array: &mut ast::ArrayExpression<'ast>) -> Result<(), String> {
        // Type check array expressions
        walk_array_expression(self, array)?;

        // Get the type of the first element (if any)
        if let Some(first) = array.elements.first() {
            let mut typer = CircomExpressionTyper::new(self.walker);
            typer.visit_ternary_or_expression(&mut first.clone())?;
            if let Some(elem_ty) = typer.take_type() {
                self.ty = Some(CircomType::Array(Box::new(elem_ty), array.elements.len()));
                return Ok(());
            }
        }

        // Empty array or couldn't infer element type - default to Field array
        self.ty = Some(CircomType::Array(Box::new(CircomType::Field), array.elements.len()));
        Ok(())
    }

    fn visit_postfix_expression(&mut self, postfix: &mut ast::PostfixExpression<'ast>) -> Result<(), String> {
        // Type check postfix expressions
        walk_postfix_expression(self, postfix)?;

        // The type depends on the access operations
        // For array access: unwrap the array type
        // For dot access: lookup the signal type
        // For increment/decrement: preserve the type
        // For now, default to Field
        self.ty = Some(CircomType::Field);
        Ok(())
    }
}