//! Mutable visitor for Circom ASTs

use super::walkfns::*;
use circom_pest_ast::*;

/// Trait for mutable visitors of Circom ASTs
pub trait CircomVisitorMut<'ast>: Sized {
    /// Visit the top-level file
    fn visit_file(&mut self, file: &mut File<'ast>) {
        walk_file(self, file);
    }

    /// Visit symbol declarations
    fn visit_symbol_declaration(&mut self, decl: &mut SymbolDeclaration<'ast>) {
        walk_symbol_declaration(self, decl);
    }

    /// Visit templates
    fn visit_template(&mut self, template: &mut TemplateDefinition<'ast>) {
        walk_template(self, template);
    }

    /// Visit functions
    fn visit_function(&mut self, function: &mut FunctionDefinition<'ast>) {
        walk_function(self, function);
    }

    /// Visit main component
    fn visit_main_component(&mut self, main: &mut MainComponent<'ast>) {
        walk_main_component(self, main);
    }

    /// Visit statements
    fn visit_statement(&mut self, stmt: &mut Statement<'ast>) {
        walk_statement(self, stmt);
    }

    /// Visit expressions
    fn visit_expression(&mut self, expr: &mut Expression<'ast>) {
        walk_expression(self, expr);
    }

    /// Visit include declarations
    fn visit_include(&mut self, inc: &mut Include<'ast>) {
        walk_include(self, inc);
    }

    /// Visit component statements
    fn visit_component_statement(&mut self, comp: &mut ComponentStatement<'ast>) {
        walk_component_statement(self, comp);
    }

    /// Visit signal statements
    fn visit_signal_statement(&mut self, signal: &mut SignalStatement<'ast>) {
        walk_signal_statement(self, signal);
    }

    /// Visit signal declarations
    fn visit_signal_declaration(&mut self, decl: &mut SignalDeclaration<'ast>) {
        walk_signal_declaration(self, decl);
    }

    /// Visit variable statements
    fn visit_variable_statement(&mut self, var: &mut VariableStatement<'ast>) {
        walk_variable_statement(self, var);
    }

    /// Visit control flow
    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement<'ast>) {
        walk_if_statement(self, if_stmt);
    }

    /// Visit for statements
    fn visit_for_statement(&mut self, for_stmt: &mut ForStatement<'ast>) {
        walk_for_statement(self, for_stmt);
    }

    /// Visit while statements
    fn visit_while_statement(&mut self, while_stmt: &mut WhileStatement<'ast>) {
        walk_while_statement(self, while_stmt);
    }

    /// Visit assignees
    fn visit_assignee(&mut self, assignee: &mut Assignee<'ast>) {
        walk_assignee(self, assignee);
    }

    /// Visit identifiers
    fn visit_identifier(&mut self, _id: &mut Identifier<'ast>) {
        // Default implementation does nothing
    }

    /// Visit arrays
    fn visit_array_expression(&mut self, array: &mut ArrayExpression<'ast>) {
        walk_array_expression(self, array);
    }

    /// Visit binary expressions
    fn visit_binary_expression(&mut self, binary: &mut BinaryExpression<'ast>) {
        walk_binary_expression(self, binary);
    }

    /// Visit ternary expressions
    fn visit_ternary_expression(&mut self, ternary: &mut TernaryExpression<'ast>) {
        walk_ternary_expression(self, ternary);
    }

    /// Visit unary expressions
    fn visit_unary_expression(&mut self, unary: &mut UnaryExpression<'ast>) {
        walk_unary_expression(self, unary);
    }

    /// Visit postfix expressions
    fn visit_postfix_expression(&mut self, postfix: &mut PostfixExpression<'ast>) {
        walk_postfix_expression(self, postfix);
    }

    /// Visit constraints
    fn visit_constraint_statement(&mut self, constraint: &mut ConstraintStatement<'ast>) {
        walk_constraint_statement(self, constraint);
    }
}