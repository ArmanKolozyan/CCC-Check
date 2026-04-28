//! Walk functions for Circom AST visitors

use circom_pest_ast::*;
use super::cvmut::CircomVisitorMut;

/// Walk a File
pub fn walk_file<'ast, V: CircomVisitorMut<'ast>>(visitor: &mut V, file: &mut File<'ast>) {
    // Visit declarations
    for decl in &mut file.declarations {
        visitor.visit_symbol_declaration(decl);
    }
}

/// Walk a symbol declaration
pub fn walk_symbol_declaration<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    decl: &mut SymbolDeclaration<'ast>
) {
    match decl {
        SymbolDeclaration::Include(inc) => visitor.visit_include(inc),
        SymbolDeclaration::Template(template) => visitor.visit_template(template),
        SymbolDeclaration::Function(function) => visitor.visit_function(function),
        SymbolDeclaration::MainComponent(main) => visitor.visit_main_component(main),
    }
}

/// Walk a template definition
pub fn walk_template<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    template: &mut TemplateDefinition<'ast>
) {
    visitor.visit_identifier(&mut template.id);
    
    for param in &mut template.params {
        visitor.visit_identifier(param);
    }
    
    for stmt in &mut template.statements {
        visitor.visit_statement(stmt);
    }
}

/// Walk a function definition
pub fn walk_function<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    function: &mut FunctionDefinition<'ast>
) {
    visitor.visit_identifier(&mut function.id);
    
    for param in &mut function.params {
        visitor.visit_identifier(param);
    }
    
    for stmt in &mut function.statements {
        visitor.visit_statement(stmt);
    }
}

/// Walk a main component
pub fn walk_main_component<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    main: &mut MainComponent<'ast>
) {
    for signal in &mut main.public_signals {
        visitor.visit_identifier(signal);
    }
    
    // Visit the component instantiation
    visitor.visit_identifier(&mut main.component_instantiation.id);
    
    for arg in &mut main.component_instantiation.args {
        visitor.visit_expression(arg);
    }
}

/// Walk a statement
pub fn walk_statement<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    stmt: &mut Statement<'ast>
) {
    match stmt {
        Statement::For(for_stmt) => visitor.visit_for_statement(for_stmt),
        Statement::While(while_stmt) => visitor.visit_while_statement(while_stmt),
        Statement::If(if_stmt) => visitor.visit_if_statement(if_stmt),
        Statement::Log(log_stmt) => visitor.visit_expression(&mut log_stmt.expression),
        Statement::Return(ret_stmt) => {
            if let Some(expr) = &mut ret_stmt.expression {
                visitor.visit_expression(expr);
            }
        },
        Statement::Assert(assert_stmt) => visitor.visit_expression(&mut assert_stmt.expression),
        Statement::Signal(signal_stmt) => visitor.visit_signal_statement(signal_stmt),
        Statement::Component(comp_stmt) => visitor.visit_component_statement(comp_stmt),
        Statement::Variable(var_stmt) => visitor.visit_variable_statement(var_stmt),
        Statement::Expression(expr) => visitor.visit_expression(expr),
    }
}

/// Walk an expression
pub fn walk_expression<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    expr: &mut Expression<'ast>
) {
    match expr {
        Expression::Binary(bin_expr) => visitor.visit_binary_expression(bin_expr),
        Expression::Unary(un_expr) => visitor.visit_unary_expression(un_expr),
        Expression::Postfix(postfix) => visitor.visit_postfix_expression(postfix),
        Expression::Identifier(id) => visitor.visit_identifier(id),
        Expression::Number(_) => { /* Numbers don't have children to visit */ },
        Expression::Array(array_expr) => visitor.visit_array_expression(array_expr),
    }
}

/// Walk an include declaration
pub fn walk_include<'ast, V: CircomVisitorMut<'ast>>(
    _visitor: &mut V, 
    _inc: &mut Include<'ast>
) {
    // Include doesn't have children to visit
}

/// Walk a component statement
pub fn walk_component_statement<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    comp: &mut ComponentStatement<'ast>
) {
    visitor.visit_assignee(&mut comp.assignee);
    
    if let Some(inst) = &mut comp.value {
        visitor.visit_identifier(&mut inst.id);
        
        for arg in &mut inst.args {
            visitor.visit_expression(arg);
        }
    }
}

/// Walk a signal statement
pub fn walk_signal_statement<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    signal: &mut SignalStatement<'ast>
) {
    match signal {
        SignalStatement::SignalDecl(decl) => visitor.visit_signal_declaration(decl),
        
        SignalStatement::SignalAssignmentStatement(assign) => match assign {
            SignalAssignmentStatement::LeftArrow(left) => {
                walk_assignee_target(visitor, &mut left.target);
                walk_ternary_or_expression(visitor, &mut left.value);
            },
            SignalAssignmentStatement::RightArrow(right) => {
                walk_ternary_or_expression(visitor, &mut right.value);
                walk_assignee_target(visitor, &mut right.target);
            },
        },
        
        SignalStatement::SignalAssignmentConstraintStatement(constraint) => match constraint {
            SignalAssignmentConstraintStatement::LeftArrow(left) => {
                walk_assignee_target(visitor, &mut left.target);
                walk_ternary_or_expression(visitor, &mut left.value);
            },
            SignalAssignmentConstraintStatement::RightArrow(right) => {
                walk_ternary_or_expression(visitor, &mut right.value);
                walk_assignee_target(visitor, &mut right.target);
            },
        },
        
        SignalStatement::ConstraintStatement(constraint) => {
            visitor.visit_constraint_statement(constraint);
        },
    }
}

/// Walk a signal declaration
pub fn walk_signal_declaration<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    decl: &mut SignalDeclaration<'ast>
) {
    for assignee in &mut decl.assignees {
        visitor.visit_assignee(assignee);
    }
    
    if let Some(expr) = &mut decl.value {
        visitor.visit_expression(expr);
    }
}

/// Walk a variable statement
pub fn walk_variable_statement<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V,
    var: &mut VariableStatement<'ast>
) {
    for decl in &mut var.declarations {
        visitor.visit_assignee(&mut decl.assignee);

        if let Some(value) = &mut decl.value {
            walk_ternary_or_expression(visitor, value);
        }
    }
}

/// Walk an if statement
pub fn walk_if_statement<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    if_stmt: &mut IfStatement<'ast>
) {
    visitor.visit_expression(&mut if_stmt.condition);
    
    for stmt in &mut if_stmt.then_statements {
        visitor.visit_statement(stmt);
    }
    
    for else_if in &mut if_stmt.else_if_branches {
        visitor.visit_expression(&mut else_if.condition);
        
        for stmt in &mut else_if.statements {
            visitor.visit_statement(stmt);
        }
    }
    
    if let Some(else_branch) = &mut if_stmt.else_branch {
        for stmt in &mut else_branch.statements {
            visitor.visit_statement(stmt);
        }
    }
}

/// Walk a for statement
pub fn walk_for_statement<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    for_stmt: &mut ForStatement<'ast>
) {
    visitor.visit_variable_statement(&mut for_stmt.var);
    visitor.visit_expression(&mut for_stmt.condition);
    visitor.visit_expression(&mut for_stmt.increment);
    
    for stmt in &mut for_stmt.statements {
        visitor.visit_statement(stmt);
    }
}

/// Walk a while statement
pub fn walk_while_statement<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    while_stmt: &mut WhileStatement<'ast>
) {
    visitor.visit_expression(&mut while_stmt.condition);
    
    for stmt in &mut while_stmt.statements {
        visitor.visit_statement(stmt);
    }
}

/// Walk an assignee
pub fn walk_assignee<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    assignee: &mut Assignee<'ast>
) {
    visitor.visit_identifier(&mut assignee.id);
    
    for access in &mut assignee.accesses {
        match access {
            AssigneeAccess::Select(array_access) => {
                visitor.visit_expression(&mut array_access.expression);
            },
            AssigneeAccess::Dot(dot_access) => {
                visitor.visit_identifier(&mut dot_access.inner);
                
                if let Some(array_access) = &mut dot_access.array_access {
                    visitor.visit_expression(&mut array_access.expression);
                }
            },
        }
    }
}

/// Walk an assignee target
pub fn walk_assignee_target<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    target: &mut AssigneeTarget<'ast>
) {
    match target {
        AssigneeTarget::Single(assignee) => visitor.visit_assignee(assignee),
        AssigneeTarget::Tuple(tuple) => {
            for assignee in &mut tuple.assignees {
                visitor.visit_assignee(assignee);
            }
        },
    }
}

/// Walk a ternary or expression
pub fn walk_ternary_or_expression<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    expr: &mut TernaryOrExpression<'ast>
) {
    match expr {
        TernaryOrExpression::Ternary(ternary) => visitor.visit_ternary_expression(ternary),
        TernaryOrExpression::Expression(expr) => visitor.visit_expression(expr),
    }
}

/// Walk an array expression
pub fn walk_array_expression<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    array: &mut ArrayExpression<'ast>
) {
    for elem in &mut array.elements {
        visitor.visit_expression(elem);
    }
}

/// Walk a binary expression
pub fn walk_binary_expression<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    binary: &mut BinaryExpression<'ast>
) {
    visitor.visit_expression(&mut binary.left);
    visitor.visit_expression(&mut binary.right);
}

/// Walk a ternary expression
pub fn walk_ternary_expression<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    ternary: &mut TernaryExpression<'ast>
) {
    visitor.visit_expression(&mut ternary.condition);
    visitor.visit_expression(&mut ternary.consequence);
    visitor.visit_expression(&mut ternary.alternative);
}

/// Walk a unary expression
pub fn walk_unary_expression<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    unary: &mut UnaryExpression<'ast>
) {
    visitor.visit_expression(&mut unary.expression);
}

/// Walk a postfix expression
pub fn walk_postfix_expression<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    postfix: &mut PostfixExpression<'ast>
) {
    visitor.visit_expression(&mut postfix.base);
    
    for access in &mut postfix.access {
        match access {
            Access::ArrayAccess(array_access) => {
                visitor.visit_expression(&mut array_access.expression);
            },
            Access::CallAccess(call_access) => {
                for arg in &mut call_access.args {
                    visitor.visit_expression(arg);
                }
            },
            Access::DotAccess(dot_access) => {
                visitor.visit_identifier(&mut dot_access.inner);
                
                if let Some(array_access) = &mut dot_access.array_access {
                    visitor.visit_expression(&mut array_access.expression);
                }
            },
            Access::Increment(_) | Access::Decrement(_) => {
                // These don't have children to visit
            },
        }
    }
}

/// Walk a constraint statement
pub fn walk_constraint_statement<'ast, V: CircomVisitorMut<'ast>>(
    visitor: &mut V, 
    constraint: &mut ConstraintStatement<'ast>
) {
    visitor.visit_expression(&mut constraint.lhs);
    visitor.visit_expression(&mut constraint.rhs);
}