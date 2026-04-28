use from_pest::FromPest;
use from_pest::Void;
use from_pest::ConversionError;
use pest::error::Error as PestError;
use pest::iterators::Pairs;
use std::fmt;
use circom_parser::parse;
use circom_parser::Rule;

#[macro_use]
extern crate lazy_static;

pub use ast::*;

mod ast {
    use from_pest::FromPest;
    use pest_ast::FromPest;
    use pest::iterators::{Pair, Pairs};
    use pest::prec_climber::{Assoc, Operator, PrecClimber};
    use pest::Span;
    use from_pest::Void;
    use from_pest::ConversionError;
    use crate::Rule;


    lazy_static! {
        static ref PREC_CLIMBER: PrecClimber<Rule> = build_precedence_climber();
    }
    
    fn build_precedence_climber() -> PrecClimber<Rule> {
        PrecClimber::new(vec![
            // Lowest precedence
            Operator::new(Rule::op_or, Assoc::Left),
            Operator::new(Rule::op_and, Assoc::Left),
            Operator::new(Rule::op_bit_xor, Assoc::Left),
            Operator::new(Rule::op_bit_and, Assoc::Left),
            Operator::new(Rule::op_bit_or, Assoc::Left),
            Operator::new(Rule::op_equal, Assoc::Left) | Operator::new(Rule::op_not_equal, Assoc::Left),
            Operator::new(Rule::op_lt, Assoc::Left) | Operator::new(Rule::op_gt, Assoc::Left) |
            Operator::new(Rule::op_lte, Assoc::Left) | Operator::new(Rule::op_gte, Assoc::Left),
            Operator::new(Rule::op_left_shift, Assoc::Left) | Operator::new(Rule::op_right_shift, Assoc::Left),
            Operator::new(Rule::op_add, Assoc::Left) | Operator::new(Rule::op_sub, Assoc::Left),
            Operator::new(Rule::op_mul, Assoc::Left) | Operator::new(Rule::op_div, Assoc::Left) | 
            Operator::new(Rule::op_idiv, Assoc::Left) | Operator::new(Rule::op_mod, Assoc::Left),
            // Highest precedence
            Operator::new(Rule::op_pow, Assoc::Right),
        ])
    }
    
    // Create an Expression from left and right terms and an operator
    // Precondition: `pair` MUST be a binary operator
    fn infix_rule<'ast>(
        lhs: Box<Expression<'ast>>,
        pair: Pair<'ast, Rule>,
        rhs: Box<Expression<'ast>>,
    ) -> Box<Expression<'ast>> {
        // a + b spans from the start of a to the end of b
        let (start, _) = lhs.span().split();
        let (_, end) = rhs.span().split();
        let span = start.span(&end);

        Box::new(match pair.as_rule() {
            Rule::op_add => Expression::binary(OpBinary::AddOp, lhs, rhs, span),
            Rule::op_sub => Expression::binary(OpBinary::SubOp, lhs, rhs, span),
            Rule::op_mul => Expression::binary(OpBinary::MulOp, lhs, rhs, span),
            Rule::op_div => Expression::binary(OpBinary::DivOp, lhs, rhs, span),
            Rule::op_pow => Expression::binary(OpBinary::PowOp, lhs, rhs, span),
            Rule::op_idiv => Expression::binary(OpBinary::IDivOp, lhs, rhs, span),
            Rule::op_mod => Expression::binary(OpBinary::ModOp, lhs, rhs, span),
            Rule::op_equal => Expression::binary(OpBinary::EqualOp, lhs, rhs, span),
            Rule::op_not_equal => Expression::binary(OpBinary::NotEqualOp, lhs, rhs, span),
            Rule::op_lte => Expression::binary(OpBinary::LteOp, lhs, rhs, span),
            Rule::op_lt => Expression::binary(OpBinary::LtOp, lhs, rhs, span),
            Rule::op_gte => Expression::binary(OpBinary::GteOp, lhs, rhs, span),
            Rule::op_gt => Expression::binary(OpBinary::GtOp, lhs, rhs, span),
            Rule::op_or => Expression::binary(OpBinary::OrOp, lhs, rhs, span),
            Rule::op_and => Expression::binary(OpBinary::AndOp, lhs, rhs, span),
            Rule::op_bit_xor => Expression::binary(OpBinary::BitXorOp, lhs, rhs, span),
            Rule::op_bit_and => Expression::binary(OpBinary::BitAndOp, lhs, rhs, span),
            Rule::op_bit_or => Expression::binary(OpBinary::BitOrOp, lhs, rhs, span),
            Rule::op_right_shift => Expression::binary(OpBinary::RightShiftOp, lhs, rhs, span),
            Rule::op_left_shift => Expression::binary(OpBinary::LeftShiftOp, lhs, rhs, span),
            _ => unreachable!(),
        })
    }

    // Create an Expression from an `expression`. `build_factor` turns each term into an `Expression` and `infix_rule` turns each (Expression, operator, Expression) into an Expression
    pub fn climb(pair: Pair<Rule>) -> Box<Expression> {
        PREC_CLIMBER.climb(pair.into_inner(), build_factor, infix_rule)
    }

    // Create an Expression from a `unaried_term`.
    // Precondition: `pair` MUST be a `unaried_term`
    fn build_factor(pair: Pair<Rule>) -> Box<Expression> {
        // Get the text of the pair
        let text = pair.as_str().trim();
        
        // Handle hex numbers
        if text.starts_with("0x") {
            let span = pair.as_span();
            return Box::new(Expression::Number(Number::Hex(HexNumber { span })));
        }
        
        // Handle both positive and negative decimal numbers
        if (text.starts_with('-') && text[1..].chars().all(|c| c.is_digit(10))) || 
           text.chars().all(|c| c.is_digit(10)) {
            let span = pair.as_span();
            return Box::new(Expression::Number(Number::Decimal(DecimalNumber { span })));
        }
        
        // Otherwise try the normal path
        match UnariedTerm::from_pest(&mut Pairs::single(pair.clone())) {
            Ok(term) => Box::new(Expression::from(term)),
            Err(e) => {
                // Print debug info and panic with more information
                println!("Error: {:?}", e);
                println!("Failed to parse term: {:?}", pair.as_rule());
                println!("Text: {}", text);
                panic!("Failed to parse term: {:?}", pair.as_rule());
            }
        }
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::file))]
    pub struct File<'ast> {
        pub pragma: Option<Pragma<'ast>>,
        pub declarations: Vec<SymbolDeclaration<'ast>>,
        pub eoi: EOI,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::pragma_decl))]
    pub struct Pragma<'ast> {
        pub directive: PragmaDirective<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::pragma_directive))]
    pub enum PragmaDirective<'ast> {
        Circom(Version<'ast>),
        CustomTemplates,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::version))]
    pub struct Version<'ast> {
        #[pest_ast(outer(with(span_into_str)))]
        pub value: String,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::EOI))]
    #[allow(clippy::upper_case_acronyms)]
    pub struct EOI;

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::symbol_declaration))]
    pub enum SymbolDeclaration<'ast> {
        Include(Include<'ast>),
        Template(TemplateDefinition<'ast>),
        Function(FunctionDefinition<'ast>),
        MainComponent(MainComponent<'ast>),
    }

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::include_decl))]
    pub struct Include<'ast> {
        pub path: PathString<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::pathstring))]
    pub struct PathString<'ast> {
        #[pest_ast(outer(with(span_into_str)))]
        pub value: String,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::template_decl))]
    pub struct TemplateDefinition<'ast> {
        pub modifiers: Option<TemplateModifiers<'ast>>,
        pub id: Identifier<'ast>,
        pub params: Vec<Identifier<'ast>>,
        pub statements: Vec<Statement<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::template_modifiers))]
    pub struct TemplateModifiers<'ast> {
        #[pest_ast(outer(with(span_into_str)))]
        pub value: String,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::function_decl))]
    pub struct FunctionDefinition<'ast> {
        pub id: Identifier<'ast>,
        pub params: Vec<Identifier<'ast>>,
        pub statements: Vec<Statement<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::assignee))]
    pub struct Assignee<'ast> {
        pub id: Identifier<'ast>,
        pub accesses: Vec<AssigneeAccess<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::assignee_access))]
    pub enum AssigneeAccess<'ast> {
        Select(ArrayAccess<'ast>),
        Dot(DotAccess<'ast>),
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::array_access))]
    pub struct ArrayAccess<'ast> {
        pub expression: Expression<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::dot_access))]
    pub struct DotAccess<'ast> {
        pub inner: Identifier<'ast>,
        pub array_access: Option<ArrayAccess<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::call_access))]
    pub struct CallAccess<'ast> {
        pub args: Vec<Expression<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::identifier))]
    pub struct Identifier<'ast> {
        #[pest_ast(outer(with(span_into_str)))]
        pub value: String,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[allow(clippy::large_enum_variant)]
    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::statement))]
    pub enum Statement<'ast> {
        For(ForStatement<'ast>),
        While(WhileStatement<'ast>),
        If(IfStatement<'ast>),
        Log(LogStatement<'ast>),
        Return(ReturnStatement<'ast>),
        Assert(AssertStatement<'ast>),
        Signal(SignalStatement<'ast>),
        Component(ComponentStatement<'ast>),
        Variable(VariableStatement<'ast>),
        Expression(Expression<'ast>),
    }

    impl<'ast> Statement<'ast> {
        pub fn span(&self) -> &Span<'ast> {
            match self {
                Statement::For(stmt) => &stmt.span,
                Statement::While(stmt) => &stmt.span,
                Statement::If(stmt) => &stmt.span,
                Statement::Log(stmt) => &stmt.span,
                Statement::Return(stmt) => &stmt.span,
                Statement::Assert(stmt) => &stmt.span,
                Statement::Signal(stmt) => stmt.span(),
                Statement::Component(stmt) => &stmt.span,
                Statement::Variable(stmt) => &stmt.span,
                Statement::Expression(stmt) => stmt.span(),
            }
        }   
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::for_statement))]
    pub struct ForStatement<'ast> {
        pub var: VariableStatement<'ast>,
        pub condition: Expression<'ast>,
        pub increment: Expression<'ast>,
        pub statements: Vec<Statement<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::while_statement))]
    pub struct WhileStatement<'ast> {
        pub condition: Expression<'ast>,
        pub statements: Vec<Statement<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::if_statement))]
    pub struct IfStatement<'ast> {
        pub condition: Expression<'ast>,
        pub then_statements: Vec<Statement<'ast>>,
        pub else_if_branches: Vec<ElseIfBranch<'ast>>,
        pub else_branch: Option<ElseBranch<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }
    
    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::else_if_branch))]
    pub struct ElseIfBranch<'ast> {
        pub condition: Expression<'ast>,
        pub statements: Vec<Statement<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }
    
    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::else_branch))]
    pub struct ElseBranch<'ast> {
        pub statements: Vec<Statement<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }
    #[derive(Debug, FromPest, PartialEq,Clone)]
    #[pest_ast(rule(Rule::log_statement))]
    pub struct LogStatement<'ast> {
        pub expression: Expression<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::return_statement))]
    pub struct ReturnStatement<'ast> {
        pub expression: Option<Expression<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::assert_statement))]
    pub struct AssertStatement<'ast> {
        pub expression: Expression<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::signal_statement))]
    pub enum SignalStatement<'ast> {
        SignalDecl(SignalDeclaration<'ast>),
        SignalAssignmentStatement(SignalAssignmentStatement<'ast>),
        SignalAssignmentConstraintStatement(SignalAssignmentConstraintStatement<'ast>),
        ConstraintStatement(ConstraintStatement<'ast>),
    }

    impl<'ast> SignalStatement<'ast> {
        pub fn span(&self) -> &Span<'ast> {
            match self {
                SignalStatement::SignalDecl(stmt) => &stmt.span,
                SignalStatement::SignalAssignmentStatement(stmt) => match stmt {
                    SignalAssignmentStatement::LeftArrow(left) => &left.span,
                    SignalAssignmentStatement::RightArrow(right) => &right.span,
                },
                SignalStatement::SignalAssignmentConstraintStatement(stmt) => match stmt {
                    SignalAssignmentConstraintStatement::LeftArrow(left) => &left.span,
                    SignalAssignmentConstraintStatement::RightArrow(right) => &right.span,
                },
                SignalStatement::ConstraintStatement(stmt) => &stmt.span,
            }
        }
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::signal_decl))]
    pub struct SignalDeclaration<'ast> {
        pub bus: Option<BusModifier>,
        pub signal_type: Option<SignalType>,
        pub tags: Option<SignalTags<'ast>>,
        pub assignees: Vec<Assignee<'ast>>,
        pub value: Option<Expression<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::signal_tags))]
    pub struct SignalTags<'ast> {
        pub tags: Vec<Identifier<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::bus_modifier))]
    pub struct BusModifier;

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::signal_type))]
    pub enum SignalType {
        Input(InputSignal),
        Output(OutputSignal),
    }

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::input_signal))]
    pub struct InputSignal {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::output_signal))]
    pub struct OutputSignal {}
    
    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::signal_assignment_statement))]
    pub enum SignalAssignmentStatement<'ast> {
        LeftArrow(SignalAssignmentLeft<'ast>),
        RightArrow(SignalAssignmentRight<'ast>),
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::signal_assignment_left))]
    pub struct SignalAssignmentLeft<'ast> {
        pub target: AssigneeTarget<'ast>,
        pub value: TernaryOrExpression<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::signal_assignment_right))]
    pub struct SignalAssignmentRight<'ast> {
        pub value: TernaryOrExpression<'ast>,
        pub target: AssigneeTarget<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::signal_assignment_constraint_statement))]
    pub enum SignalAssignmentConstraintStatement<'ast> {
        LeftArrow(SignalConstraintLeft<'ast>),
        RightArrow(SignalConstraintRight<'ast>),
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::signal_constraint_left))]
    pub struct SignalConstraintLeft<'ast> {
        pub target: AssigneeTarget<'ast>,
        pub value: TernaryOrExpression<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::signal_constraint_right))]
    pub struct SignalConstraintRight<'ast> {
        pub value: TernaryOrExpression<'ast>,
        pub target: AssigneeTarget<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::constraint_statement))]
    pub struct ConstraintStatement<'ast> {
        pub lhs: Expression<'ast>,
        pub rhs: Expression<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }
    
    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::component_statement))]
    pub struct ComponentStatement<'ast> {
        pub assignee: Assignee<'ast>,
        pub value: Option<ComponentInstantiation<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::component_instantiation))]
    pub struct ComponentInstantiation<'ast> {
        pub id: Identifier<'ast>,
        pub args: Vec<Expression<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::variable_declaration))]
    pub struct VariableDeclaration<'ast> {
        pub assignee: Assignee<'ast>,
        pub op: Option<VarAssignmentOp>,
        pub value: Option<TernaryOrExpression<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::variable_statement))]
    pub struct VariableStatement<'ast> {
        pub declarations: Vec<VariableDeclaration<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::main_component))]
    pub struct MainComponent<'ast> {
        pub public_signals: Vec<Identifier<'ast>>,
        pub component_instantiation: ComponentInstantiation<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::ternary_or_expression))]
    pub enum TernaryOrExpression<'ast> {
        Ternary(TernaryExpression<'ast>),
        Expression(Expression<'ast>),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Expression<'ast> {
        Binary(BinaryExpression<'ast>),
        Unary(UnaryExpression<'ast>),
        Postfix(PostfixExpression<'ast>),
        Identifier(Identifier<'ast>),
        Number(Number<'ast>),
        Array(ArrayExpression<'ast>),
    }

    impl<'ast> FromPest<'ast> for Expression<'ast> {
        type Rule = Rule;
        type FatalError = Void;

        // We implement AST creation manually here for Expression
        // `pest` should yield an `expression` which we can generate AST with, based on precedence rules
        fn from_pest(pest: &mut Pairs<'ast, Rule>) -> Result<Self, ConversionError<Void>> {
            let mut clone = pest.clone();
            let pair = clone.next().ok_or(::from_pest::ConversionError::NoMatch)?;
            match pair.as_rule() {
                Rule::expression => {
                    *pest = clone;
                    Ok(*climb(pair))
                }
                _ => {
                    Err(ConversionError::NoMatch)
                }
            }
        }
    }

    impl<'ast> Expression<'ast> {
        pub fn binary(
            op: OpBinary,
            left: Box<Expression<'ast>>,
            right: Box<Expression<'ast>>,
            span: Span<'ast>,
        ) -> Self {
            Self::Binary(BinaryExpression { op, left, right, span })
        }

        pub fn span(&self) -> &Span<'ast> {
            match self {
                Expression::Binary(expr) => &expr.span,
                Expression::Unary(expr) => &expr.span,
                Expression::Postfix(expr) => &expr.span,
                Expression::Identifier(expr) => &expr.span,
                Expression::Number(expr) => &expr.span(),
                Expression::Array(expr) => &expr.span,
            }
        }
    }

    impl<'ast> From<Term<'ast>> for Expression<'ast> {
        fn from(t: Term<'ast>) -> Self {
            match t {
                Term::Expression(e) => e,
                Term::Primary(p) => p.into(),
            }
        }
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::term))]
    pub enum Term<'ast> {
        Expression(Expression<'ast>),
        Primary(PrimaryExpression<'ast>),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct BinaryExpression<'ast> {
        pub op: OpBinary,
        pub left: Box<Expression<'ast>>,
        pub right: Box<Expression<'ast>>,
        pub span: Span<'ast>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct UnaryExpression<'ast> {
        pub op: OpUnary,
        pub expression: Box<Expression<'ast>>,
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::unaried_term))]
    pub struct UnariedTerm<'ast> {
        pub op: Option<OpUnary>,
        pub expression: PostfixedTerm<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    impl<'ast> From<UnariedTerm<'ast>> for Expression<'ast> {
        fn from(t: UnariedTerm<'ast>) -> Self {
            let expression = Expression::from(t.expression);
            match t.op {
                Some(op) => Expression::Unary(UnaryExpression { 
                    op, 
                    expression: Box::new(expression), 
                    span: t.span 
                }),
                None => expression,
            }
        }
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::postfixed_term))]
    pub struct PostfixedTerm<'ast> {
        pub base: Term<'ast>,
        pub access: Vec<Access<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct PostfixExpression<'ast> {
        pub base: Box<Expression<'ast>>,
        pub access: Vec<Access<'ast>>,
        pub span: Span<'ast>,
    }

    impl<'ast> From<PostfixedTerm<'ast>> for Expression<'ast> {
        fn from(t: PostfixedTerm<'ast>) -> Self {
            let base = Expression::from(t.base);
            let accesses = t.access;
            if accesses.is_empty() {
                base
            } else {
                Expression::Postfix(PostfixExpression {
                    base: Box::new(base),
                    access: accesses,
                    span: t.span,
                })
            }
        }
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::access))]
    pub enum Access<'ast> {
        ArrayAccess(ArrayAccess<'ast>),
        CallAccess(CallAccess<'ast>),
        DotAccess(DotAccess<'ast>),
        Increment(IncrementOp),
        Decrement(DecrementOp),
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::primary_expression))]
    pub enum PrimaryExpression<'ast> {
        Identifier(Identifier<'ast>),
        Arr(ArrayExpression<'ast>),
        Num(Number<'ast>),
    }

    impl<'ast> From<PrimaryExpression<'ast>> for Expression<'ast> {
        fn from(t: PrimaryExpression<'ast>) -> Self {
            match t {
                PrimaryExpression::Identifier(id) => Expression::Identifier(id),
                PrimaryExpression::Arr(arr) => Expression::Array(arr),
                PrimaryExpression::Num(num) => Expression::Number(num),
            }
        }
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::array_expr))]
    pub struct ArrayExpression<'ast> {
        pub elements: Vec<Expression<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    } 

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::ternary_expression))]
    pub struct TernaryExpression<'ast> {
        pub condition: Expression<'ast>,
        pub consequence: Expression<'ast>,
        pub alternative: Expression<'ast>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }


    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::var_assignment_op))]
    pub enum VarAssignmentOp {
        Assign(AssignOp),
        AddAssign(AddAssignOp),
        SubAssign(SubAssignOp),
        MulAssign(MulAssignOp),
        DivAssign(DivAssignOp),
        ModAssign(ModAssignOp),
        PowAssign(PowAssignOp),
        BitAndAssign(BitAndAssignOp),
        BitOrAssign(BitOrAssignOp),
        BitXorAssign(BitXorAssignOp),
        LeftShiftAssign(LeftShiftAssignOp),
        RightShiftAssign(RightShiftAssignOp),
        BitNotAssign(BitNotAssignOp),
    }

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_assign))]
    pub struct AssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_add_assign))]
    pub struct AddAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_sub_assign))]
    pub struct SubAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_mul_assign))]
    pub struct MulAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_div_assign))]
    pub struct DivAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_mod_assign))]
    pub struct ModAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_pow_assign))]
    pub struct PowAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_bit_and_assign))]
    pub struct BitAndAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_bit_or_assign))]
    pub struct BitOrAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_bit_xor_assign))]
    pub struct BitXorAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_left_shift_assign))]
    pub struct LeftShiftAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_right_shift_assign))]
    pub struct RightShiftAssignOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_bit_not_assign))]
    pub struct BitNotAssignOp {}

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum OpBinary {
        OrOp,
        AndOp,
        BitXorOp,
        BitAndOp,
        BitOrOp,
        BitNotOp,
        EqualOp,
        NotEqualOp,
        LtOp,
        LteOp,
        GtOp,
        GteOp,
        AddOp,
        SubOp,
        PowOp,
        MulOp,
        DivOp,
        IDivOp,
        ModOp,
        LeftShiftOp,
        RightShiftOp,
    }

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_unary))]
    pub enum OpUnary {
        Neg(NegOp),
        Not(NotOp),
        Increment(IncrementOp),
        Decrement(DecrementOp),
    }

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_neg))]
    pub struct NegOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_not))]
    pub struct NotOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_increment))]
    pub struct IncrementOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_decrement))]
    pub struct DecrementOp {}

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::op_post_unary))]
    pub enum PostUnaryOp {
        Increment(IncrementOp),
        Decrement(DecrementOp),
    }
    
    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::number))]
    pub enum Number<'ast> {
        Decimal(DecimalNumber<'ast>),
        Hex(HexNumber<'ast>),
    }

    impl<'ast> Number<'ast> {
        pub fn span(&self) -> &Span<'ast> {
            match self {
                Number::Decimal(n) => &n.span,
                Number::Hex(n) => &n.span,
            }
        }
    }
    
    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::dec_number))]
    pub struct DecimalNumber<'ast> {
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Eq, Clone)]
    #[pest_ast(rule(Rule::hex_number))]
    pub struct HexNumber<'ast> {
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::assignee_tuple))]
    pub struct AssigneeTuple<'ast> {
        pub assignees: Vec<Assignee<'ast>>,
        #[pest_ast(outer())]
        pub span: Span<'ast>,
    }

    #[derive(Debug, FromPest, PartialEq, Clone)]
    #[pest_ast(rule(Rule::assignee_target))]
    pub enum AssigneeTarget<'ast> {
        Single(Assignee<'ast>),
        Tuple(AssigneeTuple<'ast>),
    }

    fn span_into_str(span: Span) -> String {
        span.as_str().to_string()
    }
}

struct Prog<'ast>(ast::File<'ast>);

impl<'ast> From<Pairs<'ast, Rule>> for Prog<'ast> {
    fn from(mut pairs: Pairs<'ast, Rule>) -> Self {
        Prog(ast::File::from_pest(&mut pairs).unwrap())
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Error(PestError<Rule>);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[allow(clippy::result_large_err)]
pub fn generate_ast(input: &str) -> Result<ast::File, Error> {
    let parse_tree = parse(input).map_err(Error)?;
    Ok(Prog::from(parse_tree).0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic_template() {
        let source = r#"
        template Multiplier(n) {
            signal input a;
            signal input b;
            signal output c;
            c <== a * b;
        }
"#;
        assert!(generate_ast(source).is_ok());
    }
}

#[test]
fn test_parse_function() {
    let source = r#"
    function factorial(n) {
        var res = 1000;
        var i = res;
        return i;
    }
    "#;
    assert!(generate_ast(source).is_ok());
}

#[test]
fn test_parse_function_negative_number() {
    let source = r#"
    function factorial(n) {
        var res = -1;
        var i = res;
        return i;
    }
    "#;
    assert!(generate_ast(source).is_ok());
}

#[test]
fn test_parse_function_with_for_loop() {
    let source = r#"
    function factorial(n) {
        var res = 1000;
        for (var i = 2; i <= n; i++) {
            res = res * i;
        }
        return res;
    }
    "#;
    assert!(generate_ast(source).is_ok());
}

#[test]
fn test_signal_assignment_constraint_statement_opposite() {
    let source = r#"
    template Main() {
        signal input in[256];
        signal output out[2];

        var i;

        var base[2] = [5, 10];

        component escalarMul = EscalarMul(256, base);

        escalarMul.inp[0] <== 0;
        escalarMul.inp[1] <== 1;

        escalarMul.out[0] ==> out[0];
        escalarMul.out[1] ==> out[1];
    }
    "#;
    assert!(generate_ast(source).is_ok());
}

#[test]
fn test_power() {
    let source = r#"
    template BinSub(n) {
        signal input in[2][n];
        signal output out[n];

        signal aux;

        var lin = 2**n;
    }
    "#;
    assert!(generate_ast(source).is_ok());
}

#[test]
fn test_if_else() {
    let source = r#"
    template CompConstant(ct) {
        signal input in[254];
        signal output out;

        signal parts[127];

        var clsb;
        var cmsb;

        for (i=0;i<127; i++) {
            if (cmsb==0) {
                parts[i] <== 1;
            } else if (clsb==1) {
                parts[i] <== 0;
            } 
        }

    }
    "#;
    assert!(generate_ast(source).is_ok());
}

#[test]
fn test_example_dirs() {
    use glob::glob;
    use std::fs;
    use std::io::Read;

    let patterns = [
        "../circomlib/test/circuits/*.circom",
        "../circomlib/circuits/*.circom",
        "../circomlib/circuits/smt/*.circom",
        "../circomlib/circuits/smt/sha256/*.circom",
    ];

    for pattern in patterns {
        for entry in glob(pattern).expect("Failed to read glob pattern") {
            match entry {
                Ok(path) => {
                    if path.to_str().unwrap().contains("error") {
                        continue;
                    }

                    println!("Parsing {:?}", path.display());
                    let mut file = fs::File::open(path.clone()).unwrap();

                    let mut data = String::new();
                    file.read_to_string(&mut data).unwrap();

                    match generate_ast(&data) {
                        Ok(_) => {
                            assert!(true);
                        }
                        Err(e) => {
                            println!("Error: {:?}", e);
                            assert!(false);
                        }
                    }
                }
                Err(e) => {
                    println!("Error: {:?}", e);
                    assert!(false);
                }
            }
        }
    }
}

#[test]
fn test_macro_dataset() {
    use glob::glob;
    use std::fs;
    use std::io::Read;

    // Pattern to recursively match all .circom files in the macro dataset
    let pattern = "../../../../circ_dataset/dataset/circom/macro/**/*.circom";
    
    let mut file_count = 0;
    
    for entry in glob(pattern).expect("Failed to read glob pattern") {
        match entry {
            Ok(path) => {
                file_count += 1;
                println!("Parsing file {}: {:?}", file_count, path.display());
                
                // Read the file content
                let mut file = fs::File::open(&path)
                    .unwrap_or_else(|e| panic!("Error opening file {:?}: {}", path, e));

                let mut data = String::new();
                file.read_to_string(&mut data)
                    .unwrap_or_else(|e| panic!("Error reading file {:?}: {}", path, e));

                // Try to generate AST - fail immediately if parsing fails
                match generate_ast(&data) {
                    Ok(_) => {
                        println!("Successfully parsed: {:?}", path.display());
                    }
                    Err(e) => {
                        panic!("Error parsing file {:?}: {:?}", path, e);
                    }
                }
            }
            Err(e) => {
                panic!("Error in glob pattern: {:?}", e);
            }
        }
    }
    
    // Make sure we found at least one file
    assert!(file_count > 0, "No files were found in the dataset");
    println!("Successfully parsed all {} files", file_count);
}
