use crate::target::datan::ExprType;
/// .decl Expr(id: symbol)
#[derive(Clone, Debug)]
pub struct Expr {
    id: String,
}

/// .decl Identifier(x: symbol)
#[derive(Clone, Debug)]
pub struct Identifier {
    x: String,
}

/// .decl BinaryExpr(id: symbol, lexpr: symbol, rexpr: symbol, op: symbol, expr_type: symbol)
#[derive(Clone, Debug)]
pub struct BinaryExpr {
    id: String,
    lexpr: String,
    rexpr: String,
    op: String,
    expr_type: ExprType,
}

/// .decl Assign(x: symbol, y: symbol)
#[derive(Clone, Debug)]
pub struct Assign {
    x: String,
    y: String,
}

/// .decl PrivateVariable(x: symbol)
#[derive(Clone, Debug)]
pub struct PrivateVariable {
    x: String,
}

/// .decl PublicVariable(x: symbol)
#[derive(Clone, Debug)]
pub struct PublicVariable {
    x: String,
}

/// .decl Assert(x: symbol)
#[derive(Clone, Debug)]
pub struct Assert {
    x: String,
}

/// .decl Not(x: symbol)
#[derive(Clone, Debug)]
pub struct Not {
    id: String,
    x: String,
    expr_type: ExprType,
}

/// .decl Transform(id: symbol, expr: symbol, op: symbol)
#[derive(Clone, Debug)]
pub struct Transform {
    id: String,
    expr: String,
    op: String,
    expr_type: ExprType,
}

/// .decl Ite(id: symbol, condition: symbol, thenExpr: symbol, elseExpr: symbol)
#[derive(Clone, Debug)]
pub struct Ite {
    id: String,
    condition: String,
    then_expr: String,
    else_expr: String,
    expr_type: ExprType,
}

/// .decl BoolMaj(id: symbol, expr1: symbol, expr2: symbol, expr3: symbol)
#[derive(Clone, Debug)]
pub struct BoolMaj {
    id: String,
    expr1: String,
    expr2: String,
    expr3: String,
    expr_type: ExprType,
}

/// .decl ConcatX(id: symbol, expr1: symbol, expr2: symbol, ...)
#[derive(Clone, Debug)]
pub struct ConcatX {
    id: String,
    exprs: Vec<String>,
    expr_type: ExprType,
}

impl Expr {
    pub fn new(id: String) -> Self {
        Expr { id }
    }

    #[allow(dead_code)]
    pub fn to_string(&self) -> String {
        format!("Expr(\"{}\").", self.id)
    }
}

impl Identifier {
    pub fn new(x: String) -> Self {
        Identifier { x }
    }

    pub fn to_string(&self) -> String {
        format!("Identifier(\"{}\").", self.x)
    }
}

impl BinaryExpr {
    pub fn new(id: String, lexpr: String, rexpr: String, op: String, expr_type: ExprType) -> Self {
        BinaryExpr { id, lexpr, rexpr, op, expr_type }
    }

    pub fn to_string(&self) -> String {
        if self.expr_type == ExprType::Precompile {
            format!("CompBinaryExpr(\"{}\", \"{}\", \"{}\", \"{}\").", self.id, self.lexpr, self.rexpr, self.op)
        } else {
            format!("BinaryExpr(\"{}\", \"{}\", \"{}\", \"{}\").", self.id, self.lexpr, self.rexpr, self.op)
        }
    }
}

impl Assign {
    pub fn new(x: String, y: String) -> Self {
        Assign { x, y }
    }

    pub fn to_string(&self) -> String {
        format!("Assignment(\"{}\", \"{}\").", self.x, self.y)
    }
}

impl PrivateVariable {
    pub fn new(x: String) -> Self {
        PrivateVariable { x }
    }

    pub fn to_string(&self) -> String {
        format!("PrivateVariable(\"{}\").", self.x)
    }
}

impl PublicVariable {
    pub fn new(x: String) -> Self {
        PublicVariable { x }
    }

    pub fn to_string(&self) -> String {
        format!("PublicVariable(\"{}\").", self.x)
    }
}

impl Assert {
    pub fn new(x: String) -> Self {
        Assert { x }
    }

    pub fn to_string(&self) -> String {
        format!("Assert(\"{}\").", self.x)
    }
}

impl Not {
    pub fn new(id: String, x: String, expr_type: ExprType) -> Self {
        Not { id, x, expr_type }
    }

    pub fn to_string(&self) -> String {
        if self.expr_type == ExprType::Precompile {
            format!("CompNot(\"{}\", \"{}\").", self.id, self.x)
        } else {
            format!("Not(\"{}\", \"{}\").", self.id, self.x)
        }
    }
}

impl Transform {
    pub fn new(id: String, expr: String, op: String, expr_type: ExprType) -> Self {
        Transform { id, expr, op, expr_type }
    }

    pub fn to_string(&self) -> String {
        if self.expr_type == ExprType::Precompile {
            format!("CompTransform(\"{}\", \"{}\", \"{}\").", self.id, self.expr, self.op)
        } else {
            format!("Transform(\"{}\", \"{}\", \"{}\").", self.id, self.expr, self.op)
        }
    }
}

impl Ite {
    pub fn new(id: String, condition: String, then_expr: String, else_expr: String, expr_type: ExprType) -> Self {
        Ite { id, condition, then_expr, else_expr, expr_type }
    }

    pub fn to_string(&self) -> String {
        if self.expr_type == ExprType::Precompile {
            format!("CompIte(\"{}\", \"{}\", \"{}\", \"{}\").", self.id, self.condition, self.then_expr, self.else_expr)
        } else {
            format!("Ite(\"{}\", \"{}\", \"{}\", \"{}\").", self.id, self.condition, self.then_expr, self.else_expr)
        }
    }
}

impl ConcatX {
    pub fn new(id: String, exprs: Vec<String>, expr_type: ExprType) -> Self {
        ConcatX { id, exprs, expr_type }
    }

    pub fn to_string(&self) -> String {
        if self.expr_type == ExprType::Precompile {
            format!("CompConcat{}(\"{}\", [{}])", self.exprs.len(), self.id, self.exprs.join(", "))
        } else {
            format!("Concat{}(\"{}\", [{}])", self.exprs.len(), self.id, self.exprs.join(", "))
        }
    }
}

impl BoolMaj {
    pub fn new(id: String, expr1: String, expr2: String, expr3: String, expr_type: ExprType) -> Self {
        BoolMaj { id, expr1, expr2, expr3, expr_type }
    }

    pub fn to_string(&self) -> String {
        if self.expr_type == ExprType::Precompile {
            format!("CompBoolMaj(\"{}\", \"{}\", \"{}\", \"{}\").", self.id, self.expr1, self.expr2, self.expr3)
        } else {
            format!("BoolMaj(\"{}\", \"{}\", \"{}\", \"{}\").", self.id, self.expr1, self.expr2, self.expr3)
        }
    }
}