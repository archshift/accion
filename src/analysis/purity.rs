use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{self, AstNodeWrap};
use crate::analysis::{postorder, Visitor, scoping::Scopes};
use crate::types::Purity;

type NodePurities = HashMap<ast::AstNodeId, Purity>;

struct PurityAnalysis<'a> {
    scopes: &'a Scopes,
    node_purity: NodePurities,
}

impl<'a> PurityAnalysis<'a> {
    fn mark_node(&mut self, node: ast::AstNode, purity: Purity) {
        if let Some(old) = self.node_purity.insert(node.node_id(), purity) {
            // Only allow this when the purity would stay the same
            match (purity, old) {
                | (Purity::Pure, Purity::Pure)
                | (Purity::Impure, Purity::Impure)
                | (_, Purity::Any)
                => {}

                | _ => panic!("Tried to retroactively assign node purity! {:?} replacing {:?}", purity, old)
            }
        }
    }
    fn get(&self, node: ast::AstNode) -> Purity {
        *self.node_purity.get(&node.node_id())
            .expect("Tried to get purity for unknown node!")
    }
}

#[derive(Debug)]
pub struct Purities {
    node_purity: NodePurities,
}

impl Purities {
    pub fn node_purity(&self, node: ast::AstNode) -> Option<Purity> {
        self.node_purity.get(&node.node_id())
            .copied()
    }
}

pub fn analyze(root: &Rc<ast::Ast>, scopes: &Scopes) -> Purities {
    let mut ctx = PurityAnalysis {
        scopes,
        node_purity: NodePurities::new(),
    };
    
    postorder(root.as_any(), &mut |node| {
        ctx.visit_node(node);
    });

    for global_item in root.decls() {
        ctx.get(global_item.as_any())
            .mix(Purity::Pure);
    }

    Purities {
        node_purity: ctx.node_purity,
    }
}

impl Visitor for PurityAnalysis<'_> {
    fn visit_expr_entype(&mut self, node: &Rc<ast::ExprEntype>) {
        let ty = node.ty();
        self.get(ty.as_any())
            .mix(Purity::Pure);
        self.mark_node(node.as_any(), Purity::Any);
    }

    fn visit_expr_literal(&mut self, node: &Rc<ast::ExprLiteral>) {
        self.mark_node(node.as_any(), Purity::Any);
    }

    fn visit_expr_ident(&mut self, node: &Rc<ast::ExprIdent>) {
        let ident = node.ident();
        let name = ident.name();
        
        if let Some(_) = self.scopes.builtins.get(name) {
            self.mark_node(node.as_any(), Purity::Any);
            return;
        }
        
        let scope = self.scopes.node_scope(node.node_id()).unwrap();
        
        let decl = self.scopes.resolve(scope, name);
        if decl.is_none() {
            panic!("Error: referenced unknown ident `{}`", name);
        }
        let decl = decl.unwrap();
        
        // The only valid situations in which .get() can fail are where the ident is
        // referenced before its outer expression is visited. So we're either doing a:
        // - Recursive function reference
        // - Top-level item reference
        // - Function argument reference
        // In all of these cases, these idents are always pure.
        let purity = *self.node_purity.get(&decl.node.node_id())
            .unwrap_or(&Purity::Any);

        self.mark_node(node.as_any(), purity);
    }

    fn visit_expr_if(&mut self, node: &Rc<ast::ExprIf>) {
        let cond = node.cond();
        let then_expr = node.then_expr();
        let else_expr = node.else_expr();
        let purity = self.get(cond.as_any())
            .mix(self.get(then_expr.as_any()))
            .mix(self.get(else_expr.as_any()));
        self.mark_node(node.as_any(), purity);
    }

    fn visit_expr_if_case(&mut self, node: &Rc<ast::ExprIfCase>) {
        let cond = node.cond();
        let mut purity = self.get(cond.as_any());
        for case in node.cases() {
            match case {
                | ast::IfCase::OnVal(_, val)
                | ast::IfCase::Else(val) => {
                    purity = purity.mix(self.get(val.as_any()));
                }
            }
        }
        
        self.mark_node(node.as_any(), purity);
    }

    fn visit_expr_var_decl(&mut self, node: &Rc<ast::ExprVarDecl>) {
        let val = node.val();
        self.mark_node(node.as_any(), self.get(val.as_any()));
    }

    fn visit_expr_fn_decl(&mut self, node: &Rc<ast::ExprFnDecl>) {
        // Actual decl itself may be pure, as can be all the argument decls.
        // Return val purity depends on the `!` operator.

        let name = node.name();
        let val = node.val();

        for arg in node.args() {
            self.mark_node(arg.as_any(), Purity::Any);
        }
        self.get(val.as_any())
            .mix(if node.pure() { Purity::Pure } else { Purity::Impure });
        
        self.mark_node(node.as_any(), Purity::Any);
        if let Some(name) = name {
            self.mark_node(name.as_any(), Purity::Any);
        }
    }

    fn visit_expr_curry(&mut self, node: &Rc<ast::ExprCurry>) {
        unimplemented!()
    }

    fn visit_expr_unary(&mut self, node: &Rc<ast::ExprUnary>) {
        let inner = node.operand();
        self.mark_node(node.as_any(), self.get(inner.as_any()));
    }

    fn visit_expr_binary(&mut self, node: &Rc<ast::ExprBinary>) {
        let (left, right) = node.operands();
        let purity = self.get(left.as_any())
            .mix(self.get(right.as_any()));
        self.mark_node(node.as_any(), purity);
    
    }

    fn visit_expr_fn_call(&mut self, node: &Rc<ast::ExprFnCall>) {
        let callee = node.callee();

        let mut purity = if node.pure() { Purity::Any } else { Purity::Impure };
        purity = purity.mix(self.get(callee.as_any()));

        for arg in node.args() {
            purity = purity.mix(self.get(arg.as_any()));
        }
        self.mark_node(node.as_any(), purity);
    }

    fn visit_literal_int(&mut self, node: &Rc<ast::LiteralInt>) {
        self.mark_node(node.as_any(), Purity::Any);
    }

    fn visit_literal_str(&mut self, node: &Rc<ast::LiteralString>) {
        self.mark_node(node.as_any(), Purity::Any);
    }

    fn visit_literal_bool(&mut self, node: &Rc<ast::LiteralBool>) {
        self.mark_node(node.as_any(), Purity::Any);
    }

    fn visit_literal_nil(&mut self, node: &Rc<ast::LiteralNil>) {
        self.mark_node(node.as_any(), Purity::Any);
    }

    fn visit_ident(&mut self, _: &Rc<ast::Ident>) {}
}