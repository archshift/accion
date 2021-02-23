use std::{rc::Rc};

use crate::analysis::{postorder, Visitor};
use crate::analysis::typing::Types;
use crate::analysis::scoping::Scopes;
use crate::ast::{self, AstNodeWrap, BinaryOp};
use crate::id_map::{Id, IdMap};

use iced_x86::{Code as Opcode, Instruction, MemoryOperand, Register};

use super::preorder;

// For allocating registers, we want to keep track of the following information:
// - First, we want to know what registers *must* correspond to a value once execution
//   reaches a certain AstNode. This is for register positions mandated by the ABI.
// - Second, we want to assign free values in a somewhat-optimal way so as to minimize
//   moves and spills to memory.
// - Whatever values could not be assigned at a particular instance in execution, gets
//   spilled to the stack.

struct RegBank {
    available: Vec<Register>,
    live_until: Vec<Option<ast::AstNodeId>>,
    sequential: bool,
}

struct RegAlloc<'a> {
    func: Rc<ast::ExprFnDecl>,
    types: &'a Types,
    
    ret_regs: RegBank,
    arg_regs: RegBank,
    callee_saved: RegBank,
}
impl<'a> RegAlloc<'a> {
    fn new(func: Rc<ast::ExprFnDecl>, types: &'a Types) -> RegAlloc {
        Self {
            func,
            types,
            ret_regs: RegBank {
                available: vec![Register::RAX, Register::RDX],
                live_until: vec![None; 2],
                sequential: true,
            },
            arg_regs: RegBank {
                available: vec![
                    Register::RDI, Register::RSI, Register::RDX,
                    Register::RCX, Register::R8, Register::R9],
                live_until: vec![None; 6],
                sequential: true,
            },
            callee_saved: RegBank {
                available: vec![
                    Register::RBX, Register::RSP, Register::RBP,
                    Register::R12, Register::R13, Register::R14,
                    Register::R15
                ],
                live_until: vec![None; 7],
                sequential: false,
            },
        }
    }
}

pub struct LiveRanges {
    live_begin: IdMap<ast::AstNodeId>,
    live_end: Vec<(Id /* end node */, Id /* start node */)>,
}

pub fn live_ranges(scopes: &Scopes, func: Rc<ast::ExprFnDecl>) -> LiveRanges {
    let mut ranges = LiveRanges {
        live_begin: IdMap::new(),
        live_end: Vec::new(),
    };

    postorder(func.as_any(), &mut |node| {
        ranges.live_begin.add(node.node_id());
    });
    
    // Preorder to determine end points of values
    let mut children = Vec::new();
    preorder(func.as_any(), |node| {
        node.push_children(&mut children);
        
        for child in children.iter().rev() {
            let child_id = ranges.live_begin.get_id(&child.node_id()).unwrap();

            if let Some(decl) = scopes.declaration(child.clone()) {
                if let Some(last_use) = decl.uses.last() {
                    let last_use_id = ranges.live_begin.get_id(&last_use.node_id()).unwrap();
                    ranges.live_end.push((last_use_id, child_id));
                }
            } else {
                // Unnamed values will not have their live_end populated,
                // so populate them here.
                let parent_id = ranges.live_begin.get_id(&node.node_id()).unwrap();
                ranges.live_end.push((parent_id, child_id));
            }
        }

        children.clear();
    });

    ranges
}

pub fn print_live_ranges(func: Rc<ast::ExprFnDecl>, ranges: &LiveRanges) {
    let mut sorted_ends = ranges.live_end.clone();
    sorted_ends.sort_by_key(|&(end, start)| (start, end));

    use std::collections::HashMap;
    let mut nodes_by_id = HashMap::new();

    preorder(func.as_any(), |node| {
        nodes_by_id.insert(node.node_id(), node.as_any().clone());
    });

    for (end, start) in sorted_ends {
        let node = ranges.live_begin.get(start).unwrap();
        println!("{:<5} -> {:<5} :: {:?}", start.0, end.0, nodes_by_id.get(node).unwrap());
    }
}

enum Location {
    Reg(Register),
    Mem(MemoryOperand)
}

struct Backend {

}

impl Backend {
    fn location_of(&self, node: &ast::Expr) -> Location {
        unimplemented!()
    }
}

impl Visitor for Backend {
    type Ret = Location;

    fn visit_ast(&mut self, _ast: &Rc<ast::Ast>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_expr_unary(&mut self, node: &Rc<ast::ExprUnary>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_expr_binary(&mut self, node: &Rc<ast::ExprBinary>) -> Self::Ret {
        let (left, right) = node.operands();
        let loc1 = self.location_of(left);
        let loc2 = self.location_of(right);

        let inst = match (node.operator(), loc1, loc2) {
            // Add
            | (BinaryOp::Add, Location::Reg(op1), Location::Reg(op2))
                => Instruction::with_reg_reg(
                    Opcode::Add_rm64_r64, op1, op2
                ),
            | (BinaryOp::Add, Location::Mem(op2), Location::Reg(op1))
            | (BinaryOp::Add, Location::Reg(op1), Location::Mem(op2))
                => Instruction::with_reg_mem(
                    Opcode::Add_r64_rm64, op1, op2
                ),
            | (BinaryOp::Add, Location::Mem(op1), Location::Mem(op2)) => {
                unimplemented!()
            }

            // Sub
            | (BinaryOp::Sub, Location::Reg(op1), Location::Reg(op2))
                => Instruction::with_reg_reg(
                    Opcode::Sub_rm64_r64, op1, op2
                ),
            | (BinaryOp::Sub, Location::Mem(op2), Location::Reg(op1))
            | (BinaryOp::Sub, Location::Reg(op1), Location::Mem(op2))
                => Instruction::with_reg_mem(
                    Opcode::Sub_r64_rm64, op1, op2
                ),
            | (BinaryOp::Sub, Location::Mem(op1), Location::Mem(op2)) => {
                unimplemented!()
            }

            // Mul
            | (BinaryOp::Mul, Location::Reg(op1), Location::Reg(op2)) => {
                // Opcode::Mul_rm64
                unimplemented!()
            }
            | (BinaryOp::Mul, Location::Mem(op1), Location::Reg(op2))
            | (BinaryOp::Mul, Location::Reg(op2), Location::Mem(op1)) => {
                // Opcode::Mul_rm64
                unimplemented!()
            }
            | (BinaryOp::Mul, Location::Mem(op1), Location::Mem(op2)) => {
                unimplemented!()
            }

            _ => unimplemented!()
        };

        unimplemented!()
    }

    fn visit_expr_fn_call(&mut self, node: &Rc<ast::ExprFnCall>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_expr_fn_decl(&mut self, node: &Rc<ast::ExprFnDecl>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_expr_if(&mut self, node: &Rc<ast::ExprIf>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_expr_if_case(&mut self, node: &Rc<ast::ExprIfCase>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_expr_literal(&mut self, node: &Rc<ast::ExprLiteral>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_expr_var_decl(&mut self, node: &Rc<ast::ExprVarDecl>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_literal_bool(&mut self, node: &Rc<ast::LiteralBool>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_literal_int(&mut self, node: &Rc<ast::LiteralInt>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_literal_nil(&mut self, node: &Rc<ast::LiteralNil>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_literal_str(&mut self, node: &Rc<ast::LiteralString>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_expr_entype(&mut self, node: &Rc<ast::ExprEntype>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_expr_ident(&mut self, node: &Rc<ast::ExprIdent>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_ident(&mut self, node: &Rc<ast::Ident>) -> Self::Ret {
        unimplemented!()
    }

    fn visit_expr_curry(&mut self, node: &Rc<ast::ExprCurry>) -> Self::Ret {
        unimplemented!()
    }
}