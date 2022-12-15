#![feature(box_syntax)]

pub use proc_macros::gen_syntax_enum;
pub use infix_expr::*;

mod infix_expr;
