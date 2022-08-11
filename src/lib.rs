#![warn(trivial_numeric_casts)]

//! bfc-ir is an intermediate representation, optimizer, and speculative executor for BF programs.
//! This project is forked from [bfc](https://github.com/Wilfred/bfc)

pub use bfir::{decompile, parse, AstNode, ParseError};
pub use diagnostics::Warning;
pub use execution::{execute, ExecutionState};
pub use peephole::{optimize, OptimisationsFlags};

mod bfir;
mod bounds;
mod diagnostics;
mod execution;
mod peephole;

#[cfg(test)]
mod peephole_tests;
#[cfg(test)]
mod soundness_tests;
