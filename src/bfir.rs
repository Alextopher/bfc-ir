//! bfir defines an AST for BF. This datastructure represents the
//! original BF source code with position data so we can find the
//! source lines from a portion of AST.
//!
//! It also provides functions for generating ASTs from source code,
//! producing good error messages on malformed inputs.

use self::AstNode::*;
use crate::diagnostics::Position;
use crate::peephole::remove_redundant_sets;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::num::Wrapping;

#[cfg(test)]
use crate::diagnostics::Combine;
#[cfg(test)]
use pretty_assertions::assert_eq;

/// A cell is the fundamental BF datatype that we work with. BF
/// requires this to be at least one byte, we provide a cell of
/// exactly one byte.
pub type Cell = Wrapping<i8>;

/// `AstNode` represents a node in our BF AST.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AstNode {
    Increment {
        amount: Cell,
        offset: isize,
        position: Option<Position>,
    },
    PointerIncrement {
        amount: isize,
        position: Option<Position>,
    },
    Read {
        position: Option<Position>,
    },
    Write {
        position: Option<Position>,
    },
    Loop {
        body: Vec<AstNode>,
        position: Option<Position>,
    },
    /// These instruction have no direct equivalent in BF, but we
    /// generate them during optimisation.
    Set {
        amount: Cell,
        offset: isize,
        position: Option<Position>,
    },
    MultiplyMove {
        changes: HashMap<isize, Cell>,
        position: Option<Position>,
    },
}

fn fmt_with_indent(instr: &AstNode, indent: i32, f: &mut fmt::Formatter) {
    for _ in 0..indent {
        let _ = write!(f, "  ");
    }

    match instr {
        &Loop {
            body: ref loop_body,
            position,
            ..
        } => {
            let _ = write!(f, "Loop position: {:?}", position);

            for loop_instr in loop_body {
                let _ = writeln!(f);
                fmt_with_indent(loop_instr, indent + 1, f);
            }
        }
        instr => {
            let _ = write!(f, "{:?}", instr);
        }
    }
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_with_indent(self, 0, f);
        Ok(())
    }
}

impl AstNode {
    pub fn eq_ignore_position(&self, other: &AstNode) -> bool {
        match (self, other) {
            (
                AstNode::Increment {
                    amount: a1,
                    offset: o1,
                    ..
                },
                AstNode::Increment {
                    amount: a2,
                    offset: o2,
                    ..
                },
            ) => a1 == a2 && o1 == o2,
            (
                AstNode::PointerIncrement { amount: a1, .. },
                AstNode::PointerIncrement { amount: a2, .. },
            ) => a1 == a2,
            (AstNode::Read { .. }, AstNode::Read { .. }) => true,
            (AstNode::Write { .. }, AstNode::Write { .. }) => true,
            (AstNode::Loop { body: b1, .. }, AstNode::Loop { body: b2, .. }) => {
                b1.len() == b2.len() && b1.iter().zip(b2).all(|(a, b)| a.eq_ignore_position(b))
            }
            (
                AstNode::Set {
                    amount: a1,
                    offset: o1,
                    ..
                },
                AstNode::Set {
                    amount: a2,
                    offset: o2,
                    ..
                },
            ) => a1 == a2 && o1 == o2,
            (
                AstNode::MultiplyMove { changes: c1, .. },
                AstNode::MultiplyMove { changes: c2, .. },
            ) => c1 == c2,
            _ => false,
        }
    }
}

pub fn get_position(instr: &AstNode) -> Option<Position> {
    match *instr {
        Increment { position, .. } => position,
        PointerIncrement { position, .. } => position,
        Read { position } => position,
        Write { position } => position,
        Loop { position, .. } => position,
        Set { position, .. } => position,
        MultiplyMove { position, .. } => position,
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub position: Position,
}

/// Given a string of BF source code, parse and return our BF IR
/// representation. If parsing fails, return an Info describing what
/// went wrong.
pub fn parse(source: &str) -> Result<Vec<AstNode>, ParseError> {
    // AstNodes in the current loop (or toplevel).
    let mut instructions = vec![];
    // Contains the instructions of open parent loops (or toplevel),
    // and the starting indices of the loops.
    let mut stack = vec![];

    for (index, c) in source.chars().enumerate() {
        match c {
            '+' => instructions.push(Increment {
                amount: Wrapping(1),
                offset: 0,
                position: Some(Position {
                    start: index,
                    end: index,
                }),
            }),
            '-' => instructions.push(Increment {
                amount: Wrapping(-1),
                offset: 0,
                position: Some(Position {
                    start: index,
                    end: index,
                }),
            }),
            '>' => instructions.push(PointerIncrement {
                amount: 1,
                position: Some(Position {
                    start: index,
                    end: index,
                }),
            }),
            '<' => instructions.push(PointerIncrement {
                amount: -1,
                position: Some(Position {
                    start: index,
                    end: index,
                }),
            }),
            ',' => instructions.push(Read {
                position: Some(Position {
                    start: index,
                    end: index,
                }),
            }),
            '.' => instructions.push(Write {
                position: Some(Position {
                    start: index,
                    end: index,
                }),
            }),
            '[' => {
                stack.push((instructions, index));
                instructions = vec![];
            }
            ']' => {
                if let Some((mut parent_instr, open_index)) = stack.pop() {
                    parent_instr.push(Loop {
                        body: instructions,
                        position: Some(Position {
                            start: open_index,
                            end: index,
                        }),
                    });
                    instructions = parent_instr;
                } else {
                    return Err(ParseError {
                        message: "This ] has no matching [".to_owned(),
                        position: Position {
                            start: index,
                            end: index,
                        },
                    });
                }
            }
            _ => (),
        }
    }

    if !stack.is_empty() {
        let pos = stack.last().unwrap().1;
        return Err(ParseError {
            message: "This [ has no matching ]".to_owned(),
            position: Position {
                start: pos,
                end: pos,
            },
        });
    }

    Ok(instructions)
}

/// Given a BF IR decompile it back into BF source code.
pub fn decompile(instrs: &[AstNode]) -> String {
    // We want to first translate `Set X` instructions to be `Set 0` `Increment X`.
    // then we can use the "remove_redundant_sets" optimization to remove unnecessary sets.
    let instrs = expand_sets(instrs);
    let instrs = remove_redundant_sets(instrs);

    decompile_inner(&instrs)
}

// Convert SET X to SET 0, INCREMENT X
pub(crate) fn expand_sets(instrs: &[AstNode]) -> Vec<AstNode> {
    let mut result = vec![];
    for instr in instrs {
        match *instr {
            Set {
                amount,
                offset,
                position,
            } => {
                result.push(Set {
                    amount: Wrapping(0),
                    offset,
                    position,
                });
                result.push(Increment {
                    amount,
                    offset,
                    position,
                });
            }
            Loop { ref body, position } => {
                result.push(Loop {
                    body: expand_sets(body),
                    position,
                });
            }
            ref other => result.push(other.clone()),
        }
    }
    result
}

fn decompile_inner(instructions: &[AstNode]) -> String {
    let mut result = String::new();

    let mut last_offset = 0;
    for ir in instructions {
        match ir {
            Increment { amount, offset, .. } => {
                let diff = offset - last_offset;
                last_offset = *offset;

                let moves = match diff.cmp(&0) {
                    Ordering::Less => "<".repeat((-diff) as usize),
                    Ordering::Equal => "".to_owned(),
                    Ordering::Greater => ">".repeat(diff as usize),
                };
                let incs = match amount.0.cmp(&0) {
                    Ordering::Less => "-".repeat((-amount.0) as usize),
                    Ordering::Equal => "".to_owned(),
                    Ordering::Greater => "+".repeat(amount.0 as usize),
                };

                result.push_str(&format!("{}{}", moves, incs));
            }
            PointerIncrement { amount, .. } => {
                let diff = amount - last_offset;
                last_offset = 0;

                let moves = match diff.cmp(&0) {
                    Ordering::Less => "<".repeat((-amount) as usize),
                    Ordering::Equal => "".to_owned(),
                    Ordering::Greater => ">".repeat(*amount as usize),
                };

                result.push_str(&moves);
            }
            Read { .. } => {
                result.push(',');
            }
            Write { .. } => {
                result.push('.');
            }
            Loop { body, .. } => {
                result.push('[');
                result.push_str(&decompile(body));
                result.push(']');
            }
            Set { amount, offset, .. } => {
                let diff = offset - last_offset;
                last_offset = *offset;

                let moves = match diff.cmp(&0) {
                    Ordering::Less => "<".repeat((-diff) as usize),
                    Ordering::Equal => "".to_owned(),
                    Ordering::Greater => ">".repeat(diff as usize),
                };
                let incs = match amount.0.cmp(&0) {
                    Ordering::Less => "-".repeat((-amount.0) as usize),
                    Ordering::Equal => "".to_owned(),
                    Ordering::Greater => "+".repeat(amount.0 as usize),
                };

                result.push_str(&format!("{}[-]{}", moves, incs));
            }
            MultiplyMove { changes, .. } => {
                // Get changes sorted by offset
                let mut changes = changes.iter().collect::<Vec<_>>();
                changes.sort_by_key(|(offset, _)| *offset);

                // [-
                result.push_str("[-");
                let mut last_offset = 0;
                for (offset, amount) in changes {
                    let diff = offset - last_offset;
                    last_offset = *offset;

                    let moves = match diff.cmp(&0) {
                        Ordering::Less => "<".repeat((-diff) as usize),
                        Ordering::Equal => "".to_owned(),
                        Ordering::Greater => ">".repeat(diff as usize),
                    };
                    let incs = match amount.0.cmp(&0) {
                        Ordering::Less => "-".repeat((-amount.0) as usize),
                        Ordering::Equal => "".to_owned(),
                        Ordering::Greater => "+".repeat(amount.0 as usize),
                    };

                    result.push_str(&format!("{}{}", moves, incs));
                }

                let moves = match last_offset.cmp(&0) {
                    Ordering::Less => ">".repeat((-last_offset) as usize),
                    Ordering::Equal => "".to_owned(),
                    Ordering::Greater => "<".repeat(last_offset as usize),
                };

                result.push_str(&format!("{}]", moves));
            }
        }
    }

    result
}

#[test]
fn parse_increment() {
    assert_eq!(
        parse("+").unwrap(),
        [Increment {
            amount: Wrapping(1),
            offset: 0,
            position: Some(Position { start: 0, end: 0 }),
        }]
    );
    assert_eq!(
        parse("++").unwrap(),
        [
            Increment {
                amount: Wrapping(1),
                offset: 0,
                position: Some(Position { start: 0, end: 0 }),
            },
            Increment {
                amount: Wrapping(1),
                offset: 0,
                position: Some(Position { start: 1, end: 1 }),
            }
        ]
    );
}

#[test]
fn parse_decrement() {
    assert_eq!(
        parse("-").unwrap(),
        [Increment {
            amount: Wrapping(-1),
            offset: 0,
            position: Some(Position { start: 0, end: 0 }),
        }]
    );
}

#[test]
fn parse_pointer_increment() {
    assert_eq!(
        parse(">").unwrap(),
        [PointerIncrement {
            amount: 1,
            position: Some(Position { start: 0, end: 0 }),
        }]
    );
}

#[test]
fn parse_pointer_decrement() {
    assert_eq!(
        parse("<").unwrap(),
        [PointerIncrement {
            amount: -1,
            position: Some(Position { start: 0, end: 0 }),
        }]
    );
}

#[test]
fn parse_read() {
    assert_eq!(
        parse(",").unwrap(),
        [Read {
            position: Some(Position { start: 0, end: 0 })
        }]
    );
}

#[test]
fn parse_write() {
    assert_eq!(
        parse(".").unwrap(),
        [Write {
            position: Some(Position { start: 0, end: 0 })
        }]
    );
}

#[test]
fn parse_empty_loop() {
    let expected = [Loop {
        body: vec![],
        position: Some(Position { start: 0, end: 1 }),
    }];
    assert_eq!(parse("[]").unwrap(), expected);
}

#[test]
fn parse_simple_loop() {
    let loop_body = vec![Increment {
        amount: Wrapping(1),
        offset: 0,
        position: Some(Position { start: 1, end: 1 }),
    }];
    let expected = [Loop {
        body: loop_body,
        position: Some(Position { start: 0, end: 2 }),
    }];
    assert_eq!(parse("[+]").unwrap(), expected);
}

#[test]
fn parse_complex_loop() {
    let loop_body = vec![
        Read {
            position: Some(Position { start: 2, end: 2 }),
        },
        Increment {
            amount: Wrapping(1),
            offset: 0,
            position: Some(Position { start: 3, end: 3 }),
        },
    ];
    let expected = [
        Write {
            position: Some(Position { start: 0, end: 0 }),
        },
        Loop {
            body: loop_body,
            position: Some(Position { start: 1, end: 4 }),
        },
        Increment {
            amount: Wrapping(-1),
            offset: 0,
            position: Some(Position { start: 5, end: 5 }),
        },
    ];
    assert_eq!(parse(".[,+]-").unwrap(), expected);
}

#[test]
fn parse_unbalanced_loop() {
    assert!(parse("[").is_err());
    assert!(parse("]").is_err());
    assert!(parse("][").is_err());
    assert!(parse("[][").is_err());
}

#[test]
fn parse_comment() {
    assert_eq!(parse("foo! ").unwrap(), []);
}

#[test]
fn test_combine_pos() {
    let pos1 = Some(Position { start: 1, end: 2 });
    let pos2 = Some(Position { start: 3, end: 4 });

    assert_eq!(pos1.combine(pos2), Some(Position { start: 1, end: 4 }));
}

#[test]
fn test_combine_order() {
    let pos1 = Some(Position { start: 3, end: 4 });
    let pos2 = Some(Position { start: 1, end: 2 });

    assert_eq!(pos1.combine(pos2), Some(Position { start: 1, end: 4 }));
}

#[test]
fn test_combine_pos_not_consecutive() {
    let pos1 = Some(Position { start: 1, end: 2 });
    let pos2 = Some(Position { start: 4, end: 5 });

    assert_eq!(pos1.combine(pos2), Some(Position { start: 4, end: 5 }));
}

#[test]
fn test_combine_pos_overlap() {
    let pos1 = Some(Position { start: 1, end: 1 });
    let pos2 = Some(Position { start: 1, end: 3 });

    assert_eq!(pos1.combine(pos2), Some(Position { start: 1, end: 3 }));
}
