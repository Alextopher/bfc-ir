//! Human-readable warnings and errors for the CLI.

use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub struct Warning {
    pub message: String,
    pub position: Option<Position>,
}

/// An inclusive range used for tracking positions in source code.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Position {
    pub start: usize,
    pub end: usize,
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.start == self.end {
            write!(f, "{}", self.start)
        } else {
            write!(f, "{}-{}", self.start, self.end)
        }
    }
}

pub trait Combine<T> {
    fn combine(&self, _: T) -> T;
}

impl Combine<Option<Position>> for Option<Position> {
    fn combine(&self, other: Self) -> Self {
        match (*self, other) {
            (Some(pos1), Some(pos2)) => {
                let (first_pos, second_pos) = if pos1.start <= pos2.start {
                    (pos1, pos2)
                } else {
                    (pos2, pos1)
                };

                // If they're adjacent positions, we can merge them.
                if first_pos.end + 1 >= second_pos.start {
                    Some(Position {
                        start: first_pos.start,
                        end: second_pos.end,
                    })
                } else {
                    // Otherwise, just use the second position.
                    Some(pos2)
                }
            }
            _ => None,
        }
    }
}
