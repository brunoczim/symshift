use std::ops::{BitAnd, BitOr, BitXor, Not as OpNot, Range};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MatchKind {
    Pos,
    Neg,
}

impl Default for MatchKind {
    fn default() -> Self {
        Self::Pos
    }
}

impl OpNot for MatchKind {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Pos => Self::Neg,
            Self::Neg => Self::Pos,
        }
    }
}

impl BitAnd for MatchKind {
    type Output = Self;

    fn bitand(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Pos, Self::Pos) => Self::Pos,
            _ => Self::Neg,
        }
    }
}

impl BitOr for MatchKind {
    type Output = Self;

    fn bitor(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Neg, Self::Neg) => Self::Neg,
            _ => Self::Pos,
        }
    }
}

impl BitXor for MatchKind {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self::Output {
        if self == other {
            Self::Neg
        } else {
            Self::Pos
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Match {
    pub kind: MatchKind,
    pub start: usize,
    pub end: usize,
}

impl OpNot for Match {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self { kind: !self.kind, ..self }
    }
}

impl BitAnd for Match {
    type Output = Self;

    fn bitand(self, other: Self) -> Self::Output {
        let kind = self.kind & other.kind;

        let (start, end) = match (self.kind, other.kind) {
            (MatchKind::Pos, MatchKind::Pos) => {
                (self.start.max(other.start), self.end.min(other.end))
            },
            (MatchKind::Pos, MatchKind::Neg) => (other.start, other.end),
            (MatchKind::Neg, MatchKind::Pos) => (self.start, self.end),
            (MatchKind::Neg, MatchKind::Neg) => {
                (self.start.min(other.start), self.end.max(other.end))
            },
        };

        Self { kind, start, end }
    }
}

impl BitOr for Match {
    type Output = Self;

    fn bitor(self, other: Self) -> Self::Output {
        let kind = self.kind & other.kind;

        let (start, end) = match (self.kind, other.kind) {
            (MatchKind::Neg, MatchKind::Neg) => {
                (self.start.max(other.start), self.end.min(other.end))
            },
            (MatchKind::Neg, MatchKind::Pos) => (other.start, other.end),
            (MatchKind::Pos, MatchKind::Neg) => (self.start, self.end),
            (MatchKind::Pos, MatchKind::Pos) => {
                (self.start.min(other.start), self.end.max(other.end))
            },
        };

        Self { kind, start, end }
    }
}

impl Match {
    pub fn range(self) -> Range<usize> {
        self.start .. self.end
    }

    pub fn len(self) -> usize {
        self.end - self.start
    }
}

pub trait Pattern {
    type Symbol: Eq;

    fn test_match(&self, symbols: &[Self::Symbol]) -> Match;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct MatchChar<S>
where
    S: Eq,
{
    pub symbol: S,
}

impl<S> Pattern for MatchChar<S>
where
    S: Eq,
{
    type Symbol = S;

    fn test_match(&self, symbols: &[Self::Symbol]) -> Match {
        for (i, symbol) in symbols.iter().enumerate() {
            if symbol == &self.symbol {
                return Match { kind: MatchKind::Pos, start: i, end: 1 };
            }
        }

        Match { kind: MatchKind::Neg, start: 0, end: 1 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Not<P>
where
    P: Pattern,
{
    pub inner: P,
}

impl<P> Pattern for Not<P>
where
    P: Pattern,
{
    type Symbol = P::Symbol;

    fn test_match(&self, symbols: &[Self::Symbol]) -> Match {
        !self.inner.test_match(symbols)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct And<P1, P2>
where
    P1: Pattern,
    P2: Pattern<Symbol = P1::Symbol>,
{
    pub left: P1,
    pub right: P2,
}

impl<P1, P2> Pattern for And<P1, P2>
where
    P1: Pattern,
    P2: Pattern<Symbol = P1::Symbol>,
{
    type Symbol = P1::Symbol;

    fn test_match(&self, symbols: &[Self::Symbol]) -> Match {
        self.left.test_match(symbols) & self.left.test_match(symbols)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Or<P1, P2>
where
    P1: Pattern,
    P2: Pattern<Symbol = P1::Symbol>,
{
    pub left: P1,
    pub right: P2,
}

impl<P1, P2> Pattern for Or<P1, P2>
where
    P1: Pattern,
    P2: Pattern<Symbol = P1::Symbol>,
{
    type Symbol = P1::Symbol;

    fn test_match(&self, symbols: &[Self::Symbol]) -> Match {
        self.left.test_match(symbols) | self.left.test_match(symbols)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Seq<P1, P2>
where
    P1: Pattern,
    P2: Pattern<Symbol = P1::Symbol>,
{
    pub left: P1,
    pub right: P2,
}

impl<P1, P2> Pattern for Seq<P1, P2>
where
    P1: Pattern,
    P2: Pattern<Symbol = P1::Symbol>,
{
    type Symbol = P1::Symbol;

    fn test_match(&self, symbols: &[Self::Symbol]) -> Match {
        let left = self.left.test_match(symbols);
    }
}
