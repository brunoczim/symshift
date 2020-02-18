use std::ops::{
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitXor,
    BitXorAssign,
    Not as OpNot,
    Range,
};

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

impl BitAndAssign for MatchKind {
    fn bitand_assign(&mut self, other: Self) {
        *self = *self & other;
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

impl BitOrAssign for MatchKind {
    fn bitor_assign(&mut self, other: Self) {
        *self = *self | other;
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

impl BitXorAssign for MatchKind {
    fn bitxor_assign(&mut self, other: Self) {
        *self = *self ^ other;
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

impl BitAndAssign for Match {
    fn bitand_assign(&mut self, other: Self) {
        *self = *self & other;
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

impl BitOrAssign for Match {
    fn bitor_assign(&mut self, other: Self) {
        *self = *self | other;
    }
}

impl Match {
    pub fn range(self) -> Range<usize> {
        self.start .. self.end
    }

    pub fn len(self) -> usize {
        self.end - self.start
    }

    pub fn seq<P>(self, pattern: P, symbols: &[P::Symbol]) -> Self
    where
        P: Pattern,
    {
        let other = pattern.test_match(symbols, self.end);

        Match {
            kind: if other.start != self.end {
                MatchKind::Neg
            } else {
                self.kind & other.kind
            },
            start: self.start,
            end: other.end,
        }
    }
}

pub trait Pattern {
    type Symbol: Eq;

    fn test_match(&self, symbols: &[Self::Symbol], cursor: usize) -> Match;
}

impl<'pat, P> Pattern for &'pat P
where
    P: Pattern,
{
    type Symbol = P::Symbol;

    fn test_match(&self, symbols: &[Self::Symbol], cursor: usize) -> Match {
        (**self).test_match(symbols, cursor)
    }
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

    fn test_match(&self, symbols: &[Self::Symbol], cursor: usize) -> Match {
        for (i, symbol) in symbols[cursor ..].iter().enumerate() {
            if symbol == &self.symbol {
                return Match {
                    kind: MatchKind::Pos,
                    start: cursor + i,
                    end: cursor + 1 + i,
                };
            }
        }

        Match { kind: MatchKind::Neg, start: cursor, end: cursor + 1 }
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

    fn test_match(&self, symbols: &[Self::Symbol], cursor: usize) -> Match {
        !self.inner.test_match(symbols, cursor)
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

    fn test_match(&self, symbols: &[Self::Symbol], cursor: usize) -> Match {
        self.left.test_match(symbols, cursor)
            & self.left.test_match(symbols, cursor)
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

    fn test_match(&self, symbols: &[Self::Symbol], cursor: usize) -> Match {
        self.left.test_match(symbols, cursor)
            | self.left.test_match(symbols, cursor)
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

    fn test_match(&self, symbols: &[Self::Symbol], cursor: usize) -> Match {
        self.left.test_match(symbols, cursor).seq(&self.right, symbols)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Optional<P>
where
    P: Pattern,
{
    pub inner: P,
}

impl<P> Pattern for Optional<P>
where
    P: Pattern,
{
    type Symbol = P::Symbol;

    fn test_match(&self, symbols: &[Self::Symbol], cursor: usize) -> Match {
        let mtch = self.inner.test_match(symbols, cursor);
        if mtch.kind == MatchKind::Neg {
            Match { kind: MatchKind::Pos, start: cursor, end: cursor }
        } else {
            mtch
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Repeat<P>
where
    P: Pattern,
{
    pub inner: P,
    pub min: Option<usize>,
    pub max: Option<usize>,
}

impl<P> Pattern for Repeat<P>
where
    P: Pattern,
{
    type Symbol = P::Symbol;

    fn test_match(&self, symbols: &[Self::Symbol], cursor: usize) -> Match {
        if self.max == Some(0) {
            return Match { kind: MatchKind::Pos, start: cursor, end: cursor };
        }
        let mut mtch = self.inner.test_match(symbols, cursor);
        let mut count = 1;

        while self.max.filter(|&max| max <= count).is_none() {
            let prev = mtch.end;
            mtch = mtch.seq(&self.inner, symbols);
            if mtch.end == prev {
                break;
            }
            count += 1;
        }

        if self.min.filter(|&min| min > count).is_some() {
            mtch.kind = MatchKind::Neg;
        }
        mtch
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LazyRepeat<P>
where
    P: Pattern,
{
    pub left: P,
    pub right: P,
    pub min: Option<usize>,
    pub max: Option<usize>,
}

impl<P> Pattern for LazyRepeat<P>
where
    P: Pattern,
{
    type Symbol = P::Symbol;

    fn test_match(&self, symbols: &[Self::Symbol], cursor: usize) -> Match {
        if self.max == Some(0) {
            return self.right.test_match(symbols, cursor);
        }
        let mut mtch = self.left.test_match(symbols, cursor);
        let mut count = 1;

        while self.max.filter(|&max| max <= count).is_none()
            && self.min.filter(|&min| min > count).is_none()
        {
            let new_match = mtch.seq(&self.left, symbols);
            if mtch.end == new_match.end {
                break;
            }
            mtch = new_match;
            count += 1;
        }

        if self.min.filter(|&min| min > count).is_some() {
            mtch.kind = MatchKind::Neg;
        } else if mtch.kind != MatchKind::Neg {
            loop {
                let new_match = mtch.seq(&self.right, symbols);
                if self.max.filter(|&max| max <= count).is_some()
                    || new_match.kind == MatchKind::Pos
                {
                    mtch = new_match;
                    break;
                }

                let new_match = mtch.seq(&self.left, symbols);
                if mtch.end == new_match.end {
                    mtch.kind = MatchKind::Neg;
                    break;
                }
                mtch = new_match;
                count += 1;
            }
        }

        mtch
    }
}
